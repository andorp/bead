{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Submission.Page (
    submission
  , resolveStatus
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as B
import           Data.List (intersperse, partition)
import           Data.Maybe (listToMaybe)
import           Data.String (fromString)
import           Data.Time

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)
import           System.FilePath.Posix (takeExtension)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Bead.Config (maxUploadSizeInKb)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Submission.Common
import           Bead.View.Markdown (markdownToHtml)

submission = ViewModifyHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  , asTimeConv :: UserTimeConverter
  , asNow :: UTCTime
  , asMaxFileSize :: Int
  , asLimit :: SubmissionLimit
  }

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile
  deriving (Eq,Show)

submissionPage :: GETContentHandler
submissionPage = do
  ak <- getParameter assignmentKeyPrm
  ut <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  size <- fmap maxUploadSizeInKb $ lift getConfiguration
  -- TODO: Refactor use guards
  let renderPage limit (desc,asg) =
        return $ submissionContent
          (PageData { asKey = ak, asValue = asg, asDesc = desc, asTimeConv = ut, asNow = now, asMaxFileSize = size, asLimit = limit })

  limit <- userStory $ do
    asg <- Story.userAssignmentForSubmission ak -- GUARD
    lmt <- Story.assignmentSubmissionLimit ak
    return $! fmap (const asg) lmt

  let limit' = fmap (const ()) limit
  submissionLimit
    (renderPage limit')
    (const (renderPage limit'))
    (const $ return (fromString "Limit is reached"))
    limit


submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  uploadResult <- join $ lift $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy $ \parts -> do
      results <- mapM handlePart parts
      return . return $ results
  ak <- getParameter assignmentKeyPrm
  (_desc,asg) <- userStory $ Story.userAssignmentForSubmission ak
  -- Assignment is for the user
  let aspects = Assignment.aspects asg
  if Assignment.isPasswordProtected aspects
    -- Password-protected assignment
    then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Submission password")
            if Assignment.getPassword aspects == pwd
              -- Passwords do match
              then newSubmission ak aspects uploadResult
              -- Passwords do not match
              else return . ErrorMessage $ msg_Submission_InvalidPassword "Invalid password, the solution could not be submitted!"
    -- Non password protected assignment
    else newSubmission ak aspects uploadResult
  where
    newSubmission ak as up =
      if (not $ Assignment.isZippedSubmissions as)
        then submit $ SimpleSubmission <$> getParameter (stringParameter (fieldName submissionTextField) "Submission text")
        else
          case uploadedFile of
            Just (File name contents) ->
              if (takeExtension name == ".zip")
                then submit $ return $ ZippedSubmission contents
                else return $
                  ErrorMessage $ msg_Submission_File_InvalidFile
                    "The extension of the file to be uploaded is incorrect."
            Just PolicyFailure      -> return $
              ErrorMessage $ msg_Submission_File_PolicyFailure
                "The upload policy has been violated, probably the file was too large."
            Nothing                 -> return $
              ErrorMessage $ msg_Submission_File_NoFileReceived
                "No file has been received."
            _                       -> return $
              ErrorMessage $ msg_Submission_File_InternalError
                "Some error happened during upload."
       where
         submit s = NewSubmission ak <$> (E.Submission <$> s <*> liftIO getCurrentTime)
         uploadedFile = listToMaybe $ uncurry (++) $ partition isFile up

         isFile (File _ _) = True
         isFile _          = False

    handlePart (_partInfo, Left _exception) = return PolicyFailure
    handlePart (partInfo, Right filePath) =
      case (partFileName partInfo) of
        Just fp | not (B.null fp) -> do
          contents <- liftIO $ do
            exists <- doesFileExist filePath
            if exists
              then do
                body <- B.readFile filePath
                return $ Just body
              else return $ Nothing
          return $ case contents of
            Just body -> File (unpack fp) body
            _         -> InvalidFile
        _                         -> return UnnamedFile



submissionContent :: PageData -> IHtml
submissionContent p = do
  msg <- getI18N
  return $ do
    -- Informational table on the page
    Bootstrap.rowColMd12 $ Bootstrap.table $
      H.tbody $ do
        (msg $ msg_Submission_Course "Course: ")         .|. (fromString . aGroup $ asDesc p)
        (msg $ msg_Submission_Admin "Teacher: ")         .|. (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
        (msg $ msg_Submission_Assignment "Assignment: ") .|. (fromString . Assignment.name $ asValue p)
        (msg $ msg_Submission_Deadline "Deadline: ")     .|.
          (fromString . showDate . (asTimeConv p) . Assignment.end $ asValue p)
        (msg $ msg_Submission_TimeLeft "Time left: ")    .|. (startEndCountdownDiv
                "ctd"
                (msg $ msg_Submission_Days "day(s)")
                (msg $ msg_Submission_DeadlineReached "Deadline is reached")
                (asNow p)
                (Assignment.end $ asValue p))
        maybe (return ()) (uncurry (.|.)) (remainingTries msg (asLimit p))
    Bootstrap.rowColMd12 $ do
      H.h2 $ fromString $ msg $ msg_Submission_Description "Description"
      H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc $ asValue p
    postForm (routeOf submission) `withId` (rFormId submissionForm) ! A.enctype "multipart/form-data" $ do
      hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
      assignmentPassword msg
      Bootstrap.rowColMd12 $ h2 $
        fromString $ msg $ msg_Submission_Solution "Submission"
      if (Assignment.isZippedSubmissions aspects)
        then
          Bootstrap.formGroup $ do
            Bootstrap.helpBlock $
              (msg $ msg_Submission_Info_File
                "Please select a file with .zip extension to submit.  Note that the maximum file size in kilobytes: ") ++
              (fromString $ show $ asMaxFileSize p)
            fileInput (fieldName submissionFileField)
        else
          Bootstrap.textArea (fieldName submissionTextField) "" ""
      Bootstrap.submitButton (fieldName submitSolutionBtn) (fromString $ msg $ msg_Submission_Submit "Submit")

  where
    submission = Pages.submission ()
    aspects = Assignment.aspects $ asValue p

    assignmentPassword msg =
      when (Assignment.isPasswordProtected aspects) $ do
        H.p $ fromString . msg $ msg_Submission_Info_Password
          "This assignment can only accept submissions by providing the password."
        Bootstrap.passwordInput (fieldName submissionPwdField) (msg $ msg_Submission_Password "Password for the assignment:")
        Bootstrap.passwordInput (fieldName submissionPwdAgainField) (msg $ msg_Submission_PasswordAgain "Password again:")

-- Creates a table line first element is a bold text and the second is a HTML snippet
infixl 7 .|.
name .|. value = H.tr $ do
  H.td $ b $ fromString $ name
  H.td value

resolveStatus :: I18N -> Maybe String -> H.Html
resolveStatus msg Nothing     = fromString . msg $ msg_SubmissionList_NotEvaluatedYet "Not evaluated yet"
resolveStatus _msg (Just str) = fromString str
