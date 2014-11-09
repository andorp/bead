{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  , resolveStatus
  ) where

import           Data.List (intersperse, partition)
import           Data.Maybe (listToMaybe)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import           Data.Time
import           System.Directory (doesFileExist)
import           System.FilePath.Posix (takeExtension)

import           Snap.Util.FileUploads

import           Bead.Configuration (maxUploadSizeInKb)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Snap.Content
import           Bead.View.Snap.Content.Utils
import           Bead.View.Snap.Markdown (markdownToHtml)

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

submission = ViewModifyHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  , asTimeConv :: UserTimeConverter
  , asNow :: UTCTime
  , asMaxFileSize :: Int
  }

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile
  deriving (Eq,Show)

submissionPage :: GETContentHandler
submissionPage = withUserState $ \s -> do
  let render p = renderDynamicPagelet $ withUserFrame s p
  ak <- getParameter assignmentKeyPrm
  ut <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  size <- fmap maxUploadSizeInKb $ lift $ withTop configContext getConfiguration
  -- TODO: Refactor use guards
  userAssignmentForSubmission
    ak
    (\desc asg -> render $ submissionContent
       (PageData { asKey = ak, asValue = asg, asDesc = desc, asTimeConv = ut, asNow = now, asMaxFileSize = size }))
    (render invalidAssignment)

submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  uploadResult <- join $ lift $ do
    tmpDir <- withTop tempDirContext $ getTempDirectory
    size <- maxUploadSizeInKb <$> withTop configContext getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy $ \parts -> do
      results <- mapM handlePart parts
      return . return $ results
  ak <- getParameter assignmentKeyPrm
  userAssignmentForSubmission
    ak
    -- Assignment is for the user
    (\_desc asg -> do
       let aspects = Assignment.aspects asg
       if Assignment.isPasswordProtected aspects
         -- Password-protected assignment
         then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Feltöltési jelszó")
                 if Assignment.getPassword aspects == pwd
                   -- Passwords do match
                   then newSubmission ak aspects uploadResult
                   -- Passwords do not match
                   else return . ErrorMessage $ Msg_Submission_InvalidPassword "Invalid password, the solution could not be submitted!"
         -- Non password protected assignment
         else newSubmission ak aspects uploadResult)
    -- Assignment is not for the user
    (return . ErrorMessage $ Msg_Submission_NonUsersAssignment "The assignment is not for the actual user!")
  where
    newSubmission ak as up =
      if (not $ Assignment.isZippedSubmissions as)
        then submit $ SimpleSubmission <$> getParameter (stringParameter (fieldName submissionTextField) "Megoldás szövege")
        else
          case uploadedFile of
            Just (File name contents) ->
              if (takeExtension name == ".zip")
                then submit $ return $ ZippedSubmission contents
                else return $
                  ErrorMessage $ Msg_Submission_File_InvalidFile
                    "The extension of the file to be uploaded is incorrect."
            Just PolicyFailure      -> return $
              ErrorMessage $ Msg_Submission_File_PolicyFailure
                "The upload policy has been violated, probably the file was too large."
            Nothing                 -> return $
              ErrorMessage $ Msg_Submission_File_NoFileReceived
                "No file has been received."
            _                       -> return $
              ErrorMessage $ Msg_Submission_File_InternalError
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
  return $ (postForm (routeOf submission) `withId` (rFormId submissionForm)) ! A.enctype "multipart/form-data" $ H.div ! formDiv $ do
    H.table $ do
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Course "Course: ")
        H.td $ (fromString . aGroup $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Admin "Teacher: ")
        H.td $ (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Assignment "Assignment: ")
        H.td $ (fromString . Assignment.name . asValue $ p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Deadline "Deadline: ")
        H.td $ (fromString . showDate . (asTimeConv p) . Assignment.end $ asValue p)
      H.tr $ do
        H.td $ H.b $ fromString . msg $ Msg_Submission_TimeLeft "Time left: "
        H.td $ startEndCountdownDiv
          "ctd"
          (msg $ Msg_Submission_Days "day(s)")
          (msg $ Msg_Submission_DeadlineReached "Deadline is reached")
          (asNow p)
          (Assignment.end $ asValue p)
    H.h2 $ (fromString . msg $ Msg_Submission_Description "Description")
    H.div # assignmentTextDiv $
      markdownToHtml . Assignment.desc . asValue $ p
    H.h2 $ (fromString . msg $ Msg_Submission_Solution "Submission")
    (assignmentPasswordDiv msg)
    if (Assignment.isZippedSubmissions aspects)
      then
        H.div $ do
          H.p $ do
            fromString . msg $ Msg_Submission_Info_File
              "Please select a file with .zip extension to submit.  Note that the maximum file size in kilobytes: "
            fromString (show $ asMaxFileSize p)
          fileInput (fieldName submissionFileField)
      else
        H.div $ do
          textAreaInput (fieldName submissionTextField) Nothing ! A.rows "25" ! A.cols "80"
    H.br
    submitButton (fieldName submitSolutionBtn) (msg $ Msg_Submission_Submit "Submit")
    hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
  where
    submission = Pages.submission ()
    aspects = Assignment.aspects $ asValue p

    assignmentPasswordDiv msg =
      when (Assignment.isPasswordProtected aspects) $ do
        H.div $ do
          H.p $ fromString . msg $ Msg_Submission_Info_Password
            "This assignment can only accept submissions by providing the password."
          H.table $ do
            H.tr $ do
              H.td $ H.b $ fromString . msg $ Msg_Submission_Password "Password for the assignment:"
              H.td $ passwordInput (fieldName submissionPwdField) 20 Nothing ! A.required ""
            H.tr $ do
              H.td $ H.b $ fromString . msg $ Msg_Submission_PasswordAgain "Password again:"
              H.td $ passwordInput (fieldName submissionPwdAgainField) 20 Nothing ! A.required ""

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return . fromString . msg $ Msg_Submission_Invalid_Assignment "It is not allowed to access this assignment with this user."

resolveStatus :: I18N -> Maybe String -> H.Html
resolveStatus msg Nothing     = fromString . msg $ Msg_SubmissionList_NotEvaluatedYet "Not evaluated yet"
resolveStatus _msg (Just str) = fromString str

-- CSS Section

formDiv = A.style "width: 100%; height: 300px"
title   = A.style "float: left; width: 100%"
leftInput = A.style "float: left; width: 58%; height: 80%"
rightInfo = A.style "float: right; width: 39% height: 80%"
fillDiv   = A.style "width: 98%; height: 98%"
