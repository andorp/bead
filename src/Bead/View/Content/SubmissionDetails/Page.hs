{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SubmissionDetails.Page (
    submissionDetails
  ) where

import           Prelude hiding (div)

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Time (getCurrentTime)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content
import           Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Comments
import           Bead.View.Content.SeeMore
import           Bead.View.Content.Submission.Common
import           Bead.View.Content.Submission.Page (resolveStatus)
import           Bead.View.Markdown

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

submissionDetails = ViewModifyHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  , uTime :: UserTimeConverter
  , smLimit :: SubmissionLimit
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm

  (limit,sd) <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleSubmission sk
    sd  <- Story.submissionDetailsDesc sk
    lmt <- Story.assignmentSubmissionLimit ak
    return (lmt,sd)

  -- TODO: Refactor use guards
  -- getSubmission ak sk $ \submission -> do
  tc <- userTimeZoneToLocalTimeConverter
  return $
    submissionDetailsContent PageData {
        smKey = sk
      , aKey  = ak
      , smDetails = sd
      , uTime = tc
      , smLimit = limit
      }

submissionDetailsPostHandler :: POSTContentHandler
submissionDetailsPostHandler = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  c  <- getParameter (stringParameter (fieldName commentValueField) "Comment")
  now <- liftIO $ getCurrentTime
  mname <- getName <$> userState
  let uname = case mname of
                Just un -> un
                Nothing -> "???"

  _ <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleSubmission sk

  return $! SubmissionComment sk Comment {
                     comment = c
                   , commentAuthor = uname
                   , commentDate = now
                   , commentType = CT_Student
                   }
  where
    getName = userStateCata
      Nothing
      Nothing
      Nothing
      (\_username _uid _page name _role _token _timezone _status -> Just name)

submissionDetailsContent :: PageData -> IHtml
submissionDetailsContent p = do
  msg <- getI18N
  return $ do
    let info = smDetails p
    let tc   = uTime p
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ msg_SubmissionDetails_Course "Course, group:")  .|. (fromString . sdGroup $ info)
      (msg $ msg_SubmissionDetails_Admins "Teacher:")        .|. (fromString . join . intersperse ", " . sortHun $ sdTeacher info)
      (msg $ msg_SubmissionDetails_Assignment "Assignment:") .|. (fromString . Assignment.name $ sdAssignment info)
      (msg $ msg_SubmissionDetails_Deadline "Deadline:")     .|. (fromString . showDate . tc . Assignment.end $ sdAssignment info)
      maybe (return ()) (uncurry (.|.)) (remainingTries msg (smLimit p))
    Bootstrap.rowColMd12 $ do
      let downloadSubmissionButton =
            Bootstrap.buttonLink
              (routeOf $ Pages.getSubmission (smKey p) ())
              (msg $ msg_SubmissionDetails_Solution_Zip_Link "Download")
      if (Assignment.isZippedSubmissions . Assignment.aspects $ sdAssignment info)
        then do
          Bootstrap.helpBlock $ fromString . msg $ msg_SubmissionDetails_Solution_Zip_Info $ mconcat
            [ "The submission was uploaded as a compressed file so it could not be displayed verbatim.  "
            , "But it may be downloaded as a file by clicking on the link."
            ]
          downloadSubmissionButton
        else do
          H.p $ fromString . msg $ msg_SubmissionDetails_Solution_Text_Info $
            "The submission may be downloaded as a plain text file by clicking on the link."
          downloadSubmissionButton
          H.br
          div # submissionTextDiv $ seeMoreSubmission "submission-details-" msg maxLength maxLines $ sdSubmission info
    Bootstrap.rowColMd12 $ do
      H.a ! A.name (anchor SubmissionDetailsEvaluationDiv) $ mempty
      h2 $ fromString $ msg $ msg_SubmissionDetails_Evaluation "Evaluation"
      resolveStatus msg $ sdStatus info
    Bootstrap.rowColMd12 $ h2 $ fromString $ msg $ msg_Comments_Title "Comments"
    postForm (routeOf $ submissionDetails (aKey p) (smKey p)) $ do
      Bootstrap.textArea (fieldName commentValueField)
                         (fromString $ msg $ msg_SubmissionDetails_NewComment "New comment")
                         mempty
      Bootstrap.submitButton "" (fromString $ msg $ msg_SubmissionDetails_SubmitComment "Submit")
    let studentComments = forStudentCFs $ submissionDetailsDescToCFs info
    when (not $ null studentComments) $ do
      Bootstrap.rowColMd12 hr
      i18n msg $ commentsDiv "submission-details-comments-" tc studentComments

  where
    submissionDetails ak sk = Pages.submissionDetails ak sk ()
    maxLength = 2048
    maxLines  = 100

invalidSubmission :: IHtml
invalidSubmission = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $
      fromString $ msg $ msg_SubmissionDetails_InvalidSubmission "This submission cannot be accessed by this user."
