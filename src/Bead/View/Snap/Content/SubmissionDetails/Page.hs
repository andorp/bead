{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionDetails.Page (
    submissionDetails
  ) where

import           Prelude hiding (div)

import           Data.List (intersperse)
import           Data.Monoid
import           Data.Time (getCurrentTime)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (submissionDetailsDesc)
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Snap.Content
import           Bead.View.Snap.Content.Bootstrap as Bootstrap
import           Bead.View.Snap.Content.Comments
import           Bead.View.Snap.Content.Utils
import           Bead.View.Snap.Content.SeeMore
import           Bead.View.Snap.Content.Submission.Page (resolveStatus)
import           Bead.View.Snap.Markdown

import           Text.Blaze.Html5 as H

submissionDetails = ViewModifyHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  , uTime :: UserTimeConverter
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = withUserState $ \s -> do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  -- TODO: Refactor use guards
  usersSubmission ak sk $ \submission -> do
    let render p = renderBootstrapPage $ bootstrapUserFrame s p
    case submission of
      Nothing -> render invalidSubmission
      Just _sm -> do
        sd <- userStory $ submissionDetailsDesc sk
        tc <- userTimeZoneToLocalTimeConverter
        render $
          submissionDetailsContent PageData {
              smKey = sk
            , aKey  = ak
            , smDetails = sd
            , uTime = tc
            }

submissionDetailsPostHandler :: POSTContentHandler
submissionDetailsPostHandler = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  c  <- getParameter (stringParameter (fieldName commentValueField) "Hozzászólás")
  now <- liftIO $ getCurrentTime
  mname <- getName <$> userState
  let uname = case mname of
                Just un -> un
                Nothing -> "???"
  usersSubmission ak sk $ \s -> do
    return $ case s of
      Nothing -> LogMessage "Submission does not belong to the user"
      Just _  -> SubmissionComment sk Comment {
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
      (\_username _page name _role _token _timezone _status -> Just name)

submissionDetailsContent :: PageData -> IHtml
submissionDetailsContent p = do
  msg <- getI18N
  return $ do
    let info = smDetails p
    let tc   = uTime p
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ Msg_SubmissionDetails_Course "Course, group:")  .|. (sdGroup info)
      (msg $ Msg_SubmissionDetails_Admins "Teacher:")        .|. (join . intersperse ", " $ sdTeacher info)
      (msg $ Msg_SubmissionDetails_Assignment "Assignment:") .|. (Assignment.name $ sdAssignment info)
      (msg $ Msg_SubmissionDetails_Deadline "Deadline:")     .|. (showDate . tc . Assignment.end $ sdAssignment info)
    Bootstrap.rowColMd12 $ do
      h2 $ fromString $ msg $ Msg_SubmissionDetails_Description "Assignment"
      div # assignmentTextDiv $ markdownToHtml . Assignment.desc $ sdAssignment info
    Bootstrap.rowColMd12 $ do
      let downloadSubmissionButton =
            Bootstrap.buttonLink
              (routeOf $ Pages.getSubmission (smKey p) ())
              (msg $ Msg_SubmissionDetails_Solution_Zip_Link "Download")
      h2 $ fromString $ msg $ Msg_SubmissionDetails_Solution "Submission"
      if (Assignment.isZippedSubmissions . Assignment.aspects $ sdAssignment info)
        then do
          Bootstrap.helpBlock $ fromString . msg $ Msg_SubmissionDetails_Solution_Zip_Info $ mconcat
            [ "The submission was uploaded as a compressed file so it could not be displayed verbatim.  "
            , "But it may be downloaded as a file by clicking on the link."
            ]
          downloadSubmissionButton
        else do
          H.p $ fromString . msg $ Msg_SubmissionDetails_Solution_Text_Info $
            "The submission may be downloaded as a plain text file by clicking on the link."
          downloadSubmissionButton
          div # submissionTextDiv $ seeMorePre msg maxLength maxLines $ sdSubmission info
    Bootstrap.rowColMd12 $ do
      h2 $ fromString $ msg $ Msg_SubmissionDetails_Evaluation "Evaluation"
      resolveStatus msg $ sdStatus info
    Bootstrap.rowColMd12 $ h2 $ fromString $ msg $ Msg_Comments_Title "Comments"
    postForm (routeOf $ submissionDetails (aKey p) (smKey p)) $ do
      Bootstrap.textArea (fieldName commentValueField)
                         (fromString $ msg $ Msg_SubmissionDetails_NewComment "New comment")
                         mempty
      Bootstrap.submitButton "" (fromString $ msg $ Msg_SubmissionDetails_SubmitComment "Submit")
    let studentComments = forStudentCFs $ submissionDetailsDescToCFs info
    when (not $ null studentComments) $ do
      Bootstrap.rowColMd12 hr
      i18n msg $ commentsDiv tc studentComments

  where
    submissionDetails ak sk = Pages.submissionDetails ak sk ()
    maxLength = 100
    maxLines  = 5

invalidSubmission :: IHtml
invalidSubmission = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $
      fromString $ msg $ Msg_SubmissionDetails_InvalidSubmission "This submission cannot be accessed by this user."

-- Renders a table row with two data cells the 
infixl 7 .|.
name .|. value = tr $ do
  td $ b $ fromString name
  td $ fromString value

