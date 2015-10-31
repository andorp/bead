{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SubmissionList.Page (
    submissionList
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Data.String (fromString)
import           Data.Time

import           Text.Blaze.Html5 as H

import           Bead.Controller.UserStories
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Shared.Evaluation
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Submission.Common
import           Bead.View.Content.Utils
import           Bead.View.Markdown

submissionList = ViewHandler submissionListPage

data PageData = PageData {
    asKey :: AssignmentKey
  , smList :: SubmissionListDesc
  , uTime :: UserTimeConverter
  , smLimit :: SubmissionLimit
  }

submissionListPage :: GETContentHandler
submissionListPage = do
  ak <- getParameter assignmentKeyPrm
  -- TODO: Refactor use guards
  userStory $ do
    doesBlockAssignmentView ak
  page <- usersAssignment ak $ \assignment -> do
    case assignment of
      Nothing -> return invalidAssignment
      Just asg -> do
        now <- liftIO getCurrentTime
        case (Assignment.start asg > now) of
          True  -> return assignmentNotStartedYet
          False -> do
            (sl,lmt) <- userStory $ (,) <$> submissionListDesc ak <*> assignmentSubmissionLimit ak
            tc <- userTimeZoneToLocalTimeConverter
            return $ submissionListContent $
                PageData { asKey = ak
                         , smList = sortSbmListDescendingByTime sl
                         , uTime = tc
                         , smLimit = lmt
                         }
  return page

submissionListContent :: PageData -> IHtml
submissionListContent p = do
  msg <- getI18N
  return $ do
    let info = smList p -- Submission List Info
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ msg_SubmissionList_CourseOrGroup "Course, group:") .|. (slGroup info)
      (msg $ msg_SubmissionList_Admin "Teacher:") .|. (join $ slTeacher info)
      (msg $ msg_SubmissionList_Assignment "Assignment:") .|. (Assignment.name $ slAssignment info)
      (msg $ msg_SubmissionList_Deadline "Deadline:") .|. (showDate . (uTime p) . Assignment.end $ slAssignment info)
      maybe (return ()) (uncurry (.|.)) (remainingTries msg (smLimit p))
    Bootstrap.rowColMd12 $ h2 $ fromString $ msg $ msg_SubmissionList_Description "Description"
    H.div # assignmentTextDiv $
      (markdownToHtml . Assignment.desc . slAssignment . smList $ p)
    let submissions = slSubmissions info
    Bootstrap.rowColMd12 $ h2 $ fromString $ msg $ msg_SubmissionList_SubmittedSolutions "Submissions"
    either (userSubmissionTimes msg) (userSubmissionInfo msg) submissions
  where
    submissionDetails ak sk = Pages.submissionDetails ak sk ()

    submissionLine msg (sk, time, status, _t) = do
      Bootstrap.listGroupLinkItem
        (routeOf $ submissionDetails (asKey p) sk)
        (do Bootstrap.badge (resolveStatus msg status); fromString . showDate $ (uTime p) time)

    resolveStatus msg = fromString . submissionInfoCata
      (msg $ msg_SubmissionList_NotFound "Not Found")
      (msg $ msg_SubmissionList_NotEvaluatedYet "Not evaluated yet")
      (bool (msg $ msg_SubmissionList_TestsPassed "Tests are passed")
            (msg $ msg_SubmissionList_TestsFailed "Tests are failed"))
      (const (evaluationResultMsg . evResult))
      where
        evaluationResultMsg = evaluationResultCata
          (binaryCata (resultCata
            (msg $ msg_SubmissionList_Passed "Passed")
            (msg $ msg_SubmissionList_Failed "Failed")))
          (percentageCata (fromString . scores))
          (freeForm fromString)

        scores (Scores [])  = "0%"
        scores (Scores [p]) = concat [show . round $ 100 * p, "%"]
        scores _            = "???%"

    submissionTimeLine time = Bootstrap.listGroupTextItem $ showDate $ (uTime p) time

    userSubmissionInfo  msg submissions = do
      Bootstrap.rowColMd12 $ H.p $ fromString $ msg $ msg_SubmissionList_Info "Comments may be added for submissions."
      userSubmission msg (submissionLine msg) submissions

    userSubmissionTimes msg = userSubmission msg submissionTimeLine

    userSubmission msg line submissions =
      if (not $ null submissions)
        then do
          Bootstrap.rowColMd12 $ Bootstrap.listGroup $ mapM_ line submissions
        else do
          (Bootstrap.rowColMd12 $ fromString $ msg $ msg_SubmissionList_NoSubmittedSolutions "There are no submissions.")

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $ fromString $
      msg $ msg_SubmissionList_NonAssociatedAssignment "This assignment cannot be accessed by this user."

assignmentNotStartedYet :: IHtml
assignmentNotStartedYet = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $ fromString $
      msg $ msg_SubmissionList_NonReachableAssignment "This assignment cannot be accessed."

-- Creates a table line first element is a bold text and the second is a text
infixl 7 .|.
name .|. value = H.tr $ do
  H.td $ b $ fromString $ name
  H.td $ fromString value
