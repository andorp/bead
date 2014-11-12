{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserSubmissions.Page (
    userSubmissions
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.String (fromString)
import           Data.Time (UTCTime)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap

import           Text.Blaze.Html5 as H
import           Text.Printf (printf)

userSubmissions = ViewHandler userSubmissionPage

userSubmissionPage :: GETContentHandler
userSubmissionPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  aKey     <- getParameter assignmentKeyPrm
  mDesc <- userStory $ do
             Story.isAdministratedAssignment aKey
             Story.userSubmissions username aKey
  let render p = renderBootstrapPage $ bootstrapUserFrame s p
  case mDesc of
    Nothing -> render unauthorized
    Just  d -> do
      tc <- userTimeZoneToLocalTimeConverter
      render $ userSubmissionHtml tc d

unauthorized :: IHtml
unauthorized = do
  msg <- getI18N
  return . fromString . msg $ Msg_UserSubmissions_NonAccessibleSubmissions "This submission cannot be accessed by this user."

userSubmissionHtml :: UserTimeConverter -> UserSubmissionDesc -> IHtml
userSubmissionHtml ut u = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ Msg_UserSubmissions_Course "Course:")         .|. (usCourse u)
      (msg $ Msg_UserSubmissions_Assignment "Assignment:") .|. (usAssignmentName u)
      (msg $ Msg_UserSubmissions_Student "Student:")       .|. (usStudent u)
    Bootstrap.rowColMd12 $ h3 $
      fromString $ msg $ Msg_UserSubmissions_SubmittedSolutions "Submissions"
    i18n msg . submissionTable ut . sortDescendingByTime $ usSubmissions u
  where
    submissionTime (_submissionKey, time, _submissionInfo) = time
    sortDescendingByTime = reverse . sortBy (compare `on` submissionTime)

submissionTable :: UserTimeConverter -> [(SubmissionKey, UTCTime, SubmissionInfo)] -> IHtml
submissionTable userTime submissions = do
  msg <- getI18N
  return $ Bootstrap.rowColMd12 $ Bootstrap.listGroup $ do
    mapM_ (line msg) submissions

  where
    line msg (sk,t,si) =
      let (link, date) = linkAndDate si sk t
      in Bootstrap.listGroupLinkItem
           link
           (do Bootstrap.badge (submissionInfo msg si); fromString date)

    linkAndDate si sk t = case siEvaluationKey si of
      Nothing -> ( (routeOf (Pages.evaluation sk ())), (fromString . showDate $ userTime t) )
      Just ek -> ( (routeOf (Pages.modifyEvaluation sk ek ())) , (showDate $ userTime t) )

    submissionInfo msg = fromString . submissionInfoCata
      (msg $ Msg_UserSubmissions_NotFound "Not found")
      (msg $ Msg_UserSubmissions_NonEvaluated "Not evaluated")
      (msg . bool (Msg_UserSubmissions_Tests_Passed "Tests are passed") (Msg_UserSubmissions_Tests_Failed "Tests are failed"))
      (const (evaluationDataMap bin pct . evResult))
      where
        bin (Binary b) = msg $ resultCata (Msg_UserSubmissions_Accepted "Accepted")
                                          (Msg_UserSubmissions_Rejected "Rejected")
                                          b
        pct (Percentage (Scores [x])) = fromString $ printf "%3.0f%%" (100 * x)
        pct (Percentage _) = fromString "Error: ???%"

infixl 7 .|.
title .|. value = tr $ do
  td $ b $ fromString title
  td $ fromString value
