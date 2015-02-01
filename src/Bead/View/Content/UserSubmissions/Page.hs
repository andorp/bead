{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.UserSubmissions.Page (
    userSubmissions
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.String (fromString)
import           Data.Time (UTCTime)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

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
  page <- case mDesc of
    Nothing -> return unauthorized
    Just  d -> do
      tc <- userTimeZoneToLocalTimeConverter
      return $ userSubmissionHtml tc d
  return page

unauthorized :: IHtml
unauthorized = do
  msg <- getI18N
  return . fromString . msg $ msg_UserSubmissions_NonAccessibleSubmissions "This submission cannot be accessed by this user."

userSubmissionHtml :: UserTimeConverter -> UserSubmissionDesc -> IHtml
userSubmissionHtml ut u = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ msg_UserSubmissions_Course "Course:")         .|. (usCourse u)
      (msg $ msg_UserSubmissions_Assignment "Assignment:") .|. (usAssignmentName u)
      (msg $ msg_UserSubmissions_Student "Student:")       .|. (usStudent u)
    Bootstrap.rowColMd12 $ h3 $
      fromString $ msg $ msg_UserSubmissions_SubmittedSolutions "Submissions"
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
      (msg $ msg_UserSubmissions_NotFound "Not found")
      (msg $ msg_UserSubmissions_NonEvaluated "Not evaluated")
      (msg . bool (msg_UserSubmissions_Tests_Passed "Tests are passed") (msg_UserSubmissions_Tests_Failed "Tests are failed"))
      (const (evaluationDataMap bin pct . evResult))
      where
        bin (Binary b) = msg $ resultCata (msg_UserSubmissions_Accepted "Accepted")
                                          (msg_UserSubmissions_Rejected "Rejected")
                                          b
        pct (Percentage (Scores [x])) = fromString $ printf "%3.0f%%" (100 * x)
        pct (Percentage _) = fromString "Error: ???%"

infixl 7 .|.
title .|. value = tr $ do
  td $ b $ fromString title
  td $ fromString value
