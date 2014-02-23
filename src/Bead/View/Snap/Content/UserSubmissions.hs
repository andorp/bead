{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserSubmissions (
    userSubmissions
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Email(..), roles, showDate)
import Bead.Domain.Shared.Evaluation
import qualified Bead.Controller.UserStories as U (userSubmissions)
import Bead.Controller.Pages as P (Page(ModifyEvaluation, Evaluation))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Bead.View.Snap.I18N (IHtml)
import Data.Function (on)
import Data.List (sortBy)
import Data.String (fromString)
import Data.Time (UTCTime, LocalTime)
import Text.Printf (printf)

userSubmissions :: Content
userSubmissions = getContentHandler userSubmissionPage

userSubmissionPage :: GETContentHandler
userSubmissionPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  aKey     <- getParameter assignmentKeyPrm
  mDesc <- userStory $ U.userSubmissions username aKey
  case mDesc of
    Nothing -> renderPagelet $ withUserFrame s unauthorized
    Just  d -> do
      tc <- usersTimeZoneConverter
      renderPagelet $ withUserFrame s (userSubmissionHtml tc d)

unauthorized :: IHtml
unauthorized = do
  msg <- getI18N
  return . fromString . msg $ Msg_UserSubmissions_NonAccessibleSubmissions "This submission cannot be accessed by this user."

userSubmissionHtml :: UserTimeConverter -> UserSubmissionDesc -> IHtml
userSubmissionHtml ut u = do
  msg <- getI18N
  return $ do
    H.table $ do
      H.tr $ do
        firstCol  . msg $ Msg_UserSubmissions_Course "Course:"
        secondCol . usCourse $ u
      H.tr $ do
        firstCol  . msg $ Msg_UserSubmissions_Assignment "Assignment:"
        secondCol . usAssignmentName $ u
      H.tr $ do
        firstCol  . msg $ Msg_UserSubmissions_Student "Student:"
        secondCol . usStudent $ u
    H.p $ do
      H.h3 . fromString . msg $ Msg_UserSubmissions_SubmittedSolutions "Submissions"
      i18n msg . submissionTable ut . sortDescendingByTime . usSubmissions $ u
  where
    firstCol  t = H.td # textAlignLeft $ H.b $ fromString $ t
    secondCol t = H.td # textAlignLeft $ fromString t

    submissionTime (_submissionKey, time, _submissionInfo) = time

    sortDescendingByTime = reverse . sortBy (compare `on` submissionTime)

submissionTable :: UserTimeConverter -> [(SubmissionKey, UTCTime, SubmissionInfo)] -> IHtml
submissionTable userTime s = do
  msg <- getI18N
  return $ table "submission-table" (className userSubmissionTable) # informationalTable $ do
    headerLine msg
    mapM_ (submissionLine msg) s

  where
    headerLine msg = H.tr $ do
      H.th # (informationalCell <> grayBackground) $ fromString $ msg $ Msg_UserSubmissions_SubmissionDate "Date of submission"
      H.th # (informationalCell <> grayBackground) $ fromString $ msg $ Msg_UserSubmissions_Evaluation "Evaluation"

    submissionLine msg (sk,t,si) = H.tr $ do
      H.td # informationalCell $ sbmLink si sk t
      H.td # informationalCell $ submissionInfo msg si

    submissionInfo msg = fromString . submissionInfoCata
      (msg $ Msg_UserSubmissions_NotFound "Not found")
      (msg $ Msg_UserSubmissions_NonEvaluated "Not evaluated")
      (msg $ Msg_UserSubmissions_Tested "Tested")
      (const (evaluationDataMap bin pct))
      where
        bin (Binary b) = msg $ resultCata (Msg_UserSubmissions_Accepted "Accepted")
                                          (Msg_UserSubmissions_Rejected "Rejected")
                                          b
        pct (Percentage (Scores [x])) = fromString $ printf "%3.2f%%" (100 * x)
        pct (Percentage _) = fromString "Error: ???%"

    sbmLink si sk t = case siEvaluationKey si of
      Nothing -> linkWithText
        (routeOf (P.Evaluation sk))
        (fromString . showDate $ userTime t)
      Just ek -> linkWithText
        (routeOf (P.ModifyEvaluation sk ek))
        (showDate $ userTime t)

