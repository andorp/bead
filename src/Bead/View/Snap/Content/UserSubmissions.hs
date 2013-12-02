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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)
import Data.Time (UTCTime, LocalTime)
import Text.Printf (printf)

userSubmissions :: Content
userSubmissions = getContentHandler userSubmissionPage

userSubmissionPage :: GETContentHandler
userSubmissionPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  aKey     <- getParameter assignmentKeyPrm
  mDesc <- runStoryE $ U.userSubmissions username aKey
  case mDesc of
    Nothing -> renderPagelet $ withUserFrame s unauthorized
    Just  d -> do
      tc <- usersTimeZoneConverter
      renderPagelet $ withUserFrame s (userSubmissionHtml tc d)

unauthorized :: Pagelet
unauthorized = onlyHtml $ mkI18NHtml $ const $
  "You have tried to reach a submission that not belongs to your groups"

userSubmissionHtml :: UserTimeConverter -> UserSubmissionDesc -> Pagelet
userSubmissionHtml ut u = onlyHtml $ mkI18NHtml $ \i18n -> do
  H.table # centerTable $ do
    H.tr $ do
      firstCol  . i18n $ "Course:"
      secondCol . usCourse $ u
    H.tr $ do
      firstCol  . i18n $ "Assignment:"
      secondCol . usAssignmentName $ u
    H.tr $ do
      firstCol  . i18n $ "Student:"
      secondCol . usStudent $ u
  H.p $ do
    H.h3 . fromString . i18n $ "Submitted Solutions"
    submissionTable ut i18n . usSubmissions $ u
  where
    firstCol  t = H.td # textAlignRight $ H.b $ fromString t
    secondCol t = H.td # textAlignLeft        $ fromString t

submissionTable :: UserTimeConverter -> I18N -> [(SubmissionKey, UTCTime, SubmissionInfo)] -> Html
submissionTable userTime i18n s = do
  table "submission-table" (className userSubmissionTable) # informationalTable $ do
    headerLine
    mapM_ submissionLine s

  where
    headerLine = H.tr $ do
      (H.th # (informationalCell <> grayBackground)) . fromString . i18n $ "Date of submission"
      (H.th # (informationalCell <> grayBackground)) . fromString . i18n $ "Results"

    submissionLine (sk,t,si) = H.tr $ do
      H.td # informationalCell $ sbmLink si sk t
      H.td # informationalCell $ fromString $ i18n $ submissionInfo si

    submissionInfo :: SubmissionInfo -> String
    submissionInfo = submissionInfoCata
      "Not Found"
      "Unevaluated"
      (const (evaluationDataMap bin pct))

    bin (Binary b) = resultCata (i18n "Passed") (i18n "Failed") b
    pct (Percentage (Scores [x])) = printf "%3.2f%%" (100 * x)
    pct (Percentage _) = "Error: ???%"

    sbmLink si sk t = case siEvaluationKey si of
      Nothing -> link
        (routeWithParams P.Evaluation [requestParam sk])
        (fromString . showDate $ userTime t)
      Just ek -> link
        (routeWithParams P.ModifyEvaluation [requestParam sk,requestParam ek] )
        (showDate $ userTime t)

