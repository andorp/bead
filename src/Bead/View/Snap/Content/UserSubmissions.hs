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
import qualified Bead.View.Snap.I18NHtml as H
import Bead.View.Snap.I18N (IHtml, constant)
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

unauthorized :: Pagelet
unauthorized = onlyHtml $ "Olyan megoldást próbáltál meg elérni, amelyik nem tartozik hozzád!"

userSubmissionHtml :: UserTimeConverter -> UserSubmissionDesc -> Pagelet
userSubmissionHtml ut u = onlyHtml $ do
  H.table # centerTable $ do
    H.tr $ do
      firstCol  $ "Tárgy:"
      secondCol . usCourse $ u
    H.tr $ do
      firstCol  $ "Feladat:"
      secondCol . usAssignmentName $ u
    H.tr $ do
      firstCol  $ "Hallgató:"
      secondCol . usStudent $ u
  H.p $ do
    H.h3 "Beadott megoldások"
    submissionTable ut . sortDescendingByTime . usSubmissions $ u
  where
    firstCol  t = H.td # textAlignRight $ H.b $ fromString t
    secondCol t = H.td # textAlignLeft        $ fromString t

    submissionTime (_submissionKey, time, _submissionInfo) = time

    sortDescendingByTime = reverse . sortBy (compare `on` submissionTime)

submissionTable :: UserTimeConverter -> [(SubmissionKey, UTCTime, SubmissionInfo)] -> IHtml
submissionTable userTime s = do
  table "submission-table" (className userSubmissionTable) # informationalTable $ do
    headerLine
    mapM_ submissionLine s

  where
    headerLine = H.tr $ do
      H.th # (informationalCell <> grayBackground) $ "Beküldés dátuma"
      H.th # (informationalCell <> grayBackground) $ "Értékelés"

    submissionLine (sk,t,si) = H.tr $ do
      H.td # informationalCell $ sbmLink si sk t
      H.td # informationalCell $ fromString $ submissionInfo si

    submissionInfo :: SubmissionInfo -> String
    submissionInfo = submissionInfoCata
      "Nem található"
      "Nem értékelt"
      (const (evaluationDataMap bin pct))

    bin (Binary b) = resultCata "Elfogadott" "Elutasított" b
    pct (Percentage (Scores [x])) = printf "%3.2f%%" (100 * x)
    pct (Percentage _) = "Error: ???%"

    sbmLink si sk t = case siEvaluationKey si of
      Nothing -> link
        (routeOf (P.Evaluation sk))
        (fromString . showDate $ userTime t)
      Just ek -> link
        (routeOf (P.ModifyEvaluation sk ek))
        (showDate $ userTime t)

