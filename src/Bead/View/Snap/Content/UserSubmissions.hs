{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserSubmissions (
    userSubmissions
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Email(..), roles)
import qualified Bead.Controller.UserStories as U (userSubmissions)
import Bead.Controller.Pages as P (Page(ModifyEvaulation, Evaulation))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)
import Data.Time (UTCTime)

userSubmissions :: Content
userSubmissions = getContentHandler userSubmissionPage

userSubmissionPage :: GETContentHandler
userSubmissionPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  aKey     <- getParameter assignmentKeyPrm
  mDesc <- runStoryE $ U.userSubmissions username aKey
  case mDesc of
    Nothing -> renderPagelet $ withUserFrame s unauthorized
    Just  d -> renderPagelet $ withUserFrame s (userSubmissionHtml d)

unauthorized :: Pagelet
unauthorized = onlyHtml $ mkI18NHtml $ const $
  "You have tried to reach a submission that not belongs to your groups"

userSubmissionHtml :: UserSubmissionDesc -> Pagelet
userSubmissionHtml u = onlyHtml $ mkI18NHtml $ \i18n -> do
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
    H.h3 . fromString . i18n $ "Submitted Solutions: "
    submissionTable i18n . usSubmissions $ u
  where
    firstCol  t = H.td # textAlignRight $ H.b $ fromString t
    secondCol t = H.td # textAlignLeft        $ fromString t

submissionTable :: I18N -> [(SubmissionKey, UTCTime, SubmissionInfo, EvaulatedWith)] -> Html
submissionTable i18n s = do
  table' "submission-table" # informationalTable $ do
    headerLine
    mapM_ submissionLine s

  where
    headerLine = H.tr $ do
      (H.th # (informationalCell <> grayBackground)) . fromString . i18n $ "Date of submission"
      (H.th # (informationalCell <> grayBackground)) . fromString . i18n $ "Evaulated By"
      (H.th # (informationalCell <> grayBackground)) . fromString . i18n $ ""

    submissionLine (sk,t,si,ev) = H.tr $ do
      H.td # informationalCell $ sbmLink si sk t
      H.td # informationalCell $ fromString $ i18n $ submissionInfo si
      H.td # informationalCell $ fromString $ i18n $ evaulatedWith  ev

    submissionInfo :: SubmissionInfo -> String
    submissionInfo Submission_Not_Found   = "Not Found"
    submissionInfo Submission_Unevaulated = "Unevaulated"
    submissionInfo (Submission_Result _ _) = "Result"

    evaulatedWith EvHand = "By Hand"

    sbmLink si sk t = case siEvaulationKey si of
      Nothing -> link
        (routeWithParams P.Evaulation [requestParam sk])
        (fromString . show $ t)
      Just ek -> link
        (routeWithParams P.ModifyEvaulation [requestParam sk,requestParam ek] )
        (show t)

