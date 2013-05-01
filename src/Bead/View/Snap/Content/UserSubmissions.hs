{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserSubmissions (
    userSubmissions
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Email(..), roles)
import qualified Bead.Controller.UserStories as U (userSubmissions)
import Bead.Controller.Pages as P (Page(ModifyEvaulation))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)
import Data.Time (UTCTime)

userSubmissions :: Content
userSubmissions = getContentHandler userSubmissionPage

userSubmissionPage :: GETContentHandler
userSubmissionPage = withUserStateE $ \s -> do
  username <- getParamE (fieldName usernameField)      Username "Username is not found"
  aKey     <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  mDesc <- runStoryE $ U.userSubmissions username aKey
  case mDesc of
    Nothing -> blaze $ withUserFrame s unauthorized
    Just  d -> blaze $ withUserFrame s (userSubmissionHtml d)

unauthorized :: Html
unauthorized = "You have tried to reach a submission that not belongs to your groups"

userSubmissionHtml :: UserSubmissionDesc -> Html
userSubmissionHtml u = do
  H.p $ do
    "Course: "
    fromString . usCourse $ u
  H.p $ do
    "Assignment: "
    fromString . usAssignmentName $ u
  H.p $ do
    "Student: "
    fromString . usStudent $ u
  H.p $ do
    "Submitted Solutions: "
    submissionTable . usSubmissions $ u

submissionTable :: [(SubmissionKey, UTCTime, SubmissionInfo, EvaulatedWith)] -> Html
submissionTable s = table "submission-table" (className userSubmissionClassTable) $ do
  headerLine
  mapM_ submissionLine s

  where
    headerLine = H.tr $ do
      H.th "Date of submission"
      H.th "Evaulated By"
      H.th ""

    submissionLine (sk,t,si,ev) = H.tr $ do
      H.td $ sbmLink si sk t
      H.td $ submissionInfo si
      H.td $ evaulatedWith  ev

    submissionInfo :: SubmissionInfo -> Html
    submissionInfo Submission_Not_Found   = "Not Found"
    submissionInfo Submission_Unevaulated = "Unevaulated"
    submissionInfo (Submission_Passed _)  = "Passed"
    submissionInfo (Submission_Failed _)  = "Failed"

    evaulatedWith EvHand = "By Hand"

    sbmLink si sk t = case siEvaulationKey si of
      Nothing -> fromString . show $ t
      Just ek -> link
        (routeWithParams P.ModifyEvaulation [requestParam sk,requestParam ek] )
        (show t)
