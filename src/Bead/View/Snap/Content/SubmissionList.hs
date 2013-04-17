{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionList (
    submissionList
  ) where

import Data.Time
import Data.String (fromString)
import Control.Monad (join, liftM)
import Control.Applicative ((<*>))

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionListDesc)
import Bead.Controller.Pages as P (Page(SubmissionList, SubmissionDetails))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Content.Utils
import Bead.Domain.Relationships

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

submissionList :: Content
submissionList = getPostContentHandler submissionListPage submissionListPostHandler

data PageData = PageData {
    asKey :: AssignmentKey
  , smList :: SubmissionListDesc
  }

submissionListPage :: GETContentHandler
submissionListPage = withUserStateE $ \s -> do
  ak <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  usersAssignment ak $ \assignment -> do
    case assignment of
      Nothing -> blaze . withUserFrame s $ invalidAssignment
      Just  _ -> do
        sl <- runStoryE (submissionListDesc ak)
        blaze . withUserFrame s $ submissionListContent (PageData { asKey = ak, smList = sl })

submissionListPostHandler :: POSTContentHandler
submissionListPostHandler = do
  -- TODO: Craete the site
  error "submissionListPostHandler"

submissionListContent :: PageData -> Html
submissionListContent p = postForm (routeOf P.SubmissionList) $ do
  H.p $ do
    "Group / Course"
    (fromString . slGroup . smList $ p)
  H.p $ do
    "Teacher"
    (fromString . join . slTeacher . smList $ p)
  H.p $ do
    "Submission list"
    table "submission-table" $
      mapM_ submissionLine (slSubmissions . smList $ p)
  H.p $ do
    "Assignment"
    (fromString . slAssignmentText . smList $ p)
  where
    submissionLine (sk, time, status, t) = H.td $ do
      H.tr $ link
        (routeWithParams P.SubmissionDetails [requestParam (asKey p), requestParam sk])
        (fromString . show $ time)
      H.tr (fromString status)

invalidAssignment :: Html
invalidAssignment = "You have tried to open an assignment that not belongs to you"

