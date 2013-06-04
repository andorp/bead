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

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (class_)

submissionList :: Content
submissionList = getContentHandler submissionListPage

data PageData = PageData {
    asKey :: AssignmentKey
  , smList :: SubmissionListDesc
  }

submissionListPage :: GETContentHandler
submissionListPage = withUserStateE $ \s -> do
  ak <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  usersAssignment ak $ \assignment -> do
    case assignment of
      Nothing -> renderPagelet . withUserFrame s $ invalidAssignment
      Just  _ -> do
        sl <- runStoryE (submissionListDesc ak)
        renderPagelet . withUserFrame s $ submissionListContent (PageData { asKey = ak, smList = sl })

submissionListContent :: PageData -> Pagelet
submissionListContent p = onlyHtml $ mkI18NHtml $ \i -> H.div ! A.class_ (className submissionListDiv) $ do
  H.p $ do
    (translate i "Group / Course")
    (fromString . slGroup . smList $ p)
  H.p $ do
    (translate i "Teacher")
    (fromString . join . slTeacher . smList $ p)
  H.p $ do
    (translate i "Submission list")
    table (fieldName submissionTableName) (className submissionListTable) $
      mapM_ submissionLine (slSubmissions . smList $ p)
  H.p $ do
    (translate i "Assignment")
    (fromString . slAssignmentText . smList $ p)
  where
    submissionLine (sk, time, status, t) = H.tr $ do
      H.td $ link
        (routeWithParams P.SubmissionDetails [requestParam (asKey p), requestParam sk])
        (fromString . show $ time)
      H.td (fromString status)

invalidAssignment :: Pagelet
invalidAssignment = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "You have tried to open an assignment that not belongs to you")

