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
submissionListPage = withUserState $ \s -> do
  ak <- getParameter assignmentKeyPrm
  usersAssignment ak $ \assignment -> do
    case assignment of
      Nothing -> renderPagelet . withUserFrame s $ invalidAssignment
      Just  _ -> do
        sl <- runStoryE (submissionListDesc ak)
        renderPagelet . withUserFrame s $ submissionListContent (PageData { asKey = ak, smList = sl })

submissionListContent :: PageData -> Pagelet
submissionListContent p = onlyHtml $ mkI18NHtml $ \i -> H.div ! A.class_ (className submissionListDiv) $ do
  H.table ! centerTable $ do
    H.tr $ do
      firstCol  (i "Group / Course")
      secondCol (slGroup . smList $ p)
    H.tr $ do
      firstCol (i "Teacher")
      secondCol (join . slTeacher . smList $ p)
  H.h3 (translate i "Submission list")
  table (fieldName submissionTableName) (className submissionListTable) $
    mapM_ submissionLine (slSubmissions . smList $ p)
  H.h2 $ (translate i "Assignment")
  H.div ! A.class_ (className assignmentTextDiv) $ H.pre $
    (fromString . slAssignmentText . smList $ p)
  where
    firstCol  t = H.td ! textAlignRight $ H.b $ fromString t
    secondCol t = H.td ! textAlignLeft        $ fromString t
    submissionLine (sk, time, status, t) = H.tr $ do
      H.td $ link
        (routeWithParams P.SubmissionDetails [requestParam (asKey p), requestParam sk])
        (fromString . show $ time)
      H.td (fromString status)

invalidAssignment :: Pagelet
invalidAssignment = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "You have tried to open an assignment that not belongs to you")

