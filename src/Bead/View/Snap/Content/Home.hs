{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Data.String (fromString)
import Control.Monad (when, liftM)

import Bead.Domain.Relationships (AssignmentDesc(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.Controller.UserStories (userAssignments)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

home :: Content
home = getContentHandler homePage

data HomePageData = HomePageData {
    userState   :: UserState
  , assignments :: [(AssignmentKey, AssignmentDesc)]
  }

homePage :: GETContentHandler
homePage = withUserStateE $ \s -> do
  a <- runStoryE userAssignments
  blaze $ withUserFrame s (homeContent (HomePageData s a))

homeContent :: HomePageData -> Html
homeContent d = do
  let s = userState d
  when (isAdmin s) $ H.p $ "Admin's menu"
  when (isCourseAdmin s) $ H.p $ do
    "Course Admin's menu"
    linkToPage P.NewCourseAssignment
  when (isProfessor s) $ H.p $ do
    "Teacher's menu"
    linkToPage P.NewGroupAssignment
  when (isStudent s) $ H.p $ do
    "Student's menu"
    availableAssignments (assignments d)

availableAssignments :: [(AssignmentKey,AssignmentDesc)] -> Html
availableAssignments as = do
  table "available-assignments" $ do
    mapM_ assignmentLine as
  where
    assignmentLine (k,a) = H.tr $ do
      H.td $ link (routeWithParams P.Submission [requestParam k]) "New submission"
      H.td (fromString (aTitle a))
      H.td (fromString (aGroup a))
