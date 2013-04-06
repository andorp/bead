{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Data.String (fromString)
import Control.Monad (when, liftM)

import Bead.Domain.Relationships (AssignmentDesc(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

home :: Content
home = getContentHandler homePage

homePage :: GETContentHandler
homePage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (homeContent s)

homeContent :: UserState -> Html
homeContent s = do
  when (isAdmin s) $ H.p $ "Admin's menu"
  when (isCourseAdmin s) $ H.p $ do
    "Course Admin's menu"
    linkToPage P.NewCourseAssignment
  when (isProfessor s) $ H.p $ do
    "Teacher's menu"
    linkToPage P.NewGroupAssignment
  when (isStudent s) $ H.p $ do
    "Student's menu"
    availableAssignments testDescriptions

availableAssignments :: [AssignmentDesc] -> Html
availableAssignments as = do
  table "available-assignments" $ do
    mapM_ assignmentLine as
  where
    assignmentLine a = H.tr $ do
      H.td (fromString (aTitle a))
      H.td (fromString (aDesc a))

testDescriptions :: [AssignmentDesc]
testDescriptions = [
    AssignmentDesc True "t1" "d1" 0 0 0
  , AssignmentDesc True "t2" "d2" 0 0 0
  ]
