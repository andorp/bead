{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  ) where

import Control.Monad (liftM)
import Data.Either (either)

import Bead.Controller.Pages as P (Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.UserActions (UserAction(CreateGroupAssignment, CreateCourseAssignment))

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

newCourseAssignment :: Content
newCourseAssignment = getPostContentHandler newCourseAssignmentPage postCourseAssignment

newGroupAssignment :: Content
newGroupAssignment = getPostContentHandler newGroupAssignmentPage postGroupAssignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = withUserStateE $ \s -> do
  cs <- runStoryE administratedCourses
  blaze $ withUserFrame s (newAssignmentContent P.NewCourseAssignment (Right cs))

postCourseAssignment :: POSTContentHandler
postCourseAssignment = do
  courseKey <- getParamE (fieldName selectedCourse) CourseKey "Selected course was not found"
  assignment <- getValue
  return $ CreateCourseAssignment courseKey assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserStateE $ \s -> do
  gs <- runStoryE administratedGroups
  blaze $ withUserFrame s (newAssignmentContent P.NewGroupAssignment (Left gs))

postGroupAssignment :: POSTContentHandler
postGroupAssignment = do
  groupKey <- getParamE (fieldName selectedGroup) GroupKey "Selected group was not found"
  assignment <- getValue
  return $ CreateGroupAssignment groupKey assignment

newAssignmentContent :: Page -> (Either [(GroupKey, Group)] [(CourseKey, Course)]) -> Html
newAssignmentContent p e = postForm (routeOf p) $ do
  H.p $ "Assignment title"
  textInput (fieldName assignmentNameField) 10 Nothing
  H.p $ "Description text block / Description files"
  textAreaInput (fieldName assignmentDescField) 300 200 Nothing
  H.p $ "Test Data text block / Test data files"
  textAreaInput (fieldName assignmentTCsField) 300 100 Nothing
  H.p $ "Select automata evaulation"
  H.p $ "Assignment Type"
  enumSelection (fieldName assignmentTypeField) Normal
  H.p $ "Active period"
  do {"Start date"; utcTimeInput (fieldName assignmentStartField) Nothing }
  do {"End date"  ; utcTimeInput (fieldName assignmentEndField) Nothing }
  H.p $ "Assignement"
  either
    (valueTextSelection (fieldName selectedGroup))
    (valueTextSelection (fieldName selectedCourse))
    e
  submitButton "Save"
