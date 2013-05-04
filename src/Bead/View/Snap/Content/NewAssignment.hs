{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  ) where

import Control.Monad (liftM)
import Data.Either (either)

import Bead.Controller.Pages (Page)
import qualified Bead.Controller.Pages as P (Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.UserActions (UserAction(CreateGroupAssignment, CreateCourseAssignment))

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

-- * Content Handlers

newCourseAssignment :: Content
newCourseAssignment = getPostContentHandler newCourseAssignmentPage postCourseAssignment

newGroupAssignment :: Content
newGroupAssignment = getPostContentHandler newGroupAssignmentPage postGroupAssignment

modifyAssignment :: Content
modifyAssignment = getPostContentHandler modifyAssignmentPage postModifyAssignment

data PageData
  = PD_Course     [(CourseKey, Course)]
  | PD_Group      [(GroupKey, Group)]
  | PD_Assignment (AssignmentKey, Assignment)

pageDataMap f _ _ (PD_Course x)     = f x
pageDataMap _ g _ (PD_Group  x)     = g x
pageDataMap _ _ h (PD_Assignment x) = h x

isEmptyData = pageDataMap null null (const False)

-- * Course Assignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = withUserStateE $ \s -> do
  cs <- runStoryE S.administratedCourses
  blaze $ withUserFrame s (newAssignmentContent (PD_Course cs))

postCourseAssignment :: POSTContentHandler
postCourseAssignment = do
  courseKey <- getParamE (fieldName selectedCourse) CourseKey "Selected course was not found"
  assignment <- getValue
  return $ CreateCourseAssignment courseKey assignment

-- * Group Assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserStateE $ \s -> do
  gs <- runStoryE S.administratedGroups
  blaze $ withUserFrame s (newAssignmentContent (PD_Group gs))

postGroupAssignment :: POSTContentHandler
postGroupAssignment = do
  groupKey <- getParamE (fieldName selectedGroup) GroupKey "Selected group was not found"
  assignment <- getValue
  return $ CreateGroupAssignment groupKey assignment

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = withUserStateE $ \s -> do
  ak <- getValue
  as <- runStoryE (S.loadAssignment ak)
  blaze $ withUserFrame s (newAssignmentContent (PD_Assignment (ak,as)))

postModifyAssignment :: POSTContentHandler
postModifyAssignment = ModifyAssignment <$> getValue <*> getValue

newAssignmentContent :: PageData -> Html
newAssignmentContent pd
  | isEmptyData pd = H.p $ pageDataMap (const "You are not an admin for any course")
                                       (const "You are not an admin for any groups")
                                       (const "This assignment is not created by you")
                                       pd
newAssignmentContent pd = postForm (routeOf . page $ pd) $ do
  H.p $ do
    "Assignment title"
    textInput (fieldName assignmentNameField) 10 (amap assignmentName pd)
  H.p $ do
    "Description text block / Description files"
    textAreaInput (fieldName assignmentDescField) 50 10 (amap assignmentDesc pd)
  H.p $ do
    "Test Data text block / Test data files"
    textAreaInput (fieldName assignmentTCsField) 50 10 (amap assignmentTCs pd)
  H.p $ "Select automated evaulation method"
  H.p $ do
    "Assignment Type"
    enumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType $ pd)
  H.p $ do
    "Evaulation Type"
    enumSelection (fieldName assignmentEvField) (maybe Scale id . amap evaulationType $ pd)
  H.p $ "Active period"
  do {"Start date"; utcTimeInput (fieldName assignmentStartField) (amap assignmentStart pd) }
  do {"End date"  ; utcTimeInput (fieldName assignmentEndField)   (amap assignmentEnd   pd) }
  H.p $ do
    pageDataMap (const "Course") (const "Group") (const "") pd
    pageDataMap
          (valueTextSelection (fieldName selectedCourse))
          (valueTextSelection (fieldName selectedGroup))
          (hiddenInput (fieldName assignmentKeyField) . paramValue  . fst)
          pd
  submitButton (fieldName saveSubmitBtn) "Save"
    where
      page :: PageData -> Page
      page = pageDataMap
                   (const P.NewCourseAssignment)
                   (const P.NewGroupAssignment)
                   (const P.ModifyAssignment)

      amap :: (Assignment -> a) -> PageData -> Maybe a
      amap f (PD_Assignment (_,a)) = Just . f $ a
      amap _ _                     = Nothing

