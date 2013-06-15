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

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (id)

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
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Course cs))

postCourseAssignment :: POSTContentHandler
postCourseAssignment = do
  courseKey <- getParamE (fieldName selectedCourse) CourseKey "Selected course was not found"
  assignment <- getValue
  return $ CreateCourseAssignment courseKey assignment

-- * Group Assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserStateE $ \s -> do
  gs <- runStoryE S.administratedGroups
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Group gs))

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
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Assignment (ak,as)))

postModifyAssignment :: POSTContentHandler
postModifyAssignment = ModifyAssignment <$> getValue <*> getValue

newAssignmentContent :: PageData -> Pagelet
newAssignmentContent pd
  | isEmptyData pd = onlyHtml $ mkI18NHtml $ \i -> do
      H.p $ pageDataMap (const . translate i $ "You are not an admin for any course")
                        (const . translate i $ "You are not an admin for any groups")
                        (const . translate i $ "This assignment is not created by you")
                        pd
newAssignmentContent pd = onlyHtml $ mkI18NHtml $ \i -> postForm (routeOf . page $ pd) $ do
  H.p $ do
    (translate i "Assignment title")
    textInput (fieldName assignmentNameField) 10 (amap assignmentName pd)
  H.p $ do
    (translate i "Description text block / Description files")
    textAreaInput (fieldName assignmentDescField) 50 10 (amap assignmentDesc pd)
  H.p $ do
    (translate i "Test Data text block / Test data files")
    textAreaInput (fieldName assignmentTCsField) 50 10 (amap assignmentTCs pd)
  H.p $ (translate i "Select automated evaulation method")
  H.p $ do
    (translate i "Assignment Type")
    enumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType $ pd)
  H.p $ (translate i "Active period")
  H.div ! A.id (fieldName startDateDivId) $ do
     translate i "Start date"
     hiddenInput (fieldName assignmentStartField) ""
     H.br
  H.div ! A.id (fieldName endDateDivId) $ do
     translate i "End date"
     hiddenInput (fieldName assignmentEndField) ""
     H.br
  H.p $ do
    pageDataMap (const (translate i "Course")) (const (translate i "Group")) (const (translate i "")) pd
    pageDataMap
          (valueTextSelection (fieldName selectedCourse))
          (valueTextSelection (fieldName selectedGroup))
          (hiddenInput (fieldName assignmentKeyField) . paramValue  . fst)
          pd
  submitButton (fieldName saveSubmitBtn) (i "Save")
    where
      page :: PageData -> Page
      page = pageDataMap
                   (const P.NewCourseAssignment)
                   (const P.NewGroupAssignment)
                   (const P.ModifyAssignment)

      amap :: (Assignment -> a) -> PageData -> Maybe a
      amap f (PD_Assignment (_,a)) = Just . f $ a
      amap _ _                     = Nothing

