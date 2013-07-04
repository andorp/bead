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
import qualified Text.Blaze.Html5.Attributes as A (id, style)

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
postCourseAssignment = CreateCourseAssignment
  <$> getParameter (customCourseKeyPrm (fieldName selectedCourse))
  <*> getValue -- assignment

-- * Group Assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserStateE $ \s -> do
  gs <- runStoryE S.administratedGroups
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Group gs))

postGroupAssignment :: POSTContentHandler
postGroupAssignment = CreateGroupAssignment
  <$> getParameter (customGroupKeyPrm (fieldName selectedGroup))
  <*> getValue -- assignment

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
newAssignmentContent pd = onlyHtml $ mkI18NHtml $ \i -> postForm (routeOf . page $ pd) $ H.div ! formDiv $ do
  H.div ! slimLeftCell  $ H.b $ (translate i "Assignment title")
  H.div ! slimRightCell $ textInput (fieldName assignmentNameField) 10 (amap assignmentName pd) ! fillDiv
  H.div ! leftCell $ do
    H.b $ (translate i "Active period")
    H.div ! A.id (fieldName startDateDivId) $ do
       translate i "Start date"
       H.br
       hiddenInput (fieldName assignmentStartField) ""
    H.div ! A.id (fieldName endDateDivId) $ do
       translate i "End date"
       H.br
       hiddenInput (fieldName assignmentEndField) ""
  H.div ! rightCell $ do
    H.b $ (translate i "Description text block / Description files")
    textAreaInput (fieldName assignmentDescField) (amap assignmentDesc pd) ! fillDiv
  H.div ! leftCell $ do
    H.b $ (translate i "Assignment Type")
    H.br
    enumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType $ pd)
  H.div ! rightCell $ do
    H.b $ (translate i "Test Data text block / Test data files")
    textAreaInput (fieldName assignmentTCsField) (amap assignmentTCs pd) ! fillDiv
  H.div ! leftCell $ do
    H.p $ (translate i "Select automated evaulation method")
    H.p $ do
      pageDataMap (const (translate i "Course")) (const (translate i "Group")) (const (translate i "")) pd
      H.br
      pageDataMap
        (valueTextSelection (fieldName selectedCourse))
        (valueTextSelection (fieldName selectedGroup))
        (hiddenInput (fieldName assignmentKeyField) . paramValue  . fst)
        pd
    H.p $ submitButton (fieldName saveSubmitBtn) (i "Save")
    where
      page :: PageData -> Page
      page = pageDataMap
                   (const P.NewCourseAssignment)
                   (const P.NewGroupAssignment)
                   (const P.ModifyAssignment)

      amap :: (Assignment -> a) -> PageData -> Maybe a
      amap f (PD_Assignment (_,a)) = Just . f $ a
      amap _ _                     = Nothing

-- CSS Section

slimLeftCell  = A.style "float: left;  width:30%; height: 5%"
slimRightCell = A.style "float: right; width:68%; height: 5%"
leftCell      = A.style "float: left;  width:30%; height: 30%"
rightCell     = A.style "float: right; width:68%; height: 44%"
fillDiv       = A.style "width: 98%; height: 90%"
formDiv       = A.style "width: 100%; height: 600px"
