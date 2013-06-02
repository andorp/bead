{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.UserStories where

import Control.Monad (join)

import Test.WebDriver
import Test.WebDriver.Commands

import Test.WebDriver.Tools
import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.Frames

import Bead.Controller.Pages
import Bead.Domain.Entities hiding (CourseAdmin)

-- * User stories

loginUser :: String -> LoginData -> Test () -> Test ()
loginUser url l m = setTestFrames [webhandlerException] $ do
  cleanUp
    (do openPage url
        page l
        page . homePage $ l
        m
        page logout)
    (page logout)

registration :: String -> RegistrationData -> Test ()
registration url reg = do
  openPage url
  click =<< findElem (ByLinkText "Create new user")
  page reg
  loginUser url (loginInfo reg) (return ())

validLogin :: String -> LoginData -> Test ()
validLogin url login = loginUser url login (return ())

invalidLogin :: String -> LoginData -> Test ()
invalidLogin url loginData = do
  openPage url
  page loginData
  checkIfPageIs loginData

changeUserRole :: String -> LoginData -> LoginData -> Role -> Test ()
changeUserRole url adminUser user r = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData . selectUserToModify . lUsername $ user
    page (UserDetailsData { udRole = r })
  -- Login with the user
  validLogin url user

adminCreatesCourse :: String -> LoginData -> CourseData -> Test ()
adminCreatesCourse url adminUser c = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData . createCourse $ c

assignCourseAdmin :: String -> LoginData -> String -> String -> Test ()
assignCourseAdmin url adminUser courseAdmin c = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData $ setCourseAdmin courseAdmin c

-- * Course Admin

cAdminCreateGroup :: String -> LoginData -> GroupData -> Test ()
cAdminCreateGroup url courseAdmin g = do
  loginUser url courseAdmin $ do
    selectPageFromMenu CourseAdmin
    onPage CourseAdminPage $ createGroup $ g
    onPage CourseAdminPage $ checkGroupSelection $ g

cAdminAssignGroupAdmin :: String -> LoginData -> LoginData -> String -> Test ()
cAdminAssignGroupAdmin url courseAdmin groupAdmin g = do
  loginUser url courseAdmin $ do
    selectPageFromMenu CourseAdmin
    onPage CourseAdminPage $ setGroupAdmin (lUsername groupAdmin) g
  loginUser url groupAdmin $ do
    findGroupSubmissionTable g

cAdminCreatesAssignment :: String -> LoginData -> LoginData -> AssignmentData -> Test ()
cAdminCreatesAssignment url courseAdmin student aData = do
  loginUser url courseAdmin $ do
    selectPageLink NewCourseAssignment
    page aData
    selectPageFromMenu Home
  loginUser url student $ do
    findGroupAssignmentInTable
      (aGroupOrCourse aData)
      (aName aData)

cAdminEvaulateSubmission
  :: String -> LoginData
  -> SelectSubmissionData -> Int -> EvaulationData
  -> Test ()
cAdminEvaulateSubmission url courseAdmin s noOfSbm e = do
  loginUser url courseAdmin $ do
    page s
    page (UserSubmissionsData noOfSbm)
    page e

-- * Group Admin

gAdminCreatesAssignment :: String -> LoginData -> LoginData -> AssignmentData -> Test ()
gAdminCreatesAssignment url groupAdmin student aData = do
  loginUser url groupAdmin $ do
    selectPageLink NewGroupAssignment
    page aData
    selectPageFromMenu Home
  loginUser url student $ do
    findGroupAssignmentInTable
      (aGroupOrCourse aData)
      (aName aData)

gAdminEvaulateSubmission
  :: String -> LoginData
  -> SelectSubmissionData -> Int -> EvaulationData
  -> Test ()
gAdminEvaulateSubmission url groupAdmin s noOfSbm e = do
  loginUser url groupAdmin $ do
    page s
    page (UserSubmissionsData noOfSbm)
    page e
    -- TODO Check if the comment appears on the list

-- * Student

groupRegistration :: String -> LoginData -> String -> Test ()
groupRegistration url student g = do
  loginUser url student $ do
    selectPageFromMenu GroupRegistration
    page (GroupRegData { grName = g})

studentSubmitsSolution :: String -> LoginData -> String -> String -> String -> Test ()
studentSubmitsSolution url student g a s = do
  loginUser url student $ do
    clickNewSolution g a
    page (SubmissionData s)
    -- TODO Check if the solution appears on the list

studentCommentsOnSolution :: String -> LoginData -> String -> String -> Int -> String -> Test ()
studentCommentsOnSolution url student g a no comment = do
  loginUser url student $ do
    clickOnAssignment g a
    page (SubmissionListData no)
    page (CommentData comment)
    checkCommentOnPage (CommentData comment)

-- * Navigation

selectPageFromMenu :: Page -> Test ()
selectPageFromMenu p = do
  failsOnFalse (join [show p," is not found in the menu"]) $ doesElementExist p
  click <@> p

selectPageLink :: Page -> Test ()
selectPageLink = selectPageFromMenu

