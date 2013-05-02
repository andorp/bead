{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.UserStories where

import Control.Monad (join)

import Test.WebDriver
import Test.WebDriver.Commands

import Test.WebDriver.Tools
import Test.WebDriver.PageObject
import Test.WebDriver.SitePages

import Bead.Controller.Pages
import Bead.Domain.Entities hiding (CourseAdmin)

-- * User stories

loginUser :: String -> LoginData -> TWD () -> TWD ()
loginUser url l m = do
  cleanUp
    (do openPage url
        page l
        page . homePage $ l
        m
        page logout)
    (page logout)

registration :: String -> RegistrationData -> TWD ()
registration url reg = do
  openPage url
  click =<< findElem (ByLinkText "Create new user")
  page reg
  loginUser url (loginInfo reg) (return ())

validLogin :: String -> LoginData -> TWD ()
validLogin url login = loginUser url login (return ())

invalidLogin :: String -> LoginData -> TWD ()
invalidLogin url loginData = do
  openPage url
  page loginData
  checkIfPageIs loginData

changeUserRole :: String -> LoginData -> LoginData -> Role -> TWD ()
changeUserRole url adminUser user r = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData . selectUserToModify . lUsername $ user
    page (UserDetailsData { udRole = r })
  -- Login with the user
  validLogin url user

adminCreatesCourse :: String -> LoginData -> CourseData -> TWD ()
adminCreatesCourse url adminUser c = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData . createCourse $ c

assignCourseAdmin :: String -> LoginData -> String -> String -> TWD ()
assignCourseAdmin url adminUser courseAdmin c = do
  loginUser url adminUser $ do
    selectPageFromMenu Administration
    onPage AdminData $ setCourseAdmin courseAdmin c

-- * Course Admin

cAdminCreateGroup :: String -> LoginData -> GroupData -> TWD ()
cAdminCreateGroup url courseAdmin g = do
  loginUser url courseAdmin $ do
    selectPageFromMenu CourseAdmin
    onPage CourseAdminPage $ createGroup $ g
    onPage CourseAdminPage $ checkGroupSelection $ g

cAdminAssignGroupAdmin :: String -> LoginData -> LoginData -> String -> TWD ()
cAdminAssignGroupAdmin url courseAdmin groupAdmin g = do
  loginUser url courseAdmin $ do
    selectPageFromMenu CourseAdmin
    onPage CourseAdminPage $ setGroupAdmin (lUsername groupAdmin) g
  loginUser url groupAdmin $ do
    findGroupSubmissionTable g

-- * Group Admin

gAdminCreatesAssignment :: String -> LoginData -> LoginData -> AssignmentData -> TWD ()
gAdminCreatesAssignment url groupAdmin student aData = do
  loginUser url groupAdmin $ do
    selectPageLink NewGroupAssignment
    page aData
    selectPageFromMenu Home
  loginUser url student $ do
    findGroupAssignmentInTable
      (aGroupOrCourse aData)
      (aName aData)

gAdminEvaulateSubmission :: String -> LoginData -> TWD ()
gAdminEvaulateSubmission url groupAdmin = do
  loginUser url groupAdmin $ do
    undefined

-- * Student

groupRegistration :: String -> LoginData -> String -> TWD ()
groupRegistration url student g = do
  loginUser url student $ do
    selectPageFromMenu GroupRegistration
    page (GroupRegData { grName = g})

studentSubmitsSolution :: String -> LoginData -> String -> String -> String -> TWD ()
studentSubmitsSolution url student g a s = do
  loginUser url student $ do
    clickNewSolution g a
    page (SubmissionData s)
    -- TODO Check if the solution appears on the list

studentCommentsOnSolution :: String -> LoginData -> String -> String -> Int -> String -> TWD ()
studentCommentsOnSolution url student g a no comment = do
  loginUser url student $ do
    clickOnAssignment g a
    page (SubmissionListData no)
    page (CommentData comment)
    checkCommentOnPage (CommentData comment)

-- * Navigation

selectPageFromMenu :: Page -> TWD ()
selectPageFromMenu p = do
  failsOnFalse (join [show p," is not found in the menu"]) $ doesElementExist p
  click <@> p

selectPageLink :: Page -> TWD ()
selectPageLink = selectPageFromMenu


-- * Pagelets

