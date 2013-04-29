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
  openPage url
  page l
  page . homePage $ l
  m
  page logout

registration :: String -> RegistrationData -> TWD ()
registration url reg = do
  openPage url
  click =<< findElem (ByLinkText "Create new user")
  page reg
  page . loginInfo $ reg
  page LogoutData

validLogin :: String -> LoginData -> TWD ()
validLogin url login = loginUser url login (return ())

invalidLogin :: String -> LoginData -> TWD ()
invalidLogin url loginData = do
  openPage url
  page loginData
  checkIfPageIs loginData

groupRegistration :: String -> LoginData -> GroupRegData -> TWD ()
groupRegistration url login group = do
  loginUser url login $ do
    selectPageFromMenu GroupRegistration
    page group
    isPage (parentPage GroupRegistration)

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

cAdminAssignGroupAdmin :: String -> LoginData -> String -> String -> TWD ()
cAdminAssignGroupAdmin url courseAdmin groupAdmin g = do
  loginUser url courseAdmin $ do
    selectPageFromMenu CourseAdmin
    onPage CourseAdminPage $ setGroupAdmin groupAdmin g

-- * Group Admin

gAdminCreatesAssignment :: String -> LoginData -> AssignmentData -> TWD ()
gAdminCreatesAssignment url groupAdmin aData = do
  loginUser url groupAdmin $ do
    selectPageLink NewGroupAssignment
    page aData
    selectPageFromMenu Home
    -- TODO Check the created assignment

-- * Navigation

selectPageFromMenu :: Page -> TWD ()
selectPageFromMenu p = do
  failsOnFalse (join [show p," is not found in the menu"]) $ doesElementExist p
  click <@> p

selectPageLink :: Page -> TWD ()
selectPageLink = selectPageFromMenu


-- * Pagelets

