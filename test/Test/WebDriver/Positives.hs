module Test.WebDriver.Positives where

import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.UserStories

import Bead.Domain.Entities

positives :: String -> [TWD ()]
positives url = [
    registration url (RegistrationData "s1" "s1" "s1" "s1")
  , registration url (RegistrationData "g1" "g1" "g1" "g1")
  , registration url (RegistrationData "c1" "c1" "c1" "c1")
  , invalidLogin url (LoginData "s1" "e")
  , changeUserRole url (LoginData "a" "a") (LoginData "c1" "c1") CourseAdmin
  , changeUserRole url (LoginData "a" "a") (LoginData "g1" "g1") Professor
  , adminCreatesCourse url (LoginData "a" "a") (CourseData "ct-01" "desc")
  , assignCourseAdmin  url (LoginData "a" "a") "c1" "ct-01"
  , cAdminCreateGroup  url (LoginData "c1" "c1") (GroupData "ct-01" "g01" "g01")
  , cAdminAssignGroupAdmin url (LoginData "c1" "c1") "g1" "g01"
  , gAdminCreatesAssignment url (LoginData "g1" "g1") (AssignmentData {
        aType = GroupAsg
      , aGroupOrCourse = "g01"
      , aName = "Assignment-01"
      , aDesc = "Assignment-01-desc"
      , aTCs  = "Assignment-01-test"
      , aEv = Scale
      , asgType = Normal
      , aStartDate = "2013-05-01 12:00:00"
      , aEndDate = "2013-05-02 12:00:00"
      })
  ]

