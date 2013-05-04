module Test.WebDriver.Positives where

import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.UserStories

import Bead.Domain.Entities

admin       = LoginData "a" "a"
courseAdmin = LoginData "c1" "c1"
groupAdmin  = LoginData "g1" "g1"
student     = LoginData "s1" "s1"

positives :: String -> [TWD ()]
positives url = [
    registration url (RegistrationData "s1" "s1" "s1" "s1")
  , registration url (RegistrationData "g1" "g1" "g1" "g1")
  , registration url (RegistrationData "c1" "c1" "c1" "c1")
  , invalidLogin url (LoginData "s1" "e")
  , changeUserRole url admin courseAdmin CourseAdmin
  , changeUserRole url admin groupAdmin Professor
  , adminCreatesCourse url admin (CourseData "ct-01" "desc")
  , assignCourseAdmin  url admin "c1" "ct-01"
  , cAdminCreateGroup  url courseAdmin (GroupData "ct-01" "g01" "g01")
  , cAdminAssignGroupAdmin url courseAdmin groupAdmin "g01"
  , groupRegistration url student "g01"
  , gAdminCreatesAssignment url groupAdmin student (AssignmentData {
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
  , studentSubmitsSolution url student "g01" "Assignment-01" "solution"
  , studentCommentsOnSolution url student "g01" "Assignment-01" 0 "comment"
  , gAdminEvaulateSubmission
      url
      groupAdmin
      (SelectSubmissionData {sGroup = "g01", sStudent = "s1", sNo = 0 })
      0
      (EvaulationData { evMessage = "Good", evValue = "Passed 100"})
  ]

