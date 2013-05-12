module Test.WebDriver.Positives where

import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.UserStories

import Bead.Domain.Entities

admin       = LoginData "a" "a"
courseAdmin = LoginData "c1" "c1"
groupAdmin  = LoginData "g1" "g1"
student     = LoginData "s1" "s1"

positives :: String -> [TestCase (TWD ())]
positives url = [
    mkTestCase "Student registration"     $ registration url (RegistrationData "s1" "s1" "s1" "s1")
  , mkTestCase "Group admin registration" $ registration url (RegistrationData "g1" "g1" "g1" "g1")
  , mkTestCase "Course admin registration" $ registration url (RegistrationData "c1" "c1" "c1" "c1")
  , mkTestCase "Invalid login" $ invalidLogin url (LoginData "s1" "e")
  , mkTestCase "Create course admin" $ changeUserRole url admin courseAdmin CourseAdmin
  , mkTestCase "Group course admin"  $ changeUserRole url admin groupAdmin Professor
  , mkTestCase "Create course" $ adminCreatesCourse url admin (CourseData "ct-01" "desc" Scale)
  , mkTestCase "Assign course admin" $ assignCourseAdmin  url admin "c1" "ct-01"
  , mkTestCase "Group creating" $ cAdminCreateGroup  url courseAdmin (GroupData "ct-01" "g01" "g01" Scale)
  , mkTestCase "Assign group admin" $ cAdminAssignGroupAdmin url courseAdmin groupAdmin "g01"
  , mkTestCase "Student registers to group" $ groupRegistration url student "g01"
  , mkTestCase "Create group assignment" $ gAdminCreatesAssignment url groupAdmin student (AssignmentData {
        aType = GroupAsg
      , aGroupOrCourse = "g01"
      , aName = "Assignment-01"
      , aDesc = "Assignment-01-desc"
      , aTCs  = "Assignment-01-test"
      , asgType = Normal
      , aStartDate = "2013-05-01 12:00:00"
      , aEndDate = "2013-05-02 12:00:00"
      })
  , mkTestCase "Student submits solution" $ studentSubmitsSolution url student "g01" "Assignment-01" "solution"
  , mkTestCase "Student comments on solution" $ studentCommentsOnSolution url student "g01" "Assignment-01" 0 "comment"
  , mkTestCase "Group admin evaulates solution" $ gAdminEvaulateSubmission
      url
      groupAdmin
      (SelectSubmissionData {sGroup = "g01", sStudent = "s1", sNo = 0 })
      0
      (EvaulationData { evMessage = "Good", evValue = "Passed 100"})
  , mkTestCase "Course admin creates assignment" $ cAdminCreatesAssignment url courseAdmin student (AssignmentData {
        aType = CourseAsg
      , aGroupOrCourse = "ct-01"
      , aName = "Assignment-02"
      , aDesc = "Assignment-02-desc"
      , aTCs  = "Assignment-02-test"
      , asgType = Normal
      , aStartDate = "2013-05-01 12:00:00"
      , aEndDate = "2013-05-02 12:00:00"
      })
  , mkTestCase "Student submits course solution" $ studentSubmitsSolution url student "ct-01" "Assignment-02" "solution2"
  , mkTestCase "Student comments on course solution" $ studentCommentsOnSolution url student "ct-01" "Assignment-02" 0 "comment2"
  , mkTestCase "Course admin evaulates solution " $ cAdminEvaulateSubmission
      url
      courseAdmin
      (SelectSubmissionData {sGroup = "ct-01", sStudent = "s1", sNo = 0 })
      0
      (EvaulationData { evMessage = "Good", evValue = "Passed 100"})
  ]

