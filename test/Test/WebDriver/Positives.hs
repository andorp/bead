module Test.WebDriver.Positives where

import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.UserStories

import Bead.Domain.Entities
import Bead.Domain.Evaulation as E

admin       = LoginData "a" "a"
courseAdmin = LoginData "c" "cccc"
groupAdmin  = LoginData "g" "gggg"
student     = LoginData "s" "ssss"

positives :: String -> [TestCase (Test ())]
positives url = [
    mkTestCase "Student registration"     $ registration url (RegistrationData "s" "ssss" "s@s.com" "s")
  , mkTestCase "Group admin registration" $ registration url (RegistrationData "g" "gggg" "g@g.com" "g")
  , mkTestCase "Course admin registration" $ registration url (RegistrationData "c" "cccc" "c@c.com" "c")
  , mkTestCase "Invalid login" $ invalidLogin url (LoginData "s" "e")
  , mkTestCase "Create course admin" $ changeUserRole url admin courseAdmin CourseAdmin
  , mkTestCase "Group course admin"  $ changeUserRole url admin groupAdmin Professor
  , mkTestCase "Create course" $ adminCreatesCourse url admin (CourseData "ct-01" "desc" EvalBinary)
  , mkTestCase "Assign course admin" $ assignCourseAdmin  url admin "c" "ct-01"
  , mkTestCase "Group creation" $ cAdminCreateGroup  url courseAdmin (GroupData "ct-01" "g01" "g01" EvalBinary)
  , mkTestCase "Assign group admin" $ cAdminAssignGroupAdmin url courseAdmin groupAdmin "g01"
  , mkTestCase "Student registers to group" $ groupRegistration url student "g01"
  , mkTestCase "Create group assignment" $ gAdminCreatesAssignment url groupAdmin student (AssignmentData {
        aType = GroupAsg
      , aGroupOrCourse = "g01"
      , aName = "Assignment-01"
      , aDesc = "Assignment-01-desc"
      , aTCs  = "Assignment-01-test"
      , asgType = Normal
      })
  , mkTestCase "Student submits solution" $ studentSubmitsSolution url student "g01" "Assignment-01" "solution"
  , mkTestCase "Student comments on solution" $ studentCommentsOnSolution url student "g01" "Assignment-01" 0 "comment"
  , mkTestCase "Group admin evaulates solution" $ gAdminEvaulateSubmission
      url
      groupAdmin
      (SelectSubmissionData {sGroup = "g01", sStudent = "s", sNo = 0 })
      0
      (EvaulationData { evMessage = "Good", evValue = BinEval (Binary E.Passed)})
  , mkTestCase "Course admin creates assignment" $ cAdminCreatesAssignment url courseAdmin student (AssignmentData {
        aType = CourseAsg
      , aGroupOrCourse = "ct-01"
      , aName = "Assignment-02"
      , aDesc = "Assignment-02-desc"
      , aTCs  = "Assignment-02-test"
      , asgType = Normal
      })
  , mkTestCase "Student submits course solution" $ studentSubmitsSolution url student "ct-01" "Assignment-02" "solution2"
  , mkTestCase "Student comments on course solution" $ studentCommentsOnSolution url student "ct-01" "Assignment-02" 0 "comment2"
  , mkTestCase "Course admin evaulates solution " $ cAdminEvaulateSubmission
      url
      courseAdmin
      (SelectSubmissionData {sGroup = "ct-01", sStudent = "s", sNo = 0 })
      0
      (EvaulationData { evMessage = "Good", evValue = BinEval (Binary E.Passed)})
  ]

