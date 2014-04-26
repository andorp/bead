module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

-- Bead imports

import Bead.Domain.Entities
import Bead.Domain.Shared.Evaluation
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.Relations
import Control.Monad.Transaction.TIO

-- Utils

import Data.Time.Clock
import Data.Maybe
import System.Directory
import Control.Monad (join)

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_exercise
  , test_create_load_exercise
  , test_create_user
  , test_create_group_user
  , testUserRegSaveAndLoad
  , testOpenSubmissions
  , clean_up
  ]

test_initialize_persistence = testCase "Initialize NoSQLDir persistence layer" $ do
  setUp <- isPersistenceSetUp
  assertBool "Persistence was set up" (not setUp)
  initPersistence
  setUp <- isPersistenceSetUp
  assertBool "Settin up persistence was failed" setUp

test_create_exercise = testCase "Save an exercise" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  let assignment = Assignment "Title" "This is an exercise" Normal str UTC end UTC
  ek <- liftE $ saveAssignment assignment
  let uname = Username "student"
      user = User {
        u_role     = Student
      , u_username = uname
      , u_email    = Email "student@gmail.com"
      , u_name     = "Student"
      , u_timezone = UTC
      , u_language = Language "hu"
      }
      password = "password"
  liftE $ saveUser user
  let s = Submission {
        solution = "Solution"
      , solutionPostDate = end
      }
  sk <- liftE $ saveSubmission ek uname s
  return ()

test_create_load_exercise = testCase "Create and load exercise" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  k <- liftE $ saveAssignment (Assignment "Title" "This is an exercise" Normal str UTC end UTC)
  ks <- liftE $ filterAssignment (\_ _ -> True)
  assertBool "Readed list of exercises was empty" (length ks > 0)
  assertBool "Written key was not in the list" (elem k (map fst ks))

test_create_user = testCase "Create user" $ do
  let uname = Username "ursula"
  let user = User {
        u_role     = Student
      , u_username = uname
      , u_email    = Email "ursula@gmail.com"
      , u_name     = "Ursula"
      , u_timezone = UTC
      , u_language = Language "hu"
      }
  liftE $ saveUser user
  us <- liftE $ filterUsers (const True)
  assertBool "The filter did not find the user" (length us > 0)
  user1 <- liftE $ loadUser uname
  assertBool "Loading the registered user has failed" (user1 == user)
  let user2 = user { u_role = CourseAdmin }
  liftE $ updateUser user2
  user3 <- liftE $ loadUser uname
  assertBool "Updating and loading user has failed" (user3 == user2)

testUserRegSaveAndLoad = testCase "Save and Load User regisistration" $ do
  now <- getCurrentTime
  let u = UserRegistration "username" "e@e.com" "Family name" "token" now
  key <- liftE $ saveUserReg u
  u'  <- liftE $ loadUserReg key
  assertBool "Loaded user registration info differs from saved" (u == u')

testOpenSubmissions = testCase "Users separated correctly in open submission tables" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  reinitpersistence
  let myStudent = Username "mystudent"
      myStudentUser = User {
          u_role = Student
        , u_username = myStudent
        , u_email = Email "admin@gmail.com"
        , u_name = "mystudent"
        , u_timezone = UTC
        , u_language = Language "hu"
        }
      otherStudent = Username "otherstudent"
      otherStudentUser = User {
          u_role = Student
        , u_username = otherStudent
        , u_email = Email "admin@gmail.com"
        , u_name = "otherstudent"
        , u_timezone = UTC
        , u_language = Language "hu"
        }
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        , u_timezone = UTC
        , u_language = Language "hu"
        }
      password = "password"
      cAssignment = Assignment "CourseAssignment" "Assignment" Urn str UTC end UTC
      gAssignment1 = Assignment "GroupAssignment" "Assignment" Normal str UTC end UTC
      gAssignment2 = Assignment "GroupAssignment" "Assignment" Normal str UTC end UTC
      sbsm = Submission "submission" str
  join $ liftE $ do
    ck  <- saveCourse (Course "name" "desc" binaryEvalConfig TestScriptSimple)
    gk1 <- saveGroup ck (Group "gname1" "gdesc1" binaryEvalConfig)
    gk2 <- saveGroup ck (Group "gname2" "gdesc2" binaryEvalConfig)
    saveUser adminUser
    saveUser myStudentUser
    saveUser otherStudentUser
    subscribe myStudent ck gk1
    subscribe otherStudent ck gk2
    createCourseAdmin admin ck
    createGroupAdmin admin gk1
    cak <- saveCourseAssignment ck cAssignment
    gak1 <- saveGroupAssignment gk1 gAssignment1
    gak2 <- saveGroupAssignment gk2 gAssignment2
    sk1 <- saveSubmission cak myStudent sbsm
    sk2 <- saveSubmission gak1 myStudent sbsm
    sk3 <- saveSubmission cak otherStudent sbsm
    os <- openedSubmissionInfo admin
    return $ do
      let adminedCourse = map fst $ osAdminedCourse os
          adminedGroup  = map fst $ osAdminedGroup os
          relatedCourse = map fst $ osRelatedCourse os
      assertBool
        (join ["Course level assignment for administrated group were incorrent:", show adminedCourse])
        ([sk1] == adminedCourse)
      assertBool
        (join ["Group level assignment for administrated group were incorrent:", show adminedGroup])
        ([sk2] == adminedGroup)
      assertBool
        (join ["Course level assignment for non-administrated group were incorrent:", show relatedCourse])
        ([sk3] == relatedCourse)


test_create_group_user = testCase "Create Course and Group with a user" $ do
  let username = Username "ursula"
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        , u_timezone = UTC
        , u_language = Language "hu"
        }
      password = "password"
  ck <- liftE $ saveCourse (Course "name" "desc" binaryEvalConfig TestScriptSimple)
  gk <- liftE $ saveGroup ck (Group "gname" "gdesc" binaryEvalConfig)
  gks <- liftE $ groupKeysOfCourse ck
  assertBool "Registered group was not found in the group list" (elem gk gks)
  liftE $ subscribe username ck gk
  rCks <- liftE $ userCourses username
  assertBool "Course does not found in user's courses" (rCks == [ck])
  rGks <- liftE $ userGroups username
  assertBool "Group does not found in user's groups" (rGks == [gk])
  isInGroup <- liftE $ isUserInGroup username gk
  assertBool "Registered user is not found" isInGroup
  isInCourse <- liftE $ isUserInCourse username ck
  assertBool "Registered user is not found" isInCourse
  liftE $ saveUser adminUser
  liftE $ createCourseAdmin admin ck
  cs <- liftE $ administratedCourses admin
  assertBool "Course is not found in administrated courses" (elem ck (map fst cs))
  liftE $ createGroupAdmin admin gk
  gs <- liftE $ administratedGroups admin
  assertBool "Group is not found in administrated groups" (elem gk (map fst gs))
  str <- getCurrentTime
  end <- getCurrentTime
  let gAssignment = Assignment "GroupAssignment" "Assignment" Normal str UTC end UTC
      cAssignment = Assignment "CourseAssignment" "Assignment" Urn str UTC end UTC
  cak <- liftE $ saveCourseAssignment ck cAssignment
  cask <- liftE $ courseAssignments ck
  assertBool "Course does not have the assignment" (elem cak cask)
  gak <- liftE $ saveGroupAssignment gk gAssignment
  gask <- liftE $ groupAssignments gk
  assertBool "Group does not have the assignment" (elem gak gask)
  us <- liftE $ groupAdmins gk
  assertBool "Admin is not in the group" ([admin] == us)
  gs <- liftE $ filterGroups (\_ _ -> True)
  assertBool "Group list was different" ([gk] == map fst gs)

  testHasNoLastSubmission gak username

  -- Submission
  let sbsm = Submission "submission" str
  sk <- liftE $ saveSubmission gak username sbsm
  sk_user <- liftE $ usernameOfSubmission sk
  assertBool
    (join ["Username of the submission differs from the registered: (", show username, " ", show sk_user, ")"])
    (username == sk_user)
  sk_ak <- liftE $ assignmentOfSubmission sk
  assertBool "Assignment differs from registered" (gak == sk_ak)
  osk <- liftE $ openedSubmissions
  assertBool "Submission is not in the opened submissions" (elem sk osk)

  testHasLastSubmission gak username sk

  -- Test Submissions
  submissions <- liftE $ submissionsForAssignment gak
  assertBool "Submissions for assignment was different" (submissions == [sk])

  uss <- liftE $ userSubmissions username gak
  assertBool "Submission is not in the users' submission" (elem sk uss)

  let ev = Evaluation (BinEval (Binary Passed)) "Good"
  evKey <- liftE $ saveEvaluation sk ev
  ev1 <- liftE $ loadEvaluation evKey
  assertBool "Evaluation was not loaded correctly" (ev == ev1)
  ev_sk <- liftE $ submissionOfEvaluation evKey
  assertBool "Submission key was different for the evaluation" (sk == ev_sk)
  liftE $ removeFromOpened gak username sk

  testComment sk

  sld <- liftE $ submissionListDesc username gak
  assertBool (concat ["Group name was different: '", slGroup sld, "' 'name - gname'"]) (slGroup sld == "name - gname")
  assertBool "Admins was different" (slTeacher sld == ["admin"])
--  assertBool "There was different number od submissions" (length (slSubmissions sld) == 1)
  assertBool "Assignment text was different" ((assignmentDesc $ slAssignment sld) == "Assignment")

  return ()

testComment :: SubmissionKey -> IO ()
testComment sk = do
  now <- getCurrentTime
  let comment = Comment "comment" "author" now CT_Student
  key <- liftE $ saveComment sk comment
  c2  <- liftE $ loadComment key
  assertBool "Loaded comment was different" (comment == c2)
  sk2 <- liftE $ submissionOfComment key
  assertBool "Submission key was different" (sk == sk2)

testHasNoLastSubmission :: AssignmentKey -> Username -> IO ()
testHasNoLastSubmission ak u = do
  mKey <- liftE $ lastSubmission ak u
  assertBool "Found submission" (isNothing mKey)

testHasLastSubmission :: AssignmentKey -> Username -> SubmissionKey -> IO ()
testHasLastSubmission ak u sk = do
  mKey <- liftE $ lastSubmission ak u
  assertBool "Submission was not found" (isJust mKey)
  assertBool "Submission was different" (sk == fromJust mKey)

reinitpersistence = do
  removeDirectoryRecursive "data"
  initPersistence

clean_up = testCase "Cleaning up" $ do
  -- We use background knowledge, to clean up
  removeDirectoryRecursive "data"
  return ()

-- * Tools

liftE :: TIO a -> IO a
liftE m = do
  x <- runPersist m
  case x of
    Left e -> error e
    Right y -> return y
