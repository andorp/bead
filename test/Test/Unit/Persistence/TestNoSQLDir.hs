module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Persistence.Persist

-- Bead imports

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Shared.Evaluation
import Bead.Domain.Evaluation
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDir
import Bead.Persistence.NoSQL.Loader
import Control.Monad.Transaction.TIO

-- Utils

import Data.Time.Clock
import Data.Maybe
import System.Directory
import Control.Monad (join, liftM)

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_exercise
  , test_create_load_exercise
  , test_create_user
  , test_create_group_user
  , testUserRegSaveAndLoad
  , clean_up
  ]

persist = testDecoratedPersist (hasNoRollback . assert) noSqlDirPersist

test_initialize_persistence = testCase "Initialize NoSQLDir persistence layer" $ do
--  removeDirectoryRecursive "data"
  setUp <- isPersistenceSetUp persist
  assertBool "Persistence was set up" (not setUp)
  initPersistence persist
  setUp <- isPersistenceSetUp persist
  assertBool "Settin up persistence was failed" setUp

test_create_exercise = testCase "Save an exercise" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  let assignment = Assignment "Title" "This is an exercise" Normal str UTC end UTC
  ek <- liftE $ saveAssignment persist assignment
  let uname = Username "student"
      user = User {
        u_role     = Student
      , u_username = uname
      , u_email    = Email "student@gmail.com"
      , u_name     = "Student"
      , u_timezone = UTC
      }
      password = "password"
  liftE $ saveUser persist user
  let s = Submission {
        solution = "Solution"
      , solutionPostDate = end
      }
  sk <- liftE $ saveSubmission persist ek uname s
  return ()

test_create_load_exercise = testCase "Create and load exercise" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  k <- liftE $ saveAssignment persist (Assignment "Title" "This is an exercise" Normal str UTC end UTC)
  ks <- liftE $ filterAssignment persist (\_ _ -> True)
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
      }
  liftE $ saveUser persist user
  us <- liftE $ filterUsers persist (const True)
  assertBool "The filter did not find the user" (length us > 0)
  user1 <- liftE $ loadUser persist uname
  assertBool "Loading the registered user has failed" (user1 == user)
  let user2 = user { u_role = CourseAdmin }
  liftE $ updateUser persist user2
  user3 <- liftE $ loadUser persist uname
  assertBool "Updating and loading user has failed" (user3 == user2)

testUserRegSaveAndLoad = testCase "Save and Load User regisistration" $ do
  now <- getCurrentTime
  let u = UserRegistration "username" "e@e.com" "Family name" "token" now
  key <- liftE $ saveUserReg persist u
  u'  <- liftE $ loadUserReg persist key
  assertBool "Loaded user registration info differs from saved" (u == u')

test_create_group_user = testCase "Create Course and Group with a user" $ do
  let username = Username "ursula"
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        , u_timezone = UTC
        }
      password = "password"
  ck <- liftE $ saveCourse persist (Course "name" "desc" binaryEvalConfig)
  gk <- liftE $ saveGroup persist ck (Group "gname" "gdesc" binaryEvalConfig)
  gks <- liftE $ groupKeysOfCourse persist ck
  assertBool "Registered group was not found in the group list" (elem gk gks)
  liftE $ subscribe persist username ck gk
  rCks <- liftE $ userCourses persist username
  assertBool "Course does not found in user's courses" (rCks == [ck])
  rGks <- liftE $ userGroups persist username
  assertBool "Group does not found in user's groups" (rGks == [gk])
  isInGroup <- liftE $ isUserInGroup persist username gk
  assertBool "Registered user is not found" isInGroup
  isInCourse <- liftE $ isUserInCourse persist username ck
  assertBool "Registered user is not found" isInCourse
  liftE $ saveUser persist adminUser
  liftE $ createCourseAdmin persist admin ck
  cs <- liftE $ administratedCourses persist admin
  assertBool "Course is not found in administrated courses" (elem ck (map fst cs))
  liftE $ createGroupAdmin persist admin gk
  gs <- liftE $ administratedGroups persist admin
  assertBool "Group is not found in administrated groups" (elem gk (map fst gs))
  str <- getCurrentTime
  end <- getCurrentTime
  let gAssignment = Assignment "GroupAssignment" "Assignment" Normal str UTC end UTC
      cAssignment = Assignment "CourseAssignment" "Assignment" Urn str UTC end UTC
  cak <- liftE $ saveCourseAssignment persist ck cAssignment
  cask <- liftE $ courseAssignments persist ck
  assertBool "Course does not have the assignment" (elem cak cask)
  gak <- liftE $ saveGroupAssignment persist gk gAssignment
  gask <- liftE $ groupAssignments persist gk
  assertBool "Group does not have the assignment" (elem gak gask)
  us <- liftE $ groupAdmins persist gk
  assertBool "Admin is not in the group" ([admin] == us)
  gs <- liftE $ filterGroups persist (\_ _ -> True)
  assertBool "Group list was different" ([gk] == map fst gs)

  testHasNoLastSubmission gak username

  -- Submission
  let sbsm = Submission "submission" str
  sk <- liftE $ saveSubmission persist gak username sbsm
  sk_user <- liftE $ usernameOfSubmission persist sk
  assertBool
    (join ["Username of the submission differs from the registered: (", show username, " ", show sk_user, ")"])
    (username == sk_user)
  sk_ak <- liftE $ assignmentOfSubmission persist sk
  assertBool "Assignment differs from registered" (gak == sk_ak)
  osk <- liftE $ openedSubmissions persist
  assertBool "Submission is not in the opened submissions" (elem sk osk)

  testHasLastSubmission gak username sk

  -- Test Submissions
  submissions <- liftE $ submissionsForAssignment persist gak
  assertBool "Submissions for assignment was different" (submissions == [sk])

  uss <- liftE $ userSubmissions persist username gak
  assertBool "Submission is not in the users' submission" (elem sk uss)

  let ev = Evaluation (BinEval (Binary Passed)) "Good"
  evKey <- liftE $ saveEvaluation persist sk ev
  ev1 <- liftE $ loadEvaluation persist evKey
  assertBool "Evaluation was not loaded correctly" (ev == ev1)
  ev_sk <- liftE $ submissionOfEvaluation persist evKey
  assertBool "Submission key was different for the evaluation" (sk == ev_sk)
  liftE $ removeFromOpened persist sk

  testComment sk

  sld <- liftE $ submissionListDesc persist username gak
  assertBool "Group name was different" (slGroup sld == "gname")
  assertBool "Admins was different" (slTeacher sld == ["admin"])
--  assertBool "There was different number od submissions" (length (slSubmissions sld) == 1)
  assertBool "Assignment text was different" ((assignmentDesc $ slAssignment sld) == "Assignment")

  return ()

testComment :: SubmissionKey -> IO ()
testComment sk = do
  now <- getCurrentTime
  let comment = Comment "comment" now CT_Student
  key <- liftE $ saveComment persist sk comment
  c2  <- liftE $ loadComment persist key
  assertBool "Loaded comment was different" (comment == c2)
  sk2 <- liftE $ submissionOfComment persist key
  assertBool "Submission key was different" (sk == sk2)

testHasNoLastSubmission :: AssignmentKey -> Username -> IO ()
testHasNoLastSubmission ak u = do
  mKey <- liftE $ lastSubmission persist ak u
  assertBool "Found submission" (isNothing mKey)

testHasLastSubmission :: AssignmentKey -> Username -> SubmissionKey -> IO ()
testHasLastSubmission ak u sk = do
  mKey <- liftE $ lastSubmission persist ak u
  assertBool "Submission was not found" (isJust mKey)
  assertBool "Submission was different" (sk == fromJust mKey)


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
