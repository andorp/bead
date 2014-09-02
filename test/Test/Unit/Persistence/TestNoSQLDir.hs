module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

-- Bead imports

import Bead.Domain.Entities
import Bead.Domain.TimeZone (utcZoneInfo)
import Bead.Domain.Shared.Evaluation
import Bead.Domain.Relationships
import Bead.Persistence.Initialization
import Bead.Persistence.Persist
import Bead.Persistence.Relations

-- Utils

import Control.Monad (join, when)
import Data.Maybe
import Data.Time.Clock
import System.Directory
import System.FilePath

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_load_exercise
  , test_create_user
  , test_create_group_user
  , testUserRegSaveAndLoad
  , testOpenSubmissions
  , test_feedbacks
  , clean_up
  ]

-- Normal assignment is represented as empty aspects set
normal = emptyAspects

ballot = aspectsFromList [BallotBox]

test_initialize_persistence = testCase "Initialize NoSQLDir persistence layer" $ do
  init <- createPersistInit defaultConfig
  setUp <- isSetUp init
  assertBool "Persistence was set up" (not setUp)
  initPersist init
  setUp <- isSetUp init
  assertBool "Settin up persistence was failed" setUp

test_feedbacks = testCase "Create and delete test feedbacks" $ do
  -- Given
  interp <- createPersistInterpreter defaultConfig
  let skey = "s2013"
      skey' = SubmissionKey skey
      sdir = testIncomingDataDir </> skey
      publicMsg = "Public Message"
      privateMsg = "Private Message"
      result = "True"
  createDirectory sdir
  writeFile (sdir </> "private") privateMsg
  writeFile (sdir </> "public")  publicMsg
  writeFile (sdir </> "result")  result
  -- When
  rs <- liftE interp $ testFeedbacks
  -- Then
  assertEqual "Wrong values"
    [ (skey', MessageForAdmin privateMsg)
    , (skey', MessageForStudent publicMsg)
    , (skey', TestResult True)
    ]
    (map (\(sk,f) -> (sk, info f)) rs)

test_create_load_exercise = testCase "Create and load exercise" $ do
  interp <- createPersistInterpreter defaultConfig
  str <- getCurrentTime
  end <- getCurrentTime
  let a = Assignment "Title" "This is an exercise" normal str end binaryConfig
  k <- liftE interp $ saveAssignment a
  a' <- liftE interp $ loadAssignment k
  assertBool "The saved assignment differs from the read one." (a' == a)

test_create_user = testCase "Create user" $ do
  interp <- createPersistInterpreter defaultConfig
  let uname = Username "ursula"
  let user = User {
        u_role     = Student
      , u_username = uname
      , u_email    = Email "ursula@gmail.com"
      , u_name     = "Ursula"
      , u_timezone = utcZoneInfo
      , u_language = Language "hu"
      }
  liftE interp $ saveUser user
  us <- liftE interp $ filterUsers (const True)
  assertBool "The filter did not find the user" (length us > 0)
  user1 <- liftE interp $ loadUser uname
  assertBool "Loading the registered user has failed" (user1 == user)
  let user2 = user { u_role = CourseAdmin }
  liftE interp $ updateUser user2
  user3 <- liftE interp $ loadUser uname
  assertBool "Updating and loading user has failed" (user3 == user2)

testUserRegSaveAndLoad = testCase "Save and Load User regisistration" $ do
  interp <- createPersistInterpreter defaultConfig
  now <- getCurrentTime
  let u = UserRegistration "username" "e@e.com" "Family name" "token" now
  key <- liftE interp $ saveUserReg u
  u'  <- liftE interp $ loadUserReg key
  assertBool "Loaded user registration info differs from saved" (u == u')

testOpenSubmissions = testCase "Users separated correctly in open submission tables" $ do
  interp <- createPersistInterpreter defaultConfig
  str <- getCurrentTime
  end <- getCurrentTime
  reinitpersistence
  let myStudent = Username "mystudent"
      myStudentUser = User {
          u_role = Student
        , u_username = myStudent
        , u_email = Email "admin@gmail.com"
        , u_name = "mystudent"
        , u_timezone = utcZoneInfo
        , u_language = Language "hu"
        }
      otherStudent = Username "otherstudent"
      otherStudentUser = User {
          u_role = Student
        , u_username = otherStudent
        , u_email = Email "admin@gmail.com"
        , u_name = "otherstudent"
        , u_timezone = utcZoneInfo
        , u_language = Language "hu"
        }
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        , u_timezone = utcZoneInfo
        , u_language = Language "hu"
        }
      password = "password"
      cAssignment = Assignment "CourseAssignment" "Assignment" ballot str end binaryConfig
      gAssignment1 = Assignment "GroupAssignment" "Assignment" normal str end binaryConfig
      gAssignment2 = Assignment "GroupAssignment" "Assignment" normal str end binaryConfig
      sbsm = Submission (SimpleSubmission "submission") str
  join $ liftE interp $ do
    ck  <- saveCourse (Course "name" "desc" TestScriptSimple)
    gk1 <- saveGroup ck (Group "gname1" "gdesc1")
    gk2 <- saveGroup ck (Group "gname2" "gdesc2")
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
  interp <- createPersistInterpreter defaultConfig
  let username = Username "ursula"
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        , u_timezone = utcZoneInfo
        , u_language = Language "hu"
        }
      password = "password"
  ck <- liftE interp $ saveCourse (Course "name" "desc" TestScriptSimple)
  gk <- liftE interp $ saveGroup ck (Group "gname" "gdesc")
  gks <- liftE interp $ groupKeysOfCourse ck
  assertBool "Registered group was not found in the group list" (elem gk gks)
  liftE interp $ subscribe username ck gk
  rCks <- liftE interp $ userCourses username
  assertBool "Course does not found in user's courses" (rCks == [ck])
  rGks <- liftE interp $ userGroups username
  assertBool "Group does not found in user's groups" (rGks == [gk])
  isInGroup <- liftE interp $ isUserInGroup username gk
  assertBool "Registered user is not found" isInGroup
  isInCourse <- liftE interp $ isUserInCourse username ck
  assertBool "Registered user is not found" isInCourse
  liftE interp $ saveUser adminUser
  liftE interp $ createCourseAdmin admin ck
  cs <- liftE interp $ administratedCourses admin
  assertBool "Course is not found in administrated courses" (elem ck (map fst cs))
  liftE interp $ createGroupAdmin admin gk
  gs <- liftE interp $ administratedGroups admin
  assertBool "Group is not found in administrated groups" (elem gk (map fst gs))
  str <- getCurrentTime
  end <- getCurrentTime
  let gAssignment = Assignment "GroupAssignment" "Assignment" normal str end binaryConfig
      cAssignment = Assignment "CourseAssignment" "Assignment" ballot str end (percentageConfig 0.1)
  cak <- liftE interp $ saveCourseAssignment ck cAssignment
  cask <- liftE interp $ courseAssignments ck
  assertBool "Course does not have the assignment" (elem cak cask)
  gak <- liftE interp $ saveGroupAssignment gk gAssignment
  gask <- liftE interp $ groupAssignments gk
  assertBool "Group does not have the assignment" (elem gak gask)
  us <- liftE interp $ groupAdmins gk
  assertBool "Admin is not in the group" ([admin] == us)
  gs <- liftE interp $ filterGroups (\_ _ -> True)
  assertBool "Group list was different" ([gk] == map fst gs)

  testHasNoLastSubmission gak username

  -- Submission
  let sbsm = Submission (SimpleSubmission "submission") str
  sk <- liftE interp $ saveSubmission gak username sbsm
  sk_user <- liftE interp $ usernameOfSubmission sk
  assertBool
    (join ["Username of the submission differs from the registered: (", show username, " ", show sk_user, ")"])
    (username == sk_user)
  sk_ak <- liftE interp $ assignmentOfSubmission sk
  assertBool "Assignment differs from registered" (gak == sk_ak)
  osk <- liftE interp $ openedSubmissions
  assertBool "Submission is not in the opened submissions" (elem sk osk)

  testHasLastSubmission gak username sk

  -- Test Submissions
  submissions <- liftE interp $ submissionsForAssignment gak
  assertBool "Submissions for assignment was different" (submissions == [sk])

  uss <- liftE interp $ userSubmissions username gak
  assertBool "Submission is not in the users' submission" (elem sk uss)

  let ev = Evaluation (binaryResult Passed) "Good"
  evKey <- liftE interp $ saveSubmissionEvaluation sk ev
  ev1 <- liftE interp $ loadEvaluation evKey
  assertBool "Evaluation was not loaded correctly" (ev == ev1)
  ev_sk <- liftE interp $ submissionOfEvaluation evKey
  assertBool "Submission key was different for the evaluation" (Just sk == ev_sk)
  liftE interp $ removeFromOpened gak username sk

  testComment sk

  sld <- liftE interp $ submissionListDesc username gak
  assertBool (concat ["Group name was different: '", slGroup sld, "' 'name - gname'"]) (slGroup sld == "name - gname")
  assertBool "Admins was different" (slTeacher sld == ["admin"])
--  assertBool "There was different number od submissions" (length (slSubmissions sld) == 1)
  assertBool "Assignment text was different" ((desc $ slAssignment sld) == "Assignment")

  return ()

testComment :: SubmissionKey -> IO ()
testComment sk = do
  interp <- createPersistInterpreter defaultConfig
  now <- getCurrentTime
  let comment = Comment "comment" "author" now CT_Student
  key <- liftE interp $ saveComment sk comment
  c2  <- liftE interp $ loadComment key
  assertBool "Loaded comment was different" (comment == c2)
  sk2 <- liftE interp $ submissionOfComment key
  assertBool "Submission key was different" (sk == sk2)

testHasNoLastSubmission :: AssignmentKey -> Username -> IO ()
testHasNoLastSubmission ak u = do
  interp <- createPersistInterpreter defaultConfig
  mKey <- liftE interp $ lastSubmission ak u
  assertBool "Found submission" (isNothing mKey)

testHasLastSubmission :: AssignmentKey -> Username -> SubmissionKey -> IO ()
testHasLastSubmission ak u sk = do
  interp <- createPersistInterpreter defaultConfig
  mKey <- liftE interp $ lastSubmission ak u
  assertBool "Submission was not found" (isJust mKey)
  assertBool "Submission was different" (sk == fromJust mKey)

reinitpersistence = do
  init <- createPersistInit defaultConfig
  setUp <- isSetUp init
  when setUp $ do
    tearDown init
    initPersist init

clean_up = testCase "Cleaning up" $ do
  init <- createPersistInit defaultConfig
  tearDown init

-- * Tools

liftE interp m = do
  x <- runPersist interp m
  case x of
    Left e -> error e
    Right y -> return y
