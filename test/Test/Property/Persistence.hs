module Test.Property.Persistence (
    tests
  , createTestData
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

import Data.List ((\\), intersperse, nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import Data.Time
import System.Directory hiding (copyFile)
import System.IO
import System.IO.Temp (createTempDirectory)
import System.FilePath ((</>))

import Control.Monad.Transaction.TIO
import Bead.Persistence.Initialization
import Bead.Persistence.Persist
import Bead.Persistence.Relations
import Bead.Persistence.NoSQLDir (referredPath)
import Bead.Persistence.NoSQLDirFile

import qualified Test.Property.EntityGen as Gen

import Bead.Domain.Entities
import qualified Bead.Domain.Entity.Assignment as Assignment
import Bead.Domain.Entity.Comment
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.TestSet (add, group, test, ioTest)
import Test.Tasty.QuickCheck (testProperty)

import System.IO.Unsafe

{- Mass test of the persistence layer -}

-- Properties

-- Load and save property. The save and a load of
-- the given data should be the same.
saveAndLoadIdenpotent :: (Eq v, Show v) => String -> (v -> Persist k) -> (k -> Persist v) -> Gen v -> PropertyM IO k
saveAndLoadIdenpotent name save load gen = do
  v <- pick gen
  key <- runPersistCmd $ save v
  v'  <- runPersistCmd $ load key
  assertEquals v v' (name ++ ": Save and load is not idenpotent")
  return key

-- Modification property. The saved, modificated and a load
-- of the given data should be the same
modification :: (Eq v, Show v) => String -> (v -> Persist k) -> (k -> v -> Persist ()) -> (k -> Persist v) -> Gen v -> PropertyM IO k
modification name save modify load gen = do
  k <- saveAndLoadIdenpotent name save load gen
  v <- pick gen
  runPersistCmd $ modify k v
  v' <- runPersistCmd $ load k
  assertEquals v v' (name ++ ": Modifed and load was different")
  return k

assignmentSaveAndLoad = saveAndLoadIdenpotent
  "Assignment"
  (saveAssignment)
  (loadAssignment)
  (Gen.assignments startDate endDate)

courseSaveAndLoad = saveAndLoadIdenpotent
  "Course"
  (saveCourse)
  (loadCourse)
  Gen.courses

groupSaveAndLoad = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse)  (loadCourse) Gen.courses
  gk <- saveAndLoadIdenpotent "Group"  (saveGroup ck) (loadGroup) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse ck
  assertEquals [gk] gks "Group keys were different"
  ck' <- runPersistCmd $ courseOfGroup gk
  assertEquals ck ck' "Course keys were different"

courseAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse) (loadCourse) Gen.courses
  saveAndLoadIdenpotent
    "Assignment"
    (saveCourseAssignment key)
    (loadAssignment)
    (Gen.assignments startDate endDate)

groupAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse) (loadCourse) Gen.courses
  key1 <- saveAndLoadIdenpotent "Groups" (saveGroup key) (loadGroup) Gen.groups
  saveAndLoadIdenpotent
    "Assignment"
    (saveGroupAssignment key1)
    (loadAssignment)
    (Gen.assignments startDate endDate)

userSaveAndLoad u = do
  saveAndLoadIdenpotent "User"
    (\usr -> saveUser usr) (const (loadUser (u_username u))) (return u)

createOrLoadUser u = do
  exist <- runPersistCmd $ doesUserExist (u_username u)
  case exist of
    True  -> return ()
    False -> userSaveAndLoad u
  return u

multipleGroupsForCourse = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse) (loadCourse)  Gen.courses
  gk1 <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) Gen.groups
  gk2 <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse ck
  assertEquals (Set.fromList gks) (Set.fromList [gk1,gk2]) "Groups key set were different"

-- Tries to save and load a submission for a given user and assignment
saveAndLoadSubmissionFor u ak =
  saveAndLoadIdenpotent "Submission"
    (saveSubmission ak u) (loadSubmission) (Gen.submissions startDate)

saveAndLoadSubmission = do
  ak <- groupAssignmentSaveAndLoad
  u <- pick Gen.users
  createOrLoadUser u
  sk <- saveAndLoadIdenpotent "Submission"
          (saveSubmission ak (u_username u)) (loadSubmission) (Gen.submissions startDate)
  return (ak,u,sk)



assignmentAndUserOfSubmission = do
  (ak, u, sk) <- saveAndLoadSubmission
  ak' <- runPersistCmd $ assignmentOfSubmission sk
  assertEquals ak ak' "Assertion keys were different"
  un <- runPersistCmd $ usernameOfSubmission sk
  assertEquals (u_username u) un "Username were different"

saveAndLoadComment = do
  (ak, u, sk) <- saveAndLoadSubmission
  ck <- saveAndLoadIdenpotent "Comment" (saveComment sk) (loadComment) (Gen.comments startDate)
  sk' <- runPersistCmd $ submissionOfComment ck
  assertEquals sk sk' "Submission keys were different"

evaluationConfigForSubmission sk = do
  ak <- runPersistCmd $ assignmentOfSubmission sk
  Assignment.evType <$> (runPersistCmd $ loadAssignment ak)

evaluationGroupSaveAndLoad = do
  (ak, u, sk) <- saveAndLoadSubmission
  cfg <- evaluationConfigForSubmission sk
  saveAndLoadIdenpotent
    "Evaluation" (saveSubmissionEvaluation sk) (loadEvaluation) (Gen.evaluations cfg)

success n = stdArgs { maxSuccess = n, chatty = False }

massTest = testCase "Mass Test" massPersistenceTest

massTestParallel = testCase "Mass Test Parallel" $ do
  forkIO massPersistenceTest
  massPersistenceTest

-- ListRef is an IO reference to an arbitrary list
-- Interpretation: informational channel between different
-- part of test cases in the IO monad.
type ListRef a = IORef [a]

-- Creates a list reference with an empty list in it
createListRef :: IO (ListRef a)
createListRef = newIORef []

-- Inserts an element to the given list hold by the list reference
insertListRef :: ListRef a -> a -> IO ()
insertListRef r e = modifyIORef r (e:)

-- Returns the list hold in the list reference
listInRef :: ListRef a -> IO [a]
listInRef r = readIORef r

-- Creates the given number of files in the upload template directory
uploadTempFiles tmpDir n = do
  list <- createListRef
  quick n $ do
    (fp, handle) <- run $ openTempFile tmpDir "tmp.txt"
    words <- pick . listOf . listOf1 $ elements ['a' .. 'z']
    run $ do
      hPutStr handle $ concat $ intersperse " " words
      hClose handle
      insertListRef list fp
  listInRef list

-- Generate and store the given number of courses and returns the
-- course keys stored in the
courses n = do
  list <- createListRef
  quick n $ do
    ck <- courseSaveAndLoad
    run $ insertListRef list ck
  listInRef list

-- Generate and store the given number of groups and assigns them to random courses,
-- from the given list, returns all the created group keys
groups n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    gk <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) (Gen.groups)
    run $ insertListRef list gk
  listInRef list

-- Generate and store the given numner of assignment and assign them to random groups,
-- from the given list, returns all the created assignment keys
groupAssignmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assignment"
      (saveGroupAssignment gk) (loadAssignment) (Gen.assignments startDate endDate)
    run $ insertListRef list ak
  listInRef list

-- Generate and store the given numner of assignment and assign them to random courses,
-- from the given list, returns all the created assignment keys
courseAssignmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assignment"
      (saveCourseAssignment gk) (loadAssignment) (Gen.assignments startDate endDate)
    run $ insertListRef list ak
  listInRef list

groupAssessmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assessment"
      (saveGroupAssessment gk) (loadAssessment) Gen.assessments
    run $ insertListRef list ak
  listInRef list

courseAssessmentGen n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    ak <- saveAndLoadIdenpotent "Course assessment"
      (saveCourseAssessment ck) (loadAssessment) Gen.assessments
    run $ insertListRef list ak
  listInRef list

-- Generate and store the given number of users, and returns the usernames found in the
-- persistence layer
users n = do
  list <- createListRef
  quick n $ do
    u <- pick Gen.users
    u' <- createOrLoadUser u
    run $ insertListRef list (u_username u)
  listInRef list

-- Generate and store the given numner of admin users, and returns the usernames saved to
-- the persistence layer, if no admin was generated an error would be thrown
admins n = do
  list <- createListRef
  quick n $ do
    u <- setAdmin <$> pick Gen.users
    let username = u_username u
    exist <- runPersistCmd $ doesUserExist username
    unless exist $ do
      createOrLoadUser u
      run $ insertListRef list username
  users <- listInRef list
  assertNonEmpty users "No admins were generated."
  return users
  where
    setAdmin u = u { u_role = Admin }

-- Select an admin and a course and set the admin as a course admin
setCourseAdmins as cs n =
  quick n $ do
    a <- pick $ elements as
    c <- pick $ elements cs
    runPersistCmd $ createCourseAdmin a c

-- Select an admin and a course and set the admin as a course admin
setGroupAdmins as gs n = do
  quick n $ do
    a <- pick $ elements as
    g <- pick $ elements gs
    runPersistCmd $ createGroupAdmin a g

-- SubmissionInfoList is a list from username, assignment-key and the submission-key for them.
-- Interpretation: The submission information about which user submitted which submission
-- for the given assignment.
type SubmissionInfoList = [((Username,AssignmentKey),SubmissionKey)]

-- Throws away the assignment and the username from the submission information
infoListToSubmissionKeys :: SubmissionInfoList -> [SubmissionKey]
infoListToSubmissionKeys = (fmap snd)

-- Generates and stores the given number of comments, for the randomly selected
-- submissions. Returns all the created comment key with the associated submissionKey.
comments :: Int -> [SubmissionKey] -> IO [(CommentKey, SubmissionKey)]
comments n ss = do
  list <- createListRef
  let now = utcTimeConstant
  quick n $ do
    sk <- pick $ elements ss
    ck <- saveAndLoadIdenpotent "Comment" (saveComment sk) (loadComment) (Gen.comments now)
    run $ insertListRef list (ck, sk)
  listInRef list

-- Generates and stores the given number of feedbacks, for the randomly selected
-- submissions. Returns all the created feedback key with the associated submissionKey.
feedbacks :: Int -> [SubmissionKey] -> IO [(FeedbackKey, SubmissionKey)]
feedbacks n ss = do
  list <- createListRef
  let now = utcTimeConstant
  quick n $ do
    sk <- pick $ elements ss
    fk <- saveAndLoadIdenpotent "Feedback" (saveFeedback sk) (loadFeedback) (Gen.feedbacks now)
    run $ insertListRef list (fk, sk)
  listInRef list

{- XXX
-- Generates and stores the given number of system notifications.
-- Returns all the creates notification key.
systemNotifications :: Int -> IO [NotificationKey]
systemNotifications n = do
  list <- createListRef
  quick n $ do
    nk <- saveAndLoadIdenpotent "Notification" saveSystemNotification loadNotification Gen.notifications
    run $ insertListRef list nk
  listInRef list

-- Generates and stores the given number of comment notifications for randomly selected comments
-- Returns the list of the associated notification and comment keys.
commentNotifications :: Int -> [CommentKey] -> IO [(NotificationKey, CommentKey)]
commentNotifications n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    nk <- saveAndLoadIdenpotent "Notification" (saveCommentNotification ck) loadNotification Gen.notifications
    run $ insertListRef list (nk,ck)
  listInRef list

-- Generates and stores the given number of feedback notifications for randomly selected comments
-- Returns the list of the associated notification and feedback keys.
feedbackNotifications :: Int -> [FeedbackKey] -> IO [(NotificationKey, FeedbackKey)]
feedbackNotifications n fs = do
  list <- createListRef
  quick n $ do
    fk <- pick $ elements fs
    nk <- saveAndLoadIdenpotent "Notification" (saveFeedbackNotification fk) loadNotification Gen.notifications
    run $ insertListRef list (nk,fk)
  listInRef list
-}

-- Generate and store the given number of submissions, for the randomly selected
-- user and assignment. Returns all the created submission keys with the associated
-- username and assignment
submissions :: Int -> [Username] -> [AssignmentKey] -> IO SubmissionInfoList
submissions n us as = do
  list <- createListRef
  quick n $ do
    u <- pick $ elements us
    ak <- pick $ elements as
    sk <- saveAndLoadIdenpotent "Submission"
      (saveSubmission ak u) (loadSubmission) (Gen.submissions startDate)
    run $ insertListRef list ((u,ak),sk)
  listInRef list

-- Generate and store the given number of evaluations, for the randomly selected
-- submission, and returns all the created evaluation keys
evaluations n ss = do
  list <- createListRef
  quick n $ do
    sk <- pick $ elements ss
    cfg <- evaluationConfigForSubmission sk
    ek <- saveAndLoadIdenpotent "Evaluation"
      (saveSubmissionEvaluation sk) (loadEvaluation) (Gen.evaluations cfg)
    run $ insertListRef list ek
  listInRef list

-- Generates and stores the given number of test scripts for the randomly selected
-- courses, and returns all the created TestScriptKeys
testScripts n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    tsk <- saveAndLoadIdenpotent "TestScript"
      (saveTestScript ck) (loadTestScript) (Gen.testScripts)
    run $ insertListRef list tsk
  listInRef list

scores n us as = do
  list <- createListRef
  quick n $ do
    u <- pick $ elements us
    a <- pick $ elements as
    sk <- saveAndLoadIdenpotent "Score"
      (saveScore u a) (loadScore) (Gen.scores)
    run $ insertListRef list sk
  listInRef list

-- Generates and stores the given number of the test cases for the randomly selected
-- test scripts and assignments, and returns all the created TestCaseKeys
testCases n tcs as = do
  list <- createListRef
  quick n $ do
    tsk <- pick $ elements tcs
    ak  <- pick $ elements as
    mtk <- runPersistCmd $ testCaseOfAssignment ak
    case mtk of
      Just tk -> return ()
      Nothing -> do tck <- saveAndLoadIdenpotent "TestCase"
                             (saveTestCase tsk ak) (loadTestCase) (Gen.testCases)
                    run $ insertListRef list tck
  listInRef list

massPersistenceTest = do
  cs <- courses 100
  gs <- groups 250 cs
  gas <- groupAssignmentGen 400 gs
  cas <- courseAssignmentGen 400 cs
  let as = gas ++ cas
  us <- users 300
  ss <- infoListToSubmissionKeys <$> submissions 500 us as
  evaluations 400 ss
  return ()


quick n p = check (return ()) $ quickCheckWithResult (success n) $ monadicIO p

quickWithCleanUp cleanup n p = check cleanup $ quickCheckWithResult (success n) $ monadicIO p

check cleanup m = do
  x <- m
  case x of
    s@(Success {}) -> cleanup
    f@(Failure {}) -> cleanup >> (fail $ reason f)
    other          -> cleanup >> (fail $ output other)

-- The test are at very high level, we must see if basic load
-- properties are hold.

reinitPersistence = do
  init <- createPersistInit defaultConfig
  tearDown init
  initPersist init

initPersistence = do
  init <- createPersistInit defaultConfig
  initPersist init

courseAndGroupAssignments cn gn cs gs = do
  cas <- courseAssignmentGen cn cs
  gas <- groupAssignmentGen gn gs
  return (cas ++ gas)

courseAndGroupAssessments cn gn cs gs = do
  cas <- courseAssessmentGen cn cs
  gas <- groupAssessmentGen gn gs
  return (cas ++ gas)

-- User can register course and groups and these groups and courses can have assignments.
-- The user can subscribe to groups and course, and it is necessary to him to
-- see the assignment of the groups and courses, and only the assignment of the
-- courses that the user registered, others no
userAssignmentKeyTests = test $ testCase "User assignment tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 300
  quick 300 $ do
    u <- pick $ elements us
    gk <- pick $ elements gs
    ck <- runPersistCmd $ courseOfGroup gk
    runPersistCmd $ subscribe u ck gk
    gas <- runPersistCmd $ groupAssignments gk
    cas <- runPersistCmd $ courseAssignments ck
    let uas = gas ++ cas
    as <- runPersistCmd $ fmap toList $ userAssignmentKeys u
    when (null as) $ assertTrue (null gas)
      "Group has assignment, but user does not see it"
    unless (or [null uas, null as]) $ assertTrue
      (not . Set.null $ Set.intersection (Set.fromList uas) (Set.fromList as))
      (join [
          "User assignment for a given course and group was not found. User:", show u
        , " Group and course assignments: ", show uas, " User assignments:", show as
        , " Group: ", show gk
        , " Course: ", show ck
        , " Group assignments: ", show gas
        , " Course assignment: ", show cas
        ])
  where
    toList = nub . join . map (Set.toList . snd) . Map.toList

-- Every assignment has a group or a course
courseOrGroupAssignmentTest = test $ testCase "Course or group assignment tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  gas <- groupAssignmentGen 150 gs
  cas <- courseAssignmentGen 150 cs
  let as = cas ++ gas
  quick 500 $ do
    ak <- pick $ elements as
    k <- runPersistCmd $ courseOrGroupOfAssignment ak
    either
      (\c -> assertTrue (elem c cs) "Course is not in the courses")
      (\g -> assertTrue (elem g gs) "Group is not in the groups")
      k

-- Group description can be created from any group
groupDescriptionTest = test $ testCase "Group description tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  us <- users 300
  quick 300 $ do
    groupAdmin <- pick $ elements us
    gk         <- pick $ elements gs
    runPersistCmd $ createGroupAdmin groupAdmin gk
    groupAdmins <- runPersistCmd $ groupAdmins gk
    assertTrue (elem groupAdmin groupAdmins) "Group admin was not in the group admins"
  quick 500 $ do
    gk <- pick $ elements gs
    (gk', desc) <- runPersistCmd $ groupDescription gk
    assertEquals gk gk' "Group keys are different"
    assertTrue (not . null . gName $ desc) "Name was empty"
    admins <- runPersistCmd $ groupAdmins gk
    assertTrue (length (gAdmins desc) == length admins) "Group admin numbers was different"

-- Every submission has some kind of description
submissionDescTest = test $ testCase "Every submission has some kind of description" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  us <- users 300
  as <- courseAndGroupAssignments 150 150 cs gs
  ss <- infoListToSubmissionKeys <$> submissions 500 us as
  quick 500 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDesc sk
    assertNonEmpty (eCourse desc) "Course name was empty"
    maybe (return ()) (flip assertNonEmpty "Group name was empty") $ eGroup desc
    assertNonEmpty (eStudent desc) "Student name was empty"
    assertNonEmpty (eSolution desc) "Solution was empty"
    assertNonEmpty (Assignment.name . eAssignment $ desc) "Assignment title was empty"
    assertEmpty (Map.toList $ eComments desc) "The comment list was not empty"

-- Every assignment must have a course name and the
-- dedicated users must be returned as admins
-- for the given course
courseNameAndAdminsTest = test $ testCase "Every assignment has to have a name and admin" $ do
  reinitPersistence
  cs <- courses 100
  us <- users 150
  gs <- groups 100 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  quick 400 $ do
    u <- pick $ elements us
    c <- pick $ elements cs
    a <- pick $ elements as
    (name, admins) <- runPersistCmd $ courseNameAndAdmins a
    ek <- runPersistCmd $ courseOrGroupOfAssignment a
    admins' <- either
      (runPersistCmd . courseAdmins)
      (runPersistCmd . groupAdmins)
      ek
    assertNonEmpty name "Course name was empty"
    assertTrue (length admins' == length admins) "Admin numbers are different"

-- Every assignment and an associated user has a submission list,
-- which contains information about the submissions posted to the given assignment
-- by the user
submissionListDescTest = test $ testCase "Every assignment and an associated user has a submission list" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 300
  ss <- infoListToSubmissionKeys <$> submissions 400 us as
  quick 500 $ do
    u <- pick $ elements us
    a <- pick $ elements as
    desc <- runPersistCmd $ submissionListDesc u a
    assertNonEmpty (slGroup desc) "Group was empty"
    assertNonEmpty (Assignment.desc $ slAssignment desc) "Assignment was empty"
    assertEmpty (slTeacher desc) "There was teachers to the group"

-- Allways the last evaluation is valid for the submission.
lastEvaluationTest = test $ testCase "Allways the last evaluation is valid for the submission" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 400
  assignmentSubmission1 <- Map.fromList <$> submissions 400 us as
  assignmentSubmission2 <- Map.fromList <$> submissions 400 us as
  -- If there is a user and an assignment in the new submission map
  -- and in the old one also, the submission keys must be different ones
  forM_ (Map.keys assignmentSubmission2) $ \key -> do
      case (Map.lookup key assignmentSubmission2, Map.lookup key assignmentSubmission1) of
        (Nothing, Nothing) -> fail "Impossible situation: key must exist"
        (Just v,  Just v')
          | v == v' -> fail "Submission is the old one after submitting the new one"
        _ -> return ()

createCourseAdmins n us cs = quick n $ do
  u <- pick $ elements us
  usr <- runPersistCmd $ loadUser u
  pre (atLeastCourseAdmin . u_role $ usr)
  c <- pick $ elements cs
  runPersistCmd $ createCourseAdmin u c

createGroupAdmins n us gs = quick n $ do
  u <- pick $ elements us
  g <- pick $ elements gs
  runPersistCmd $ createGroupAdmin u g

-- Every submission has a description, this description must be loaded
submissionDetailsDescTest = test $ testCase "Every submission has a description" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 400
  createCourseAdmins 200 us cs
  createGroupAdmins 200 us gs
  ss <- infoListToSubmissionKeys <$> submissions 400 us as
  quick 1000 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDetailsDesc sk
    assertNonEmpty (sdGroup desc) "Group name was empty"
    forM (sdTeacher desc) $ \t -> assertNonEmpty t "Admin name was empty"
    assertNonEmpty (Assignment.desc $ sdAssignment desc) "Description was empty"
    when (isJust (sdStatus desc)) $ assertNonEmpty (fromJust $ sdStatus desc) "Status was empty"
    assertNonEmpty (sdSubmission desc) "Submission text was empty"
    forM (Map.toList $ sdComments desc) $ \(_,c) -> assertNonEmpty (comment c) "Comment was empty"

-- If the user administrates courses or groups, submission information about the
-- submission of the group or course attendees. The number of the tables are same as
-- the number of the groups and courses administrated by this user
submissionTablesTest = test $ testCase "Submission tables" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  us <- users 400
  as <- courseAndGroupAssignments 150 150 cs gs
  createCourseAdmins 200 us cs
  createGroupAdmins 200 us gs

  quick 500 $ do
    u <- pick $ elements us
    g <- pick $ elements gs
    c <- runPersistCmd $ courseOfGroup g
    runPersistCmd $ subscribe u c g

  quick 1000 $ do
    u <- pick $ elements us
    acs <- runPersistCmd $ administratedCourses u
    ags <- runPersistCmd $ administratedGroups  u
    ts  <- runPersistCmd $ submissionTables     u
    forM ts $ \t -> do
      assertNonEmpty (stiCourse t) "Course name was empty"
--      assertTrue (length (stAssignments t) >= 0) "Invalid assignment list" TODO
      forM (stiUsers t) $ usernameCata (\u -> assertNonEmpty u "Username was empty")
      assertTrue (length (stiUserLines t) >= 0) "Invalid user line number"

-- The user can have submissions for the given assignment, and information can be
-- calculated about these submissions
userSubmissionDescTest = test $ testCase "The user can have submissions and information" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 250 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 300
  asgMap <- Map.fromList <$> submissions 750 us as
  quick 1000 $ do
    u <- pick $ elements us
    a <- pick $ elements as
    desc <- runPersistCmd $ userSubmissionDesc u a
    assertNonEmpty (usCourse desc) "Course was empty"
    assertNonEmpty (usAssignmentName desc) "Assignment name was empty"
    assertNonEmpty (usStudent desc) "Student name was empty"
    ss <- runPersistCmd $ userSubmissions u a
    assertEquals
      (Set.fromList ss) (Set.fromList (map fst3 (usSubmissions desc)))
      "Submission numbers were different"
  where
    fst3 (a,_,_) = a

-- All the saved course must have a key and these
-- course keys must be listed
courseKeysTest = test $ testCase "All the saved courses must have a key" $ do
  reinitPersistence
  savedKeys  <- Set.fromList <$> courses 100
  loadedKeys <- Set.fromList <$> (runPersistIOCmd $ courseKeys)
  assertEquals savedKeys loadedKeys "Saved and loaded courses were different"
  savedKeys2  <- Set.fromList <$> courses 50
  loadedKeys2 <- Set.fromList <$> (runPersistIOCmd $ courseKeys)
  assertTrue (Set.isSubsetOf loadedKeys loadedKeys2) "Not all old course keys were in the loaded set"
  assertTrue (Set.isSubsetOf savedKeys2 loadedKeys2) "New course keys were not in the loaded set"

-- All the saved assignment must have a key and these keys must be listed
assignmentKeyTest = test $ testCase "All the saved assignments must have a key" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  saved  <- Set.fromList <$> (courseAndGroupAssignments 150 150 cs gs)
  loaded <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys)
  assertEquals saved loaded "Saved and loaded assignment keys were different"
  saved2  <- Set.fromList <$> (courseAndGroupAssignments 50 50 cs gs)
  loaded2 <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys)
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all assignment keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New assignment keys were not in the loaded set"

-- All the saved submissions must have a key and these keys must be listed
filterSubmissionsTest = test $ testCase "All the saved submissions must have a key" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 400
  saved  <- (Set.fromList . infoListToSubmissionKeys) <$> (submissions 500 us as)
  loaded <- Set.fromList <$> (runPersistIOCmd submissionKeys)
  assertEquals saved loaded "Saved and loaded submission keys were different"
  saved2  <- (Set.fromList . infoListToSubmissionKeys) <$> (submissions 100 us as)
  loaded2 <- Set.fromList <$> (runPersistIOCmd submissionKeys)
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all submission keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New submission keys were not in the loaded set"

-- Modified assignments must be untouched after loading them
modifyAssignmentsTest = test $ testCase "Modified assignments must be untouched after loading them" $ do
  cs <- courses 100
  gs <- groups 150 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  quick 1000 $ do
    ak <- pick $ elements as
    a0 <- runPersistCmd $ loadAssignment ak
    a  <- pick (Gen.assignments startDate endDate)
    runPersistCmd $ modifyAssignment ak a
    a1 <- runPersistCmd $ loadAssignment ak
    assertEquals a a1 "Modified and loaded assignments were differents"

-- Modified evaluations must be untouched after loading them
modifyEvaluationTest = test $ testCase "Modified evaluations must be untouched after loading them" $ do
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 200
  ss <- infoListToSubmissionKeys <$> (submissions 400 us as)
  es <- evaluations 600 ss
  quick 1000 $ do
    ek <- pick $ elements es
    msk <- runPersistCmd $ submissionOfEvaluation ek
    assertTrue
      (isJust msk)
      "There is no submission for the submission related evaluation"
    let sk = fromJust msk
    cfg <- evaluationConfigForSubmission sk
    e <- pick $ Gen.evaluations cfg
    runPersistCmd $ modifyEvaluation ek e
    e1 <- runPersistCmd $ loadEvaluation ek
    assertEquals e e1 $ concat
      [ "Modified and loaded evaluations were different "
      , "Evaluation key:", show ek
      , "Generated: ", show e
      , "Loaded: ", show e1
      ]

-- Subscribe users to groups
subscribeUsers n us gs =
  quick n $ do
    u <- pick $ elements us
    g <- pick $ elements gs
    runPersistCmd $ do
      c <- courseOfGroup g
      subscribe u c g

-- Test if the users make unsubscribe from the courses by the admin
deleteUsersFromCourseTest = test $ testCase "Delete user form course" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  quick 1000 $ do
    u <- pick $ elements us
    ucs <- runPersistCmd $ userCourses u
    case ucs of
      [] -> testEmptyCourse cs u
      [c] -> testOneCourse u c
      cs' -> testMoreCourses u cs'
  where
    -- Test if selecting any of the course, trying to unsubscribe does not
    -- produce error and the number of the subscriptions does not change
    testEmptyCourse cs u = do
      c <- pick $ elements cs
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals [] ucs "Subscirbed to some course."

    -- Test if subscribing from the course produces an empty course list
    testOneCourse u c = do
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals [] ucs "Subscirbed to some course."

    -- Check if the deletion of one course removes only the deleted course
    testMoreCourses u cs' = do
      c <- pick $ elements cs'
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals ((length cs') - 1) (length ucs) "No only one courses was deleted"
      assertEquals (cs' \\ [c]) ucs "No the right course was deleted"

deleteUsersFromCourseNegativeTest = test $ testCase "Delete user from courses not belong to" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  -- Tries to subscribe students from groups that are not attended in
  quick 1000 $ do
    u <- pick $ elements us
    ucs <- runPersistCmd $ userCourses u
    c' <- pick $ elements (cs \\ ucs)
    runPersistCmd $ deleteUserFromCourse c' u
    ucs' <- runPersistCmd $ userCourses u
    assertEquals ucs ucs' "User's course list has changed"

unsubscribeFromSubscribedGroupsTest = test $ testCase "User unsubscribes from a course" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  quick 1000 $ do
    u <- pick $ elements us
    ugs <- runPersistCmd $ userGroups u
    when (not $ null ugs) $ do
    g <- pick $ elements ugs
    join $ runPersistCmd $ do
      ucsb <- userCourses u
      ugsb <- userGroups  u
      c <- courseOfGroup  g
      unregscb <- unsubscribedFromCourse c
      unregsgb <- unsubscribedFromGroup  g
      unsubscribe u c g
      ucsa <- userCourses u
      ugsa <- userGroups  u
      unregsca <- unsubscribedFromCourse c
      unregsga <- unsubscribedFromGroup  g
      return $ case g `elem` ugsb of
        True -> do
          assertEquals (length ugsa) (length ugsb - 1) $ concat
            [ "User is not unsubscribed from group "
            , " Before unsubscription: ", show ugsb
            , " After unsubscription: ", show ugsa
            ]
          assertSetEquals (ugsb) (g:ugsa) "User is not unsubscribed from group #2"
          assertFalse (u `elem` unregsgb) "User was in the group unsubscribed list"
          assertTrue  (u `elem` unregsga) "User is not in the group unsubscribed list"
          -- First unsubscription, before and after values must differs
        False -> do
          -- Second unsubscription, before and after values must be the same
          assertSetEquals (ugsb) (ugsa) "User is unsubscribed from course #2"
          assertSetEquals (unregsgb) (unregsga) "User is in the course unsubscribed list"

saveLoadAndModifyTestScriptsTest = test $ testCase "Save, load and modify test scripts" $ do
  reinitPersistence
  cs <- courses 200
  tss <- testScripts 1000 cs
  quick 1000 $ do
    ts <- pick $ elements tss
    nts <- pick $ Gen.testScripts
    join $ runPersistCmd $ do
      modifyTestScript ts nts
      nts' <- loadTestScript ts
      ck <- courseOfTestScript ts
      ctss <- testScriptsOfCourse ck
      return $ do
        assertEquals nts nts' "Modifing the test script failed"
        assertTrue (elem ts ctss) "Test Script is not in it's course"

saveLoadAndModifyTestCasesTest = test $ testCase "Save, load and modify test cases" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  tss <- testScripts 500 cs
  as <- courseAndGroupAssignments 100 100 cs gs
  tcs <- testCases 1000 tss as
  quick 1000 $ do
    tc  <- pick $ elements tcs
    ntc <- pick $ Gen.testCases
    join $ runPersistCmd $ do
      modifyTestCase tc ntc
      ntc' <- loadTestCase tc
      return $ do
        assertEquals ntc ntc' "Modification of the test case has failed"

-- Creates a temporary directory for the bead in the system's temp dir
createBeadTempDir :: IO FilePath
createBeadTempDir = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp "bead."


userFileHandlingTest = test $ testCase "Copy, list, and get user's data file path" $ do
  reinitPersistence
  tmpDir <- createBeadTempDir
  us <- users 100
  fs <- uploadTempFiles tmpDir 1000
  let userFileTypes = [UsersPublicFile, UsersPrivateFile]
  quickWithCleanUp (removeDirectoryRecursive tmpDir) 1000 $ do
    u <- pick $ elements us
    f <- pick $ elements fs
    fn <- pick $ oneof
      [ t <$> (vectorOf 8 $ elements ['a'..'z']) | t <- userFileTypes ]
    ufs <- map fst <$> (runPersistCmd $ listFiles u)
    join $ case fn `elem` ufs of
      True  -> testOverwriteFile u f fn ufs
      False -> testNewFile u f fn ufs
  where
    testNewFile u f fn ufs = runPersistCmd $ do
      copyFile u f fn
      ufs' <- map fst <$> listFiles u
      path <- getFile u fn
      return $ do
        assertSetEquals (fn:ufs) ufs'
          $ concat ["New file was not copied into the ", show u, " dir"]
        assertTrue (length path > 0) "Invalid path"

    testOverwriteFile u f fn ufs = runPersistCmd $ do
      path  <- getFile u fn
      copyFile u f fn
      content <- liftIO $ readFile f
      path' <- getFile u fn
      content' <- liftIO $ readFile path'
      return $ do
        assertEquals path path' "The overwritted file path's has changed"
        assertEquals content content' "The file content was not overwritted"

userOverwriteFileTest = test $ testCase "Overwrite user's data file" $ do
  reinitPersistence
  tmpDir <- createBeadTempDir
  us <- users 100
  fs <- uploadTempFiles tmpDir 1000
  let userFileTypes = [UsersPublicFile, UsersPrivateFile]
  forM_ us $ \u -> quick 5 $ do
    f <- pick $ elements fs
    fn <- pick $ oneof
      [ t <$> (vectorOf 8 $ elements ['a'..'z']) | t <- userFileTypes ]
    runPersistCmd $ copyFile u f fn
  quickWithCleanUp (removeDirectoryRecursive tmpDir) 1000 $ do
    u <- pick $ elements us
    ufs <- map fst <$> (runPersistCmd $ listFiles u)
    f <- pick $ elements fs
    fn <- pick $ elements ufs
    join $ runPersistCmd $ do
      path <- getFile u fn
      copyFile u f fn
      content <- liftIO $ readFile f
      path' <- getFile u fn
      content' <- liftIO $ readFile path'
      ufs' <- map fst <$> listFiles u
      return $ do
        assertSetEquals ufs ufs' "The user's file set was changed"
        assertEquals path path' "The user's file path was changed"
        assertEquals content content' "The user's file content is not copied correctly"

testJobCreationTest = test $ testCase "Test Job cration" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  tss <- testScripts 100 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  tcs <- testCases 600 tss as
  ss <- submissions 1500 us as
  testedSks <- createListRef
  quick 1000 $ do
    ((_u,_ak),sk) <- pick $ elements ss
    tsks <- run $ listInRef testedSks
    case sk `elem` tsks of
      True -> return ()
      False -> do
        run $ insertListRef testedSks sk
        join $ runPersistCmd $ do
          saveTestJob sk
          ak <- assignmentOfSubmission sk
          mtck <- testCaseOfAssignment ak
          maybe (testIfHasNoTestJob sk) (testIfHasTestJob sk) mtck

  where
    testIfHasNoTestJob sk = do
      let tk = submissionKeyToTestJobKey sk
      exist <- liftIO $ doesDirectoryExist $ referredPath tk
      return $ do
        assertFalse exist "Test Job directory is exist"

    testIfHasTestJob sk tck = do
      -- Domain knowledge is used
      tsk <- testScriptOfTestCase tck
      let tk = submissionKeyToTestJobKey sk
      script     <- liftIO $ readFile $ referredPath tk </> "script"
      submission2 <- loadSubmission sk
      assertSubmissions <- withSubmissionValue (solution submission2)
        (\sol -> do testSolution <- liftIO $ readFile $ referredPath tk </> "submission"
                    return $ assertEquals sol testSolution "Solutions are different")
        (\sol -> do testSolution <- liftIO $ BS.readFile $ referredPath tk </> "submission"
                    return $ assertEquals sol testSolution "Solutions are different")
      script2     <- loadTestScript tsk
      case2       <- loadTestCase   tck
      assertTests <- withTestCaseValue
        (tcValue case2)
        (\testValue -> do tests <- liftIO $ readFile $ referredPath tk </> "tests"
                          return $ assertEquals tests testValue "Tests are different")
        (\testValue -> do tests <- liftIO $ BS.readFile $ referredPath tk </> "tests"
                          return $ assertEquals tests testValue "Tests are different")
      return $ do
--        assertEquals submission (solution submission2) "Submissions are different"
        assertEquals script (tsScript script2) "Scripts are different"
        assertTests
        assertSubmissions

insertAndFinalizeTestFeedback sk feedback = do
  insertTestFeedback sk feedback
  finalizeTestFeedback sk

finalizeFeedbacksTest = test $ testCase "Locked feedback tests" $ do
  reinitPersistence
  us <- users 100
  cs <- courses 10
  gs <- groups 50 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  ss <- submissions 1500 us as
  lockedFeedbackList <- createListRef
  finalizedFeedbackList <- createListRef
  quick 1000 $ do
    ((_u,_ak),sk) <- pick $ elements ss
    locked    <- run $ listInRef lockedFeedbackList
    finalized <- run $ listInRef finalizedFeedbackList
    case (sk `elem` locked, sk `elem` finalized) of
      (False, False) -> checkIfCanBeAttached sk lockedFeedbackList
      (False, True)  -> checkIfThereIsAFeedback sk
      (True, False)  -> checkIfCanBeFinalized sk finalizedFeedbackList
      (True, True)   -> checkIfThereIsAFeedback sk

  where
    checkIfThereIsAFeedback sk =
      join $ runPersistCmd $ do
        cks <- map fst <$> testFeedbacks
        return $ do
          assertTrue (sk `elem` cks) "Test Feedback is not inserted."

    checkIfCanBeAttached sk feedbackList = do
      run $ insertListRef feedbackList sk
      feedback <- pick $ Gen.testFeedbackInfo
      join $ runPersistCmd $ do
        insertTestFeedback sk feedback
        cks <- map fst <$> testFeedbacks
        return $ do
          assertFalse (sk `elem` cks)
            "Test Locked Feedback occurs in the feedback list."

    checkIfCanBeFinalized sk feedbackList = do
      run $ insertListRef feedbackList sk
      join $ runPersistCmd $ do
        finalizeTestFeedback sk
        cks <- map fst <$> testFeedbacks
        return $ do
          assertTrue (sk `elem` cks)
            "Test Finalized Feedback does not occur in the feedback list."

incomingFeedbacksTest = test $ testCase "Incoming feedbacks" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  ss <- submissions 1500 us as
  feedbackList <- createListRef
  quick 1000 $ do
    ((_u,_ak),sk) <- pick $ elements ss
    cks <- run $ listInRef feedbackList
    case sk `elem` cks of
      True  -> checkIfThereIsAFeedback sk
      False -> checkIfCanBeCommented sk feedbackList
  where
    checkIfThereIsAFeedback sk =
      join $ runPersistCmd $ do
        cks <- map fst <$> testFeedbacks
        return $ do
          assertTrue (sk `elem` cks) "Test Feedback is not inserted"

    checkIfCanBeCommented sk feedbackList = do
      run $ insertListRef feedbackList sk
      feedback <- pick $ Gen.testFeedbackInfo
      join $ runPersistCmd $ do
        insertAndFinalizeTestFeedback sk feedback
        cks <- map fst <$> testFeedbacks
        return $ do
          assertTrue (sk `elem` cks) "Test Feedback is not inserted"

unevaluatedScoresTests = test $ testCase "Unevaluated scores" $ do
  reinitPersistence
  us <- users 50
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  scs <- scores 500 us as
  quick 1000 $ do
    s <- pick $ elements scs
    -- All the saved scores should have an assessment
    -- All the saved scores should have appear in the score list of the assessment
    a <- runPersistCmd $ assessmentOfScore s
    sa <- runPersistCmd $ scoresOfAssessment a
    assertTrue (elem s sa) ("The score was not in the score list of assessment: " ++ show s)

    -- All the saved scores should have a username
    -- All the saved scores should have appear in the score list of the user
    u <- runPersistCmd $ usernameOfScore s
    su <- runPersistCmd $ scoresOfUser u
    assertTrue (elem s su) ("The score was not in the score list of user: " ++ show s)

    -- All the saved scores should not have an evaluation
    e <- runPersistCmd $ evaluationOfScore s
    assertEquals Nothing e "Unevaluated score has an evaluation"

scoreEvaluationTests = test $ testCase "Evaluated scores" $ do
  reinitPersistence
  us <- users 50
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  scs <- scores 500 us as
  quick 1000 $ do
    s <- pick $ elements scs
    es <- runPersistCmd $ evaluationOfScore s
    -- All the score evaluation should not have a submission key
    case es of
      -- All the evaluated scores should have the evaluation key
      Nothing -> do
        -- TODO: Copy the evaluation of the score
        cfg <- runPersistCmd $ evalConfigOfScore s
        e   <- pick $ Gen.evaluations cfg
        ek  <- runPersistCmd $ saveScoreEvaluation s e
        ek' <- runPersistCmd $ evaluationOfScore s
        assertEquals (Just ek) ek' "The freshly evaluated score does not have the score key."
        s'  <- runPersistCmd $ scoreOfEvaluation ek
        assertEquals (Just s) s' "The score evaluation does not have the score."
        sbm <- runPersistCmd $ submissionOfEvaluation ek
        assertEquals Nothing sbm "The score evaluation has a submission key."

      -- All the evaluation of the score should have the score key
      Just ek -> do
        s'  <- runPersistCmd $ scoreOfEvaluation ek
        assertEquals (Just s) s' "The score evaluation does not have the score."
        sbm <- runPersistCmd $ submissionOfEvaluation ek
        assertEquals Nothing sbm "The score evaluation has a submission key."
  where
    evalConfigOfScore s = do
      ak <- assessmentOfScore s
      a <- loadAssessment ak
      return $! evaluationCfg a

assessmentTests = test $ testCase "Assessment tests" $ do
  let groupOrCourseOf a = runPersistCmd $ do
        c <- courseOfAssessment a
        g <- groupOfAssessment a
        return (c,g)

  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  quick 1000 $ do
    a <- pick $ elements as

    -- All the assessemnt should have at least either a course or group
    (c,g) <- groupOrCourseOf a

    case (c,g) of
      (Nothing, Nothing) -> fail "There was no course or group of the assessment."
      (Just _, Just _)   -> fail "There were course and group of assessment."

      -- The course of assessment should be appear in its course assessment list
      (Just c, Nothing)  -> do
        as' <- runPersistCmd $ assessmentsOfCourse c
        assertTrue (elem a as') "The course assessment was not registered in its course."

      -- The group of the assessment should be appear in its group assessment list
      (Nothing, Just g)  -> do
        as' <- runPersistCmd $ assessmentsOfGroup g
        assertTrue (elem a as') "The group assessment was not registered in its group."

    -- All the non scores assessment should be have empty score list
    s <- runPersistCmd $ scoresOfAssessment a
    assertEquals s [] "There were some scores for the assessemnt"

    -- The modification of an assessment should be stored property
    asm <- pick $ Gen.assessments
    runPersistCmd $ modifyAssessment a asm
    asm' <- runPersistCmd $ loadAssessment a
    assertEquals asm asm' "The modification of the assessment has failed."

    -- The modification of an assessment should not change its group or course
    (c',g') <- groupOrCourseOf a
    assertEquals (c,g) (c',g') "The course or group of the assessment has changed after modification."

openSubmissionsTest = test $ testCase "Open submissions list" $ do
  reinitPersistence
  us <- users 50
  as <- admins 10
  cs <- courses 10
  gs <- groups 40 cs
  asg <- courseAndGroupAssignments 30 30 cs gs
  setCourseAdmins as cs 100
  setGroupAdmins as gs 100
  subscribeUsers 400 us gs
  submissions 100 us asg
  quick 100 $ do
    a <- pick $ elements as
    os <- runPersistCmd $ openedSubmissionInfo a
    let adminedCourses = map fst $ osAdminedCourse os
        adminedGroups  = map fst $ osAdminedGroup os
        relatedCourses = map fst $ osRelatedCourse os
    pre (not (or [null adminedCourses, null adminedGroups, null relatedCourses]))
    checkAdminedCourse a adminedCourses
    checkAdminedGroup a adminedGroups
    checkRelatedCourse a relatedCourses
  return ()
  where
    -- Check if the user of the assignment attends a course that
    -- the admin administrates. Check if the course of the submission
    -- is administrated by the admin. Check if the course is related to the
    -- admin via groups that the user administrates
    checkAdminedCourse a sks = runPersistCmd $ do
      adminedCourses <- (map fst) <$> administratedCourses a
      relatedCourses <- do
        gs <- map fst <$> administratedGroups a
        mapM courseOfGroup gs
      let courses = adminedCourses ++ relatedCourses
      forM_ sks $ \sk -> do
        u  <- usernameOfSubmission sk
        isInCourse <- or <$> mapM (isUserInCourse u) courses
        assertTrue isInCourse $ join
          [ "ADMINED COURSE: User is not registered in administrated course: "
          , show courses, " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> fail $ join
            [ "ADMINED COURSES: Group ", show gk, " Assignment ", show ak
            , " Submission ", show sk
            ]
          Left ck -> do
            assertTrue (elem ck courses) $ join
              [ "ADMINED COURSES: The course key was not administrated by the user or associated for the group "
              , " admin ", show a, " ", show ck
              ]
    -- Check if the user of the assignment attends a group that
    -- the admin administrates. Check if the submission is administrated by the admin
    -- checks if the submission is administrated by the admin.
    checkAdminedGroup  a sks = runPersistCmd $ do
      adminedGroups <- (map fst) <$> administratedGroups a
      forM_ sks $ \sk -> do
        u <- usernameOfSubmission sk
        isInGroup <- or <$> mapM (isUserInGroup u) adminedGroups
        assertTrue isInGroup $ join
          [ "ADMINED GROUP: User is not registered in administrated groups:"
          , show adminedGroups, " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> do
            assertTrue (elem gk adminedGroups) $ join
              [ "ADMINED GROUP: The group key was not administrated by the user "
              , show a, " ", show gk
              ]
          Left ck -> fail $ join
            [ "ADMINED GROUP: Course ", show ck, " Assignment ", show ak
            , " Submission ", show sk
            ]
    checkRelatedCourse a sks = runPersistCmd $ do
      groups <- map fst <$> administratedGroups a
      courses <- (++) <$> mapM courseOfGroup groups <*> (map fst <$> administratedCourses a)
      forM_ sks $ \sk -> do
        u <- usernameOfSubmission sk
        isNotInGroup <- (not . or) <$> mapM (isUserInGroup u) groups
        assertTrue isNotInGroup $ join
          [ "RELATED COURSE: User is registered in administrated group: ", show groups
          , " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> fail $ join
            [ "RELATED COURSE: Group ", show gk, " Assignment ", show ak
            , " user: ", show u, " admin: ", show a
            ]
          Left ck -> do
            assertTrue (elem ck courses) $ join
              [ "RELATED COURSE: The course key was not administrated by the user "
              , show a, " ", show ck, " student ", show u, " submission ", show sk
              ]

deleteIncomingFeedbackTest = test $ testCase "Delete incoming feedbacks" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  ss <- submissions 1500 us as
  quick 1000 $ do
    ((_u,_ak),sk) <- pick $ elements ss
    sks <- runPersistCmd $ (map fst <$> testFeedbacks)
    case sk `elem` sks of
      True  -> checkIfCanBeDeleted   sk
      False -> checkIfCanBeCommented sk
  where
    checkIfCanBeDeleted sk = do
      join $ runPersistCmd $ do
        deleteTestFeedbacks sk
        cks <- map fst <$> testFeedbacks
        return $ do
          assertFalse (sk `elem` cks) ("Feedback was not deleted: " ++ show sk)

    checkIfCanBeCommented sk = do
      feedback <- pick $ Gen.testFeedbackInfo
      join $ runPersistCmd $ do
        insertAndFinalizeTestFeedback sk feedback
        cks <- map fst <$> testFeedbacks
        return $ do
          assertTrue (sk `elem` cks) "There was no feedback"
{-
-- All the notifications for comments returns the given comment key, and no feedback key
saveCommentNotificationTest = test $ testCase "Comment notifications" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  cks <- comments 1500 (map snd ss)
  let now = utcTimeConstant
  quick 1000 $ do
    (ck,sk) <- pick $ elements cks
    notif <- pick $ Gen.notifications
    nk <- runPersistCmd $ saveCommentNotification ck notif
    (mck,mfk,users) <- runPersistCmd $ do
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      return (mck,mfk,users)
    assertEquals (Just ck) mck   "Commented notification has no comment key."
    assertEquals Nothing   mfk   "Commented notification has a notification key."
    assertEquals []        users "Commented notification had a non-empty users list."

-- All the notifications for feedback returns the given feedback key, and no comment key
saveFeedbackNotificationTest = test $ testCase "Feedback notifications" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  fs <- feedbacks 1500 (map snd ss)
  quick 1000 $ do
    (fk,sk) <- pick $ elements fs
    notif <- pick $ Gen.notifications
    nk <- runPersistCmd $ saveFeedbackNotification fk notif
    (mck,mfk,users) <- runPersistCmd $ do
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      return (mck,mfk,users)
    assertEquals Nothing   mck   "Feedback notification has a comment key."
    assertEquals (Just fk) mfk   "Feedback notification has no notification key."
    assertEquals []        users "Feedback notification had a non-empty users list."

-- All the system notification does not returns an feedback or comment key, and they are
-- associated to the users
attachedSystemNotificationTest = test $ testCase "System notifications with attached users" $ do
  reinitPersistence
  us <- users 400
  ns <- systemNotifications 1500
  quick 1000 $ do
    user <- pick $ elements us
    nk <- pick $ elements ns
    (mck,mfk,users,nks) <- runPersistCmd $ do
      attachNotificationToUser user nk
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      nks   <- notificationsOfUser user
      return (mck,mfk,users,nks)
    assertEquals Nothing   mck     "System notification has no comment key."
    assertEquals Nothing   mfk     "System notification has no notification key."
    assertTrue   (elem user users) "System notification is not associated with the selected user on the notification side."
    assertTrue   (elem nk   nks)   "System notification is not associated with the selected user on the user side."

attachedNotificationTest = test $ testCase "Notifications with attached users" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  cks <- comments 200 (map snd ss)
  fs <- feedbacks 200 (map snd ss)
  cns <- map fst <$> commentNotifications 600 (map fst cks)
  fns <- map fst <$> feedbackNotifications 600 (map fst fs)
  sns <- systemNotifications 600
  let ns = cns ++ fns ++ sns
  let sfns = sns ++ fns
  let scns = sns ++ cns
  quick 1000 $ do
    user <- pick $ elements us
    nk   <- pick $ elements ns
    (mck,mfk,users,nks) <- runPersistCmd $ do
      attachNotificationToUser user nk
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      nks   <- notificationsOfUser user
      return (mck,mfk,users,nks)
    assertTrue   (elem user users) "System notification is not associated with the selected user on the notification side."
    assertTrue   (elem nk   nks)   "System notification is not associated with the selected user on the user side."
    case mck of
      Nothing -> assertTrue (elem nk sfns) "A non commented notification had a comment."
      Just ck -> assertTrue (elem nk cns) "A commented notification lost its comment key."
    case mfk of
      Nothing -> assertTrue (elem nk scns) ("A non feedback notification had a feedback." ++ show nk)
      Just fk -> assertTrue (elem nk fns) "A feedback notification lost its feedback key."
-}

-- * Run persistent command

-- TODO: FIX this dirty hack to instatiate only once the persistent layer
persistRef :: IORef Interpreter
persistRef = unsafePerformIO $ newIORef undefined

createInterpreter :: IO ()
createInterpreter = do
  interp <- createPersistInterpreter defaultConfig
  writeIORef persistRef interp

getPersistInterpreter :: IO Interpreter
getPersistInterpreter = readIORef persistRef

runPersistCmd :: Persist a -> PropertyM IO a
runPersistCmd m = do
  interp <- run getPersistInterpreter
  x <- run $ runPersist interp m
  case x of
    Left msg -> fail msg >> return undefined
    Right x  -> return x

runPersistIOCmd :: Persist a -> IO a
runPersistIOCmd m = do
  interp <- getPersistInterpreter
  x <- runPersist interp m
  case x of
    Left msg -> fail msg >> return undefined
    Right x  -> return x

startDate :: UTCTime
startDate = read "2013-03-01 12:00:00"

endDate :: UTCTime
endDate = read "2013-03-30 12:00:00"


tests = do
  ioTest "Init persist interpreter" createInterpreter
  propertyTests
  massTests
  complexTests


propertyTests = group "Persistence Layer QuickCheck properties" $ do
  add initPersistenceLayer
  add $ testProperty "Assignment Save and Load" $ monadicIO assignmentSaveAndLoad
  add $ testProperty "Course Save and Load" $ monadicIO courseSaveAndLoad
  add $ testProperty "Group Save and Load" $ monadicIO groupSaveAndLoad
  add $ testProperty "Course Assignment Save and Load" $ monadicIO courseAssignmentSaveAndLoad
  add $ testProperty "Group Assignment Save and Load" $ monadicIO groupAssignmentSaveAndLoad
  add $ testProperty "User Save and Load" $ monadicIO (pick Gen.users >>= userSaveAndLoad)
  add $ testProperty "Multiple groups for course" $ monadicIO multipleGroupsForCourse
  add $ testProperty "Submission Save and Load" $ monadicIO saveAndLoadSubmission
  add $ testProperty "Assignment and user of submission" $ monadicIO assignmentAndUserOfSubmission
  add $ testProperty "Comment save and load" $ monadicIO saveAndLoadComment
  add $ testProperty "Evaluation save and load" $ monadicIO evaluationGroupSaveAndLoad
  add cleanUpPersistence


massTests = group "Persistence Layer Mass tests" $ do
  test initPersistenceLayer
  test massTest
  test massTestParallel
  test cleanUpPersistence


complexTests = group "Persistence Layer Complex tests" $ do
  test initPersistenceLayer
  userAssignmentKeyTests
  courseOrGroupAssignmentTest
  groupDescriptionTest
  submissionDescTest
  courseNameAndAdminsTest
  submissionListDescTest
  lastEvaluationTest
  submissionDetailsDescTest
  submissionTablesTest
  userSubmissionDescTest
  courseKeysTest
  assignmentKeyTest
  filterSubmissionsTest
  modifyAssignmentsTest
  modifyEvaluationTest
  deleteUsersFromCourseTest
  deleteUsersFromCourseNegativeTest
  unsubscribeFromSubscribedGroupsTest
  saveLoadAndModifyTestScriptsTest
  saveLoadAndModifyTestCasesTest
  userFileHandlingTest
  userOverwriteFileTest
  testJobCreationTest
  incomingFeedbacksTest
  finalizeFeedbacksTest
  deleteIncomingFeedbackTest
  openSubmissionsTest
  assessmentTests
  unevaluatedScoresTests
  scoreEvaluationTests
{- XXX
  saveCommentNotificationTest
  saveFeedbackNotificationTest
  attachedSystemNotificationTest
  attachedNotificationTest
-}
  test cleanUpPersistence

monadicProperty gen prop = monadicIO (forAllM gen prop)

initPersistenceLayer = testCase "Initialization" $ do
  init <- createPersistInit defaultConfig
  initPersist init

cleanUpPersistence = testCase "Clean up" $ do
  init <- createPersistInit defaultConfig
  tearDown init

-- Fails if the two given list does not represent the same set
assertSetEquals :: (Monad m, Show a, Eq a, Ord a) => [a] -> [a] -> String -> m ()
assertSetEquals xs ys msg = assertEquals
 (Set.fromList xs) (Set.fromList ys) (concat [msg, " ", show xs, " ", show ys])

-- The test will fail with the given message, if the given values are different
assertEquals :: (Monad m, Eq a) => a -> a -> String -> m ()
assertEquals x y msg
  | x == y    = return ()
  | otherwise = fail msg

-- The test will fail with the given message, if the boolean value is false
assertTrue :: (Monad m) => Bool -> String -> m ()
assertTrue True  _   = return ()
assertTrue False msg = fail msg

-- The test will fail with the given message, if the boolean value is true
assertFalse :: (Monad m) => Bool -> String -> m ()
assertFalse False _ = return ()
assertFalse True msg = fail msg

-- The test will fail with the given message, if the list is null
assertNonEmpty :: (Monad m) => [a] -> String -> m ()
assertNonEmpty [] msg = fail msg
assertNonEmpty _ _ = return ()

-- The test will fail with the given message, if the list is not empty
assertEmpty :: (Monad m) => [a] -> String -> m ()
assertEmpty [] _ = return ()
assertEmpty _ msg = fail msg

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM m a = do
  x <- m
  when x a

runPersistTestSet t = do
  test initPersistenceLayer
  test t
  test cleanUpPersistence

-- * Helpers

utcTimeConstant :: UTCTime
utcTimeConstant = read "2015-08-27 17:08:58 UTC"

-- * Consistent data generation

createTestData n = do
  createInterpreter
  let noOfUsers = 1100
  let noOfCourses = 14 * n
  let noOfGroups = 6 * noOfCourses
  print "Init persistence ..."
  initPersistence
  print "Creating users ..."
  us <- users (1100 * n)
  print "Creating courses ..."
  cs <- courses (14 * n)
  print "Creating groups ..."
  gs <- groups noOfGroups cs

  print "Subscribing users to groups ..."
  quick (4 * noOfUsers) $ do
    gk <- pick $ elements gs
    ck <- runPersistCmd $ courseOfGroup gk
    u  <- pick $ elements us
    runPersistCmd $ subscribe u ck gk

  as <- courseAndGroupAssignments (8 * noOfCourses) (8 * noOfGroups) cs gs
  let noOfSubmissions = 15000 * n
  print "Creating subscriptions ..."
  quick noOfSubmissions $ do
    u <- pick $ elements us
    aks <- runPersistCmd $ userAssignmentKeyList u
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      s  <- pick $ Gen.submissions startDate
      sk <- runPersistCmd $ saveSubmission ak u s
      return ()

  print "Creating comments ..."
  quick (3 * noOfSubmissions) $ do
    u <- pick $ elements us
    aks <- runPersistCmd $ userAssignmentKeyList u
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      sks <- runPersistCmd $ userSubmissions u ak
      when (not $ null sks) $ do
        sk <- pick $ elements sks
        c  <- pick $ Gen.comments startDate
        ck <- runPersistCmd $ saveComment sk c
        return ()

  print "Creating evaluations ..."
  quick (4 * noOfSubmissions) $ do
    u <- pick $ elements us
    aks <- runPersistCmd $ userAssignmentKeyList u
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      sks <- runPersistCmd $ userSubmissions u ak
      when (not $ null sks) $ do
        sk <- pick $ elements sks
        cfg <- evaluationConfigForSubmission sk
        e <- pick $ Gen.evaluations cfg
        runPersistCmd $ saveSubmissionEvaluation sk e
        return ()
