module Test.Quick.Persistence (
    tests
  , massTests
  , complexTests
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Concurrent (forkIO)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Time
import System.Directory

import Control.Monad.Transaction.TIO
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDir

import qualified Test.Quick.EntityGen as Gen

import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Types
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (Test(..), testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

{- Mass test of the persistence layer -}

-- Properties

-- Load and save property. The save and a load of
-- the given data should be the same.
saveAndLoadIdenpotent :: (Eq v, Show v) => String -> (v -> TIO k) -> (k -> TIO v) -> Gen v -> PropertyM IO k
saveAndLoadIdenpotent name save load gen = do
  v <- pick gen
  key <- runPersistCmd $ save v
  v'  <- runPersistCmd $ load key
  assertEquals v v' (name ++ ": Save and load is not idenpotent")
  return key

-- Modification property. The saved, modificated and a load
-- of the given data should be the same
modification :: (Eq v, Show v) => String -> (v -> TIO k) -> (k -> v -> TIO ()) -> (k -> TIO v) -> Gen v -> PropertyM IO k
modification name save modify load gen = do
  k <- saveAndLoadIdenpotent name save load gen
  v <- pick gen
  runPersistCmd $ modify k v
  v' <- runPersistCmd $ load k
  assertEquals v v' (name ++ ": Modifed and load was different")
  return k

persist = noSqlDirPersist

assignmentSaveAndLoad = saveAndLoadIdenpotent
  "Assignment"
  (saveAssignment persist)
  (loadAssignment persist)
  (Gen.assignments startDate endDate)

courseSaveAndLoad = saveAndLoadIdenpotent
  "Course"
  (saveCourse persist)
  (loadCourse persist)
  Gen.courses

groupSaveAndLoad = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse persist)  (loadCourse persist) Gen.courses
  gk <- saveAndLoadIdenpotent "Group"  (saveGroup persist ck) (loadGroup persist) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse persist ck
  assertEquals [gk] gks "Group keys were different"
  ck' <- runPersistCmd $ courseOfGroup persist gk
  assertEquals ck ck' "Course keys were different"

courseAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse persist) (loadCourse persist) Gen.courses
  saveAndLoadIdenpotent
    "Assignment"
    (saveCourseAssignment persist key)
    (loadAssignment persist)
    (Gen.assignments startDate endDate)

groupAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse persist) (loadCourse persist) Gen.courses
  key1 <- saveAndLoadIdenpotent "Groups" (saveGroup persist key) (loadGroup persist) Gen.groups
  saveAndLoadIdenpotent
    "Assignment"
    (saveGroupAssignment persist key1)
    (loadAssignment persist)
    (Gen.assignments startDate endDate)

userSaveAndLoad u = do
  saveAndLoadIdenpotent "User"
    (\usr -> saveUser persist usr) (const (loadUser persist (u_username u))) (return u)

createOrLoadUser u = do
  exist <- runPersistCmd $ doesUserExist persist (u_username u)
  case exist of
    True  -> return ()
    False -> userSaveAndLoad u
  return u

multipleGroupsForCourse = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse persist) (loadCourse persist)  Gen.courses
  gk1 <- saveAndLoadIdenpotent "Group" (saveGroup persist ck) (loadGroup persist) Gen.groups
  gk2 <- saveAndLoadIdenpotent "Group" (saveGroup persist ck) (loadGroup persist) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse persist ck
  assertEquals (Set.fromList gks) (Set.fromList [gk1,gk2]) "Groups key set were different"

saveAndLoadSubmission = do
  ak <- groupAssignmentSaveAndLoad
  u <- pick Gen.users
  createOrLoadUser u
  sk <- saveAndLoadIdenpotent "Submission"
          (saveSubmission persist ak (u_username u)) (loadSubmission persist) (Gen.submissions startDate)
  return (ak,u,sk)

assignmentAndUserOfSubmission = do
  (ak, u, sk) <- saveAndLoadSubmission
  ak' <- runPersistCmd $ assignmentOfSubmission persist sk
  assertEquals ak ak' "Assertion keys were different"
  un <- runPersistCmd $ usernameOfSubmission persist sk
  assertEquals (u_username u) un "Username were different"

saveAndLoadComment = do
  (ak, u, sk) <- saveAndLoadSubmission
  ck <- saveAndLoadIdenpotent "Comment" (saveComment persist sk) (loadComment persist) (Gen.comments startDate)
  sk' <- runPersistCmd $ submissionOfComment persist ck
  assertEquals sk sk' "Submission keys were different"

evaluationConfigForSubmission sk = do
  ak <- runPersistCmd $ assignmentOfSubmission persist sk
  s <- runPersistCmd $ loadSubmission persist sk
  key <- runPersistCmd $ courseOrGroupOfAssignment persist ak
  either
    (runPersistCmd . fmap courseEvalConfig . loadCourse persist)
    (runPersistCmd . fmap groupEvalConfig  . loadGroup  persist)
    key

evaluationGroupSaveAndLoad = do
  (ak, u, sk) <- saveAndLoadSubmission
  cfg <- evaluationConfigForSubmission sk
  saveAndLoadIdenpotent
    "Evaluation" (saveEvaluation persist sk) (loadEvaluation persist) (Gen.evaluations cfg)

success n = stdArgs { maxSuccess = n, chatty = False }

massTest = testCase "Mass Test" massPersistenceTest

massTest2 = testCase "Mass Test Parallel" $ do
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
    gk <- saveAndLoadIdenpotent "Group" (saveGroup persist ck) (loadGroup persist) (Gen.groups)
    run $ insertListRef list gk
  listInRef list

-- Generate and store the given numner of assignment and assign them to random groups,
-- from the given list, returns all the created assignment keys
groupAssignmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assignment"
      (saveGroupAssignment persist gk) (loadAssignment persist) (Gen.assignments startDate endDate)
    run $ insertListRef list ak
  listInRef list

-- Generate and store the given numner of assignment and assign them to random courses,
-- from the given list, returns all the created assignment keys
courseAssignmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assignment"
      (saveCourseAssignment persist gk) (loadAssignment persist) (Gen.assignments startDate endDate)
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

-- SubmissionInfoList is a list from username, assignment-key and the submission-key for them.
-- Interpretation: The submission information about which user submitted which submission
-- for the given assignment.
type SubmissionInfoList = [((Username,AssignmentKey),SubmissionKey)]

-- Throws away the assignment and the username from the submission information
submissionKeys :: SubmissionInfoList -> [SubmissionKey]
submissionKeys = (fmap snd)

-- Generate and story the given number of submissions, for the randomly selected
-- user and assignment. Returns all the created submission keys with the associated
-- username and assignment
submissions :: Int -> [Username] -> [AssignmentKey] -> IO SubmissionInfoList
submissions n us as = do
  list <- createListRef
  quick n $ do
    u <- pick $ elements us
    ak <- pick $ elements as
    sk <- saveAndLoadIdenpotent "Submission"
      (saveSubmission persist ak u) (loadSubmission persist) (Gen.submissions startDate)
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
      (saveEvaluation persist sk) (loadEvaluation persist) (Gen.evaluations cfg)
    run $ insertListRef list ek
  listInRef list

massPersistenceTest = do
  cs <- courses 100
  gs <- groups 250 cs
  gas <- groupAssignmentGen 400 gs
  cas <- courseAssignmentGen 400 cs
  let as = gas ++ cas
  us <- users 300
  ss <- submissionKeys <$> submissions 500 us as
  evaluations 400 ss
  return ()


quick n p = check $ quickCheckWithResult (success n) $ monadicIO p

check m = do
  x <- m
  case x of
    (Success {}) -> return ()
    _            -> fail "quickcheck has failed"

-- The test are at very high level, we must see if basic load
-- properties are hold.

reinitPersistence = do
  removeDirectoryRecursive "data"
  initPersistence persist

courseAndGroupAssignments cn gn cs gs = do
  cas <- courseAssignmentGen cn cs
  gas <- groupAssignmentGen gn gs
  return (cas ++ gas)

-- User can register course and groups and these groups and courses can have assignments.
-- The user can subscribe to groups and course, and it is necessary to him to
-- see the assignment of the groups and courses, and only the assignment of the
-- courses that the user registered, others no
userAssignmentKeyTests = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 300
  quick 300 $ do
    u <- pick $ elements us
    gk <- pick $ elements gs
    ck <- runPersistCmd $ courseOfGroup persist gk
    runPersistCmd $ subscribe persist u ck gk
    gas <- runPersistCmd $ groupAssignments persist gk
    cas <- runPersistCmd $ courseAssignments persist ck
    let uas = gas ++ cas
    as <- runPersistCmd $ fmap (maybe [] id) $ userAssignmentKeys persist u
    when (null as) $ assertTrue (null gas)
      "Group has assignment, but user does not see it"
    unless (or [null uas, null as]) $ assertTrue
      (not . Set.null $ Set.intersection (Set.fromList uas) (Set.fromList as))
      (join [
          "User assignment for a given course and group was not found. User:", show u
        , "Group and course assignments: ", show uas, " User assignments:", show as
        ])

-- Every assignment has a group or a course
courseOrGroupAssignmentTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  gas <- groupAssignmentGen 150 gs
  cas <- courseAssignmentGen 150 cs
  let as = cas ++ gas
  quick 500 $ do
    ak <- pick $ elements as
    k <- runPersistCmd $ courseOrGroupOfAssignment persist ak
    either
      (\c -> assertTrue (elem c cs) "Course is not in the courses")
      (\g -> assertTrue (elem g gs) "Group is not in the groups")
      k

-- Group description can be created from any group
groupDescriptionTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  us <- users 300
  quick 300 $ do
    groupAdmin <- pick $ elements us
    gk         <- pick $ elements gs
    runPersistCmd $ createGroupAdmin persist groupAdmin gk
    groupAdmins <- runPersistCmd $ groupAdmins persist gk
    assertTrue (elem groupAdmin groupAdmins) "Group admin was not in the group admins"
  quick 500 $ do
    gk <- pick $ elements gs
    (gk', desc) <- runPersistCmd $ groupDescription persist gk
    assertEquals gk gk' "Group keys are different"
    assertTrue (not . null . gName $ desc) "Name was empty"
    admins <- runPersistCmd $ groupAdmins persist gk
    assertTrue (length (gAdmins desc) == length admins) "Group admin numbers was different"

-- Every submission has some kind of description
submissionDescTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  us <- users 300
  as <- courseAndGroupAssignments 150 150 cs gs
  ss <- submissionKeys <$> submissions 500 us as
  quick 500 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDesc persist sk
    assertNonEmpty (eGroup desc) "Group name was empty"
    assertNonEmpty (eStudent desc) "Student name was empty"
    assertNonEmpty (eSolution desc) "Solution was empty"
    assertNonEmpty (eAssignmentTitle desc) "Assignment title was empty"
    assertEmpty (eComments desc) "The comment list was not empty"

-- Every assignment must have a course name and the
-- dedicated users must be returned as admins
-- for the given course
courseNameAndAdminsTest = do
  reinitPersistence
  cs <- courses 100
  us <- users 150
  gs <- groups 100 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  quick 400 $ do
    u <- pick $ elements us
    c <- pick $ elements cs
    a <- pick $ elements as
    (name, admins) <- runPersistCmd $ courseNameAndAdmins persist a
    ek <- runPersistCmd $ courseOrGroupOfAssignment persist a
    admins' <- either
      (runPersistCmd . courseAdmins persist)
      (runPersistCmd . groupAdmins persist)
      ek
    assertNonEmpty name "Course name was empty"
    assertTrue (length admins' == length admins) "Admin numbers are different"

-- Every assignment and an associated user has a submission list,
-- which contains information about the submissions posted to the given assignment
-- by the user
submissionListDescTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 300
  ss <- submissionKeys <$> submissions 400 us as
  quick 500 $ do
    u <- pick $ elements us
    a <- pick $ elements as
    desc <- runPersistCmd $ submissionListDesc persist u a
    assertNonEmpty (slGroup desc) "Group was empty"
    assertNonEmpty (slAssignmentText desc) "Assignment was empty"
    assertEmpty (slTeacher desc) "There was teachers to the group"

-- Allways the last evaluation is valid for the submission.
lastEvaluationTest = do
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
  usr <- runPersistCmd $ loadUser persist u
  pre (atLeastCourseAdmin . u_role $ usr)
  c <- pick $ elements cs
  runPersistCmd $ createCourseAdmin persist u c

createGroupAdmins n us gs = quick n $ do
  u <- pick $ elements us
  g <- pick $ elements gs
  runPersistCmd $ createGroupAdmin persist u g

-- Every submission has a description, this description must be loaded
submissionDetailsDescTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 400
  createCourseAdmins 200 us cs
  createGroupAdmins 200 us gs
  ss <- submissionKeys <$> submissions 400 us as
  quick 1000 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDetailsDesc persist sk
    assertNonEmpty (sdGroup desc) "Group name was empty"
    forM (sdTeacher desc) $ \t -> assertNonEmpty t "Admin name was empty"
    assertNonEmpty (sdAssignment desc) "Description was empty"
    assertNonEmpty (sdStatus desc) "Status was empty"
    assertNonEmpty (sdSubmission desc) "Submission text was empty"
    forM (sdComments desc) $ \c -> assertNonEmpty (comment c) "Comment was empty"

-- If the user administrates courses or groups, submission information about the
-- submission of the group or course attendees. The number of the tables are same as
-- the number of the groups and courses administrated by this user
submissionTablesTest = do
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
    c <- runPersistCmd $ courseOfGroup persist g
    runPersistCmd $ subscribe persist u c g

  quick 1000 $ do
    u <- pick $ elements us
    acs <- runPersistCmd $ administratedCourses persist u
    ags <- runPersistCmd $ administratedGroups  persist u
    ts  <- runPersistCmd $ submissionTables     persist u
    assertEquals ((length acs) + (length ags)) (length ts)
      "The submission table number is different from the administrated courses and groups"
    forM ts $ \t -> do
      assertNonEmpty (stCourse t) "Course name was empty"
      assertTrue (stNumberOfAssignments t >= 0) "Number of assignments was negative"
      assertNonEmpty (show . stEvalConfig $ t) "Evaluation config was empty"
      assertTrue (length (stAssignments t) >= 0) "Invalid assignment list"
      forM (stUsers t) $ usernameCata (\u -> assertNonEmpty u "Username was empty")
      assertTrue (length (stUserLines t) >= 0) "Invalid user line number"

-- The user can have submissions for the given assignment, and information can be
-- calculated about these submissions
userSubmissionDescTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 250 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 300
  asgMap <- Map.fromList <$> submissions 750 us as
  quick 1000 $ do
    u <- pick $ elements us
    a <- pick $ elements as
    desc <- runPersistCmd $ userSubmissionDesc persist u a
    assertNonEmpty (usCourse desc) "Course was empty"
    assertNonEmpty (usAssignmentName desc) "Assignment name was empty"
    assertNonEmpty (usStudent desc) "Student name was empty"
    ss <- runPersistCmd $ userSubmissions persist u a
    assertEquals
      (Set.fromList ss) (Set.fromList (map fst3 (usSubmissions desc)))
      "Submission numbers were different"
  where
    fst3 (a,_,_) = a

-- All the saved course must have a key and these
-- course keys must be listed
courseKeysTest = do
  reinitPersistence
  savedKeys  <- Set.fromList <$> courses 100
  loadedKeys <- Set.fromList <$> (runPersistIOCmd $ courseKeys persist)
  assertEquals savedKeys loadedKeys "Saved and loaded courses were different"
  savedKeys2  <- Set.fromList <$> courses 50
  loadedKeys2 <- Set.fromList <$> (runPersistIOCmd $ courseKeys persist)
  assertTrue (Set.isSubsetOf loadedKeys loadedKeys2) "Not all old course keys were in the loaded set"
  assertTrue (Set.isSubsetOf savedKeys2 loadedKeys2) "New course keys were not in the loaded set"

-- All the saved assignment must have a key and these keys must be listed
assignmentKeyTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  saved  <- Set.fromList <$> (courseAndGroupAssignments 150 150 cs gs)
  loaded <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys persist)
  assertEquals saved loaded "Saved and loaded assignment keys were different"
  saved2  <- Set.fromList <$> (courseAndGroupAssignments 50 50 cs gs)
  loaded2 <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys persist)
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all assignment keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New assignment keys were not in the loaded set"

-- All the saved submissions must have a key and these keys must be listed
filterSubmissionsTest = do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 400
  saved  <- (Set.fromList . submissionKeys) <$> (submissions 500 us as)
  loaded <- (Set.fromList . map fst) <$> (runPersistIOCmd $ filterSubmissions persist (\_ _ -> True))
  assertEquals saved loaded "Saved and loaded submission keys were different"
  saved2  <- (Set.fromList . submissionKeys) <$> (submissions 100 us as)
  loaded2 <- (Set.fromList . map fst) <$> (runPersistIOCmd $ filterSubmissions persist (\_ _ -> True))
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all submission keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New submission keys were not in the loaded set"

-- Users must be able to change password and reamain loginable
-- TODO: Investigate
updatePwdTest = do
  let pwd = "password"
  quick 1000 $ do
    u <- pick Gen.users
    let username = u_username u
    exist <- runPersistCmd $ doesUserExist persist username
    pre (not exist)
    runPersistCmd $ saveUser persist u
--    loginable <- runPersistCmd $ canUserLogin persist username pwd
--    assertTrue loginable "User is not loginable"
    p <- pick Gen.passwords
--    runPersistCmd $ updatePwd persist username
--    loginable <- runPersistCmd $ canUserLogin persist username p
--    assertTrue loginable "User is not loginable #2"
    return ()

-- Users can not login in using invalid password
userCanLoginTest = do
  let wrongPwd = "wrongpwd"
  quick 1000 $ do
    u <- pick Gen.users
    let username = u_username u
    exist <- runPersistCmd $ doesUserExist persist username
    pre (not exist)
    runPersistCmd $ saveUser persist u
--    loginable <- runPersistCmd $ canUserLogin persist username pwd
--    assertTrue loginable "User is not loginable"
--    loginable <- runPersistCmd $ canUserLogin persist username 
--    assertFalse loginable "User could login with invalid password"
    return ()

-- Modified assignments must be untouched after loading them
modifyAssignmentsTest = do
  cs <- courses 100
  gs <- groups 150 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  quick 1000 $ do
    ak <- pick $ elements as
    a0 <- runPersistCmd $ loadAssignment persist ak
    a  <- pick (Gen.assignments startDate endDate)
    runPersistCmd $ modifyAssignment persist ak a
    a1 <- runPersistCmd $ loadAssignment persist ak
    assertEquals a a1 "Modified and loaded assignments were differents"

-- Modified evaluations must be untouched after loading them
modifyEvaluationTest = do
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 200
  ss <- submissionKeys <$> (submissions 400 us as)
  es <- evaluations 600 ss
  quick 1000 $ do
    ek <- pick $ elements es
    sk <- runPersistCmd $ submissionOfEvaluation persist ek
    cfg <- evaluationConfigForSubmission sk
    e <- pick $ Gen.evaluations cfg
    runPersistCmd $ modifyEvaluation persist ek e
    e1 <- runPersistCmd $ loadEvaluation persist ek
    assertEquals e e1 "Modified and loaded evaluations were different"

runPersistCmd :: TIO a -> PropertyM IO a
runPersistCmd m = do
  x <- run $ runPersist m
  case x of
    Left msg -> fail msg >> return undefined
    Right x  -> return x

runPersistIOCmd :: TIO a -> IO a
runPersistIOCmd m = do
  x <- runPersist m
  case x of
    Left msg -> fail msg >> return undefined
    Right x  -> return x

startDate :: UTCTime
startDate = read "2013-03-01 12:00:00"

endDate :: UTCTime
endDate = read "2013-03-30 12:00:00"

tests = testGroup "Persistence Layer QuickCheck properties" [
    initPersistenceLayer
  , testProperty "Assignment Save and Load" $ monadicIO assignmentSaveAndLoad
  , testProperty "Course Save and Load" $ monadicIO courseSaveAndLoad
  , testProperty "Group Save and Load" $ monadicIO groupSaveAndLoad
  , testProperty "Course Assignment Save and Load" $ monadicIO courseAssignmentSaveAndLoad
  , testProperty "Group Assignment Save and Load" $ monadicIO groupAssignmentSaveAndLoad
--  , testProperty "User Save and Load" $ monadicIO (pick Gen.users >>= userSaveAndLoad)
  , testProperty "Multiple groups for course" $ monadicIO multipleGroupsForCourse
  , testProperty "Submission Save and Load" $ monadicIO saveAndLoadSubmission
  , testProperty "Assignment and user of submission" $ monadicIO assignmentAndUserOfSubmission
  , testProperty "Comment save and load" $ monadicIO saveAndLoadComment -- }
  , testProperty "Evaluation save and load" $ monadicIO evaluationGroupSaveAndLoad
  , cleanUpPersistence
  ]

massTests = testGroup "Persistence Layer Mass tests" [
    initPersistenceLayer
  , massTest
  , cleanUpPersistence
  ]

complexTests = testGroup "Persistence Layer Complex tests" [
    initPersistenceLayer
  , testCase "User assignment tests" $ userAssignmentKeyTests
  , testCase "Every assignment has a group or a course" $ courseOrGroupAssignmentTest
  , testCase "Group description can be created from any group" $ groupDescriptionTest
  , testCase "Every submission has some kind of description" $ submissionDescTest
  , testCase "Every assignment course must have a name and admins" $ courseNameAndAdminsTest
  , testCase "Every assignment and an associated user has a submission list" $ submissionListDescTest
  , testCase "Allways the last evaluation is valid for the submission" $ lastEvaluationTest
  , testCase "Every submission has a description" $ submissionDetailsDescTest
  , testCase "Submission tables" $ submissionTablesTest
  , testCase "The user can have submissions and information" $ userSubmissionDescTest
  , testCase "All the saved courses must have a key" $ courseKeysTest
  , testCase "All the saved assignments must have a key" $ assignmentKeyTest
  , testCase "All the saved submissions must have a key" $ filterSubmissionsTest
  , testCase "Users must be able to change password and reamain loginable" $ updatePwdTest
  , testCase "Modified assignments must be untouched after loading them" $ modifyAssignmentsTest
  , testCase "Modified evaluations must be untouched after loading them" $ modifyEvaluationTest
  , testCase "Users can not login in using invalid password" $ userCanLoginTest
  , cleanUpPersistence
  ]

monadicProperty gen prop = monadicIO (forAllM gen prop)

initPersistenceLayer = testCase "Initialization" $ do
  initPersistence noSqlDirPersist

cleanUpPersistence = testCase "Clean up" $ do
  removeDirectoryRecursive "data"

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
