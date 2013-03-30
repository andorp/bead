module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

-- Bead imports

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDir

-- Utils

import Data.Time.Clock
import System.Directory (removeDirectoryRecursive)

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_exercise
  , test_create_load_exercise
  , test_create_user
  , test_create_group_user
  , clean_up
  ]

persist = noSqlDirPersist

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
  e <- saveExercise persist (Assignment "This is an exercise" "This is the test" Normal str end)
  case e of
    Left e -> error e
    Right k -> return ()

test_create_load_exercise = testCase "Create and load exercise" $ do
  str <- getCurrentTime
  end <- getCurrentTime
  k <- liftE $ saveExercise persist (Assignment "This is an exercise" "This is the test" Normal str end)
  ks <- liftE $ filterExercises persist (\_ _ -> True)
  assertBool "Readed list of exercises was empty" (length ks > 0)
  assertBool "Written key was not in the list" (elem k (map fst ks))

test_create_user = testCase "Create user" $ do
  let uname = Username "ursula"
  let user = User {
        u_role     = Student
      , u_username = uname
      , u_email    = Email "ursula@gmail.com"
      , u_name     = "Ursula"
      }
      password = "password"
  liftE $ saveUser persist user password
  us <- liftE $ filterUsers persist (const True)
  assertBool "The filter did not find the user" (length us > 0)
  user1 <- liftE $ loadUser persist uname
  assertBool "Loading the registered user has failed" (user1 == user)
  let user2 = user { u_role = CourseAdmin }
  liftE $ updateUser persist user2
  user3 <- liftE $ loadUser persist uname
  assertBool "Updating and loading user has failed" (user3 == user2)

test_create_group_user = testCase "Create Course and Group with a user" $ do
  let username = Username "ursula"
      admin = Username "admin"
      adminUser = User {
          u_role = Admin
        , u_username = admin
        , u_email = Email "admin@gmail.com"
        , u_name = "admin"
        }
      password = "password"
  ck <- liftE $ saveCourse persist (Course "name" "desc")
  gk <- liftE $ saveGroup persist ck (Group "gname" "gdesc")
  gks <- liftE $ groupKeysOfCourse persist ck
  assertBool "Registered group was not found in the group list" (elem gk gks)
  liftE $ subscribe persist username ck gk
  isInGroup <- liftE $ isUserInGroup persist username gk
  assertBool "Registered user is not found" isInGroup
  isInCourse <- liftE $ isUserInCourse persist username ck
  assertBool "Registered user is not found" isInCourse
  liftE $ saveUser persist adminUser password
  liftE $ createCourseAdmin persist admin ck
  cs <- liftE $ administratedCourses persist admin
  assertBool "Course is not found in administrated courses" (elem ck (map fst cs))
  liftE $ createGroupProfessor persist admin gk
  gs <- liftE $ administratedGroups persist admin
  assertBool "Group is not found in administrated groups" (elem gk (map fst gs))


clean_up = testCase "Cleaning up" $ do
  -- We use background knowledge, to clean up
  removeDirectoryRecursive "data"
  return ()

-- * Tools

liftE :: IO (Erroneous a) -> IO a
liftE m = do
  x <- m
  case x of
    Left e -> error e
    Right y -> return y
