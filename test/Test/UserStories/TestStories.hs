module Test.UserStories.TestStories (
    tests
  ) where

import Prelude hiding (log)
import Bead.Domain.Entities as E
import Bead.Controller.Logging
import Bead.Controller.UserStories as U
import Bead.Controller.ServiceContext
import Bead.Controller.Pages as P
import Bead.Persistence.NoSQLDir
import Bead.Persistence.Persist

import Test.HUnit hiding (Test(..))
import Test.Framework (Test(..), testGroup)
import Test.Framework.Providers.HUnit

import System.Directory (removeDirectoryRecursive)

errorLogger = Logger {
    log = \e msg -> case e of
      ERROR -> error msg
      _     -> return ()
  }

context :: IO ServiceContext
context = do
  container <- ioUserContainer
  return $ serviceContext noSqlDirPersist container errorLogger

adminUserState = UserState {
    user = Username "admin"
  , page = P.Home
  , name = "Admin"
  , role = E.Admin
  }


student = User {
    u_role = E.Student
  , u_username = (Username "student")
  , u_email = Email "student@university.com"
  , u_name = "Stu Dent"
  }

adminUser = User {
    u_role = E.Admin
  , u_username = (Username "admin")
  , u_email = Email "admin@university.com"
  , u_name = "Admin"
  }

-- * Test Tooles

runStory c u s = do
  e <- runUserStory c u s
  case e of
    Left ue -> error $ show ue
    Right a -> return a

assertUserState :: UserState -> User -> IO ()
assertUserState UserNotLoggedIn _ = error "User is not logged in"
assertUserState state usr = do
  assertBool "Invalid user is logged in" $ (user state) == (u_username usr)
  assertBool "Invalid person name: "     $ (name state) == (u_name usr)
  assertBool "Invalid role was loaded"   $ (role state) == (u_role usr)

-- * Tests

tests = testGroup "User Stories" [
    initPersist
  , register
  , loginAndLogout
  , courseTest
  , exerciseTest
  , cleanUpPersist
  ]

initPersist = testCase "Initalizing persistence layer" $ do
  setUp <- isPersistenceSetUp noSqlDirPersist
  assertBool "Persistence was set up" (not setUp)
  initPersistence noSqlDirPersist
  setUp <- isPersistenceSetUp noSqlDirPersist
  assertBool "Setting up persistence was failed" setUp


cleanUpPersist = testCase "Cleaning up persistence" $ do
  removeDirectoryRecursive "data"

register = testCase "User registration" $ do
  c <- context
  runStory c adminUserState $ createUser student "user"
  return ()

loginAndLogout = testCase "Login And Logout" $ do
  c <- context
  (_,state) <- runStory c UserNotLoggedIn $ login (Username "student") "user"
  assertUserState state student
  (_,state) <- runStory c state $ logout
  case state of
    UserState {} -> error "User is remained logged in"
    UserNotLoggedIn -> return ()

courseTest = testCase "Create Course" $ do
  c <- context
  let r = E.Course {
      courseCode = CourseCode "c"
    , courseName = "Functional programming"
    , courseDesc = "Everything about FP"
    }
  (k,state) <- runStory c adminUserState $ createCourse r
  assertUserState state adminUser
  (ks,state) <- runStory c adminUserState $ selectCourses (\_ _ -> True)
  assertUserState state adminUser
  assertBool "Create course key is not found" (elem k $ map fst ks)
  ((r',_),state) <- runStory c adminUserState $ U.loadCourse k
  assertUserState state adminUser
  assertBool "Loaded course differs from the created one" (r' == r)
  return ()

exerciseTest = testCase "Create exercise list its keys and load it" $ do
  c <- context
  let e = E.Exercise "exercise"
  (k,state) <- runStory c adminUserState $ createExercise e
  assertUserState state adminUser
  (ks,state) <- runStory c adminUserState $ selectExercises (\_ _ -> True)
  assertUserState state adminUser
  assertBool "Created exercise key is not found" (elem k $ map fst ks)
  (e',state) <- runStory c adminUserState $ U.loadExercise k
  assertUserState state adminUser
  assertBool "Loaded exercise differs from the created one" (e == e')

