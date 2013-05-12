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

import Data.Time.Clock
import System.Directory (removeDirectoryRecursive)

errorLogger = Logger {
    log = \e msg -> case e of
      ERROR -> error msg
      _     -> return ()
  }

context :: IO ServiceContext
context = do
  container <- ioUserContainer
  serviceContext noSqlDirPersist container errorLogger

adminUserState = UserState {
    user = Username "admin"
  , page = P.Home
  , name = "Admin"
  , role = E.Admin
  , token = "token"
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
  , courseAndGroupAssignmentTest
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
  (_,state) <- runStory c UserNotLoggedIn $ login (Username "student") "user" "token"
  assertUserState state student
  (_,state) <- runStory c state $ logout
  case state of
    UserState {} -> error "User is remained logged in"
    UserNotLoggedIn -> return ()

courseTest = testCase "Create Course" $ do
  c <- context
  let r = E.Course {
      courseName = "Functional programming"
    , courseDesc = "Everything about FP"
    , courseEvaulation = Scale
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

courseAndGroupAssignmentTest = testCase "Course and group assignments" $ do
  c <- context
  str <- getCurrentTime
  end <- getCurrentTime
  let ca = E.Assignment "cname" "cexercise" "ctest" Normal str end
      ga = E.Assignment "gname" "gexercise" "gtest" Normal str end
      c1  = E.Course "FP" "FP-DESC" Scale
      c2  = E.Course "MA" "MA-DESC" Scale
      g1  = E.Group  "G1" "G1-DESC" Scale
      g2  = E.Group  "G2" "G2-DESC" Scale
  runStory c adminUserState $ createUser adminUser "password"
  (_,l) <- runStory c UserNotLoggedIn $ login (E.Username "admin") "password" "token"
  ((a1,a2,as),_) <- runStory c l $ do
    ck1 <- createCourse c1
    ck2 <- createCourse c2
    gk1 <- createGroup ck1 g1
    gk2 <- createGroup ck2 g2
    a1 <- createGroupAssignment gk1 ga
    a2 <- createCourseAssignment ck2 ca
    subscribeToGroup gk1
    subscribeToGroup gk2
    as <- userAssignments
    return (a1,a2,as)
  let as' = map fst as
  assertBool "Assignment does not found in the assignment list" ([a1,a2] == as' || [a2,a1] == as')
