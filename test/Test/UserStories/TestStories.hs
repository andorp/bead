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
import Bead.Domain.Shared.Evaluation

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
  , timezone = UTC
  , status = Nothing
  }

student = User {
    u_role = E.Student
  , u_username = (Username "student")
  , u_email = Email "student@university.com"
  , u_name = "Stu Dent"
  , u_timezone = UTC
  , u_language = Language "hu"
  }

student2 = User {
    u_role = E.Student
  , u_username = (Username "student2")
  , u_email = Email "student@university.com"
  , u_name = "Stu Dent"
  , u_timezone = UTC
  , u_language = Language "hu"
  }


adminUser = User {
    u_role = E.Admin
  , u_username = (Username "admin")
  , u_email = Email "admin@university.com"
  , u_name = "Admin"
  , u_timezone = UTC
  , u_language = Language "hu"
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
  , saveAndLoadUserReg
  , cleanUpPersist
  ]

initPersist = testCase "Initalizing persistence layer" $ do
  setUp <- isPersistenceSetUp noSqlDirPersist
  assertBool "Persistence was set up" (not setUp)
  initPersistence noSqlDirPersist
  setUp <- isPersistenceSetUp noSqlDirPersist
  assertBool "Setting up persistence was failed" setUp

saveAndLoadUserReg = testCase "Save and load user reg data" $ do
  c <- context
  now <- getCurrentTime
  let u = UserRegistration "username" "e@e.com" "Family Name" "token" now
  (key,Registration) <- runStory c Registration $ U.createUserReg u
  (u', Registration)  <- runStory c Registration $ U.loadUserReg key
  assertBool "Saved and load user registration differs" (u' == u)

cleanUpPersist = testCase "Cleaning up persistence" $ do
  removeDirectoryRecursive "data"

register = testCase "User registration" $ do
  c <- context
  runStory c adminUserState $ createUser student
  return ()

loginAndLogout = testCase "Login And Logout" $ do
  c <- context
  (_,state) <- runStory c UserNotLoggedIn $ login (Username "student") "token"
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
    , courseEvalConfig = binaryEvalConfig
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
  let ca = E.Assignment "cname" "cexercise" Normal str UTC end UTC
      ga = E.Assignment "gname" "gexercise" Normal str UTC end UTC
      c1  = E.Course "FP" "FP-DESC" binaryEvalConfig
      c2  = E.Course "MA" "MA-DESC" binaryEvalConfig
      g1  = E.Group  "G1" "G1-DESC" binaryEvalConfig
      g2  = E.Group  "G2" "G2-DESC" $ percentageEvalConfig (PctConfig 0.4)
  runStory c adminUserState $ createUser adminUser
  runStory c adminUserState $ createUser student2
  (_,l) <- runStory c UserNotLoggedIn $ login (E.Username "admin") "token"
  ((a1,a2,as,ck2,gk2),_) <- runStory c l $ do
    ck1 <- createCourse c1
    ck2 <- createCourse c2
    gk1 <- createGroup ck1 g1
    gk2 <- createGroup ck2 g2
    a1 <- createGroupAssignment gk1 ga
    a2 <- createCourseAssignment ck2 ca
    subscribeToGroup gk1
    subscribeToGroup gk2
    as <- fmap (maybe [] id) userAssignments
    return (a1,a2,as,ck2,gk2)
  let as' = map fst3 as
  assertBool "Assignment does not found in the assignment list" ([a1,a2] == as' || [a2,a1] == as')
  (_,ul) <- runStory c UserNotLoggedIn $ login (E.Username "student2") "token"
  ((uc,ug),_) <- runStory c ul $ do
    subscribeToGroup  gk2
    uc <- U.isUserInCourse ck2
    ug <- attendedGroups
    return (uc,ug)
  assertBool "User is not registered in course" (uc == True)
  assertBool "User is not registered in group" (elem gk2 (map fst ug))

-- * Helpers

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
