{-# LANGUAGE CPP #-}
module Test.Unit.UserStory (
    tests
  ) where

import           Control.Monad.Trans (lift)
import qualified Data.Map as Map
import           Prelude hiding (log)

import           Data.Time.Clock

import           Bead.Controller.ServiceContext
import           Bead.Controller.UserStories as U
import           Bead.Domain.Entities as E hiding (name, uid)
import           Bead.Domain.Relationships (TCCreation(..))
import           Bead.Domain.Shared.Evaluation

import           Test.HUnit hiding (Test(..), test)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.TestSet

import           Test.Model.UserStory


-- * Tests

tests = group "User Stories" $ do
  test initPersist
  test register
  test loginAndLogout
  test courseTest
  test courseAndGroupAssignmentTest
#ifndef SSO
  test saveAndLoadUserReg
#endif
  test cleanUpPersist

initPersist = testCase "Initalizing persistence layer" $
  Test.Model.UserStory.initPersistent

cleanUpPersist = testCase "Cleaning up persistence" $
  Test.Model.UserStory.cleanUpPersistent

#ifndef SSO
saveAndLoadUserReg = testCase "Save and load user reg data" $ do
  userStoryTestContext $ do
    let now = utcTimeConstant
    let u = UserRegistration "username" "e@e.com" "Family Name" "token" now
    key <- registrationStory $ U.createUserReg u
    u'  <- registrationStory $ U.loadUserReg key
    lift $ assertBool "Saved and load user registration differs" (u' == u)
#endif

register = testCase "User registration" $ do
  userStoryTestContext $ do
    adminStory $ createUser student

loginAndLogout = testCase "Login And Logout" $ do
  c <- context
  (_,state) <- runStory c UserNotLoggedIn $ login (Username "student") "token"
  assertUserState state student
  (_,state) <- runStory c state $ logout
  case state of
    UserState {} -> error "User is remained logged in"
    UserNotLoggedIn -> return ()
    Registration -> error "Registration state is returned"
    TestAgent -> error "TestAgent state is returned"

courseTest = testCase "Create Course" $ do
  c <- context
  let r = E.Course {
      courseName = "Functional programming"
    , courseDesc = "Everything about FP"
    , courseTestScriptType = TestScriptSimple
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
  str <- getCurrentTime
  end <- getCurrentTime
  let ca = Assignment "cname" "cexercise" emptyAspects str end binaryConfig
      ga = Assignment "gname" "gexercise" emptyAspects str end (percentageConfig 0.3)
      c1  = E.Course "FP" "FP-DESC" TestScriptSimple
      c2  = E.Course "MA" "MA-DESC" TestScriptZipped
      g1  = E.Group  "G1" "G1-DESC"
      g2  = E.Group  "G2" "G2-DESC"
      adminUsername = E.Username "admin"
      groupAdminUsr = E.Username "groupadmin"
      student2Username = E.Username "student2"
  userStoryTestContext $ do
    adminStory $ do
      createUser adminUser
      createUser groupAdminUser
      createUser student2

    (ck2,gk1,gk2,a2) <- userStory adminUsername $ do
      ck1 <- createCourse c1
      ck2 <- createCourse c2
      U.createCourseAdmin adminUsername ck1
      U.createCourseAdmin adminUsername ck2
      gk1 <- createGroup ck1 g1
      gk2 <- createGroup ck2 g2
      U.createGroupAdmin groupAdminUsr gk1
      U.createGroupAdmin groupAdminUsr gk2
      a2 <- createCourseAssignment ck2 ca NoCreation
      return (ck2,gk1,gk2,a2)

    (a1,as) <- userStory groupAdminUsr $ do
      a1 <- createGroupAssignment gk1 ga NoCreation
      subscribeToGroup gk1
      subscribeToGroup gk2
      as <- fmap toList userAssignments
      return (a1,as)
    let as' = map trd as
    lift $ assertBool "Assignment does not found in the assignment list" ([a1,a2] == as' || [a2,a1] == as')

    (uc,ug) <- userStory student2Username $ do
      subscribeToGroup  gk2
      uc <- U.isUserInCourse ck2
      ug <- attendedGroups
      return (uc,ug)

    lift $ assertBool "User is not registered in course" (uc == True)
    lift $ assertBool "User is not registered in group" (elem gk2 (map trd ug))
  where
    toList = concat . map snd . Map.toList

-- * Helpers

trd :: (a,b,c) -> a
trd (a,_,_) = a

utcTimeConstant :: UTCTime
utcTimeConstant = read "2015-08-27 17:08:58 UTC"
