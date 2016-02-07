module Test.Regression.UserStory where

import           Control.Monad.Trans (lift)
import           Data.Time.Clock
import qualified Data.Map as Map
import           Bead.Controller.UserStories as U
import           Bead.Domain.Entities as E hiding (name, uid)
import           Bead.Domain.Relationships (TCCreation(..), SubmissionInfo(..))
import           Bead.Domain.Shared.Evaluation

import           Test.HUnit hiding (Test(..), test)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.TestSet

import           Test.Model.UserStory

tests = group "User Story regressions" $ do
  test $ testCase "Init persistent" $ Test.Model.UserStory.initPersistent
  test $ submissionTestInfoChanges
  test $ testCase "Clean up persistent" $ Test.Model.UserStory.cleanUpPersistent

submissionTestInfoChanges = testCase "Submission test information changes correctly" $ do
  now <- getCurrentTime
  let end = addUTCTime 3600 now
  let c1  = E.Course "FP" "FP-DESC" TestScriptSimple
  let g1  = E.Group  "G1" "G1-DESC"
  let ga = Assignment "gname" "gexercise" emptyAspects now end (percentageConfig 0.3)
  let adminUsername = E.Username "admin"
  let courseAdminUsername = E.Username "courseadmin"
  let groupAdminUsername = E.Username "groupadmin"
  let studentUsername = E.Username "student"
  userStoryTestContext $ do
    adminStory $ do
      createUser adminUser
      createUser courseAdminUser
      createUser groupAdminUser
      createUser student
      let r = E.Course {
          courseName = "Functional programming"
        , courseDesc = "Everything about FP"
        , courseTestScriptType = TestScriptSimple
        }
      createCourse r
  
    ck1 <- userStory adminUsername $ do
      ck1 <- createCourse c1
      U.createCourseAdmin courseAdminUsername ck1
      return ck1

    gk1 <- userStory courseAdminUsername $ do
      gk1 <- createGroup ck1 g1
      U.createGroupAdmin groupAdminUsername gk1
      return gk1
    
    ak1 <- userStory groupAdminUsername $ createGroupAssignment gk1 ga NoCreation
    
    sk1 <- userStory studentUsername $ do
      subscribeToGroup gk1
      sk1 <- submitSolution ak1 (Submission (SimpleSubmission "Blah") now)
      return sk1
    
    testAgentStory $ do
      U.insertTestFeedback sk1 (TestResult False)
      testAgentFeedbacks
      U.insertTestFeedback sk1 (TestResult True)
      testAgentFeedbacks    
    
    si <- userStory studentUsername $ do
      ua <- userAssignments
      return . trd . head . value . head . Map.toList $ Map.map snd ua

    -- TODO: Write assert typeclass
    lift $ assertBool "Submission test information is not changed" (si == Submission_Tested True)
  where
    value = snd
    trd (_x,_y,z) = z
