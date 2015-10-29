{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.TestCase where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sql

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS
import           Bead.Persistence.SQL.JSON

#ifdef TEST
import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.TestScript
import           Bead.Persistence.SQL.MySQLTestRunner
import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, shrink, equals)
#endif

-- * Test Cases

-- Saves the test case for the given assignment and given test script
saveTestCase :: Domain.TestScriptKey -> Domain.AssignmentKey -> Domain.TestCase -> Persist Domain.TestCaseKey
saveTestCase testScriptKey assignmentKey testCase = do
  let testScriptKey' = toEntityKey testScriptKey
      assignmentKey' = toEntityKey assignmentKey
  testCaseKey <- insert (fromDomainValue testCase)
  insertUnique (TestScriptOfTestCase testCaseKey testScriptKey')
  insertUnique (TestCaseOfAssignment assignmentKey' testCaseKey)
  return $! toDomainKey testCaseKey

-- Loads the test case from the database
loadTestCase :: Domain.TestCaseKey -> Persist Domain.TestCase
loadTestCase key = do
  mTC <- get (fromDomainKey key)
  return $!
    maybe (persistError "loadTestCase" $ "test case was not defined:" ++ show key)
          toDomainValue
          mTC

-- Returns the assignment of the given test case
assignmentOfTestCase :: Domain.TestCaseKey -> Persist Domain.AssignmentKey
assignmentOfTestCase key = do
  assignments <- selectList [TestCaseOfAssignmentTestCase ==. toEntityKey key] []
  return $!
    maybe (persistError "assignmentOfTestCase" $ "no TestCase was found. " ++ show key)
          (toDomainKey . testCaseOfAssignmentAssignment . entityVal)
          (listToMaybe assignments)

-- Returns the test script of the given test case
testScriptOfTestCase :: Domain.TestCaseKey -> Persist Domain.TestScriptKey
testScriptOfTestCase key = do
  testScripts <- selectList [TestScriptOfTestCaseTestCase ==. toEntityKey key] []
  return $!
    maybe (persistError "testScriptOfTestCase" $ "no TestCase was found. " ++ show key)
          (toDomainKey . testScriptOfTestCaseTestScript . entityVal)
          (listToMaybe testScripts)

-- Updates the test case for the given test case key
modifyTestCase :: Domain.TestCaseKey -> Domain.TestCase -> Persist ()
modifyTestCase key testCase = do
  update (toEntityKey key) $ Domain.withTestCase testCase id -- encodeTestCaseType
    $ \name desc value info ->
        let (simple, zipped) = Domain.withTestCaseValue value
              (\s -> (Just $ Text.pack s, Nothing))
              (\z -> (Nothing, Just z))
        in [ TestCaseName         =. Text.pack name
           , TestCaseDescription  =. Text.pack desc
           , TestCaseSimpleValue  =. simple
           , TestCaseZippedValue  =. zipped
           , TestCaseInfo         =. Text.pack info
           ]

-- Deletes the link from the test case connected to an assignment
removeTestCaseAssignment :: Domain.TestCaseKey -> Domain.AssignmentKey -> Persist ()
removeTestCaseAssignment tk ak =
  deleteBy (UniqueTestCaseToAssignment (toEntityKey ak) (toEntityKey tk))

-- Copy the Zipped test case file into the test case
copyTestCaseFile :: Domain.TestCaseKey -> Domain.Username -> Domain.UsersFile -> Persist ()
copyTestCaseFile tcKey username userfile =
  withUser username (persistError "copyTestCaseFile" $ "User is not found:" ++ show username)
  (\_userEnt -> void $ do
      content <- FS.getFile username userfile >>= FS.fileLoadBS
      update (toEntityKey tcKey)
        [ TestCaseZippedValue =. (Just content)
        , TestCaseSimpleValue =. Nothing
        ])

modifyTestScriptOfTestCase :: Domain.TestCaseKey -> Domain.TestScriptKey -> Persist ()
modifyTestScriptOfTestCase caseKey scriptKey = void $ do
  let caseKey'   = toEntityKey caseKey
      scriptKey' = toEntityKey scriptKey
  deleteWhere [TestScriptOfTestCaseTestCase ==. caseKey']
  insertUnique (TestScriptOfTestCase caseKey' scriptKey')

#ifdef TEST

testCaseTests = do
  ioTest "Test Case end-to-end case" $ runSql $ do
    c  <- saveCourse course
    ts <- saveTestScript c script
    a  <- saveCourseAssignment c asg
    tc <- saveTestCase ts a case1
    case1' <- loadTestCase tc
    equals case1 case1' "Saved and load test cases were different"
    a' <- assignmentOfTestCase tc
    equals a a' "Assignment of the test case was wrong"
    ts' <- testScriptOfTestCase tc
    equals ts ts' "Test script of the test case was wrong"
    modifyTestCase tc case2
    case2' <- loadTestCase tc
    equals case2 case2' "Test case was not modified correctly"
    ts2 <- saveTestScript c script2
    modifyTestScriptOfTestCase tc ts2
    ts3 <- testScriptOfTestCase tc
    equals ts2 ts3 "Test script of the test case did not get modified"
    removeTestCaseAssignment tc a
    tc2 <- testCaseOfAssignment a
    equals Nothing tc2 "Test case of the assignment was wrong"

#endif
