{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.TestCase where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Entity.Assignment as Domain
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

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
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
  update (toEntityKey key) $ Domain.withTestCase testCase encodeTestCaseType
    $ \name desc value type_ info ->
        [ TestCaseName         =. Text.pack name
        , TestCaseDescription  =. Text.pack desc
        , TestCaseValue        =. value
        , TestCaseTestCaseType =. type_
        , TestCaseInfo         =. Text.pack info
        ]

-- Deletes the link from the test case connected to an assignment
removeTestCaseAssignment :: Domain.TestCaseKey -> Domain.AssignmentKey -> Persist ()
removeTestCaseAssignment tk ak =
  deleteBy (UniqueTestCaseToAssignment (toEntityKey ak) (toEntityKey tk))

copyTestCaseFile :: Domain.TestCaseKey -> Domain.Username -> Domain.UsersFile -> Persist ()
copyTestCaseFile tcKey username userfile =
  withUser username (persistError "copyTestCaseFile" $ "User is not found:" ++ show username)
  (\_userEnt -> void $ do
      content <- FS.getFile username userfile >>= FS.fileLoad
      update (toEntityKey tcKey) [TestCaseValue =. BS.pack content])

modifyTestScriptOfTestCase :: Domain.TestCaseKey -> Domain.TestScriptKey -> Persist ()
modifyTestScriptOfTestCase caseKey scriptKey = void $ do
  let caseKey'   = toEntityKey caseKey
      scriptKey' = toEntityKey scriptKey
  deleteWhere [TestScriptOfTestCaseTestCase ==. caseKey']
  insertUnique (TestScriptOfTestCase caseKey' scriptKey')

#ifdef TEST

testCaseTests = do
  let course  = Domain.Course "name" "desc" (Domain.BinEval ()) Domain.TestScriptSimple
      script  = Domain.TestScript "name" "desc" "notes" "script" Domain.TestScriptSimple
      script2 = Domain.TestScript "name2" "desc2" "notes2" "script2" Domain.TestScriptZipped
      case1   = Domain.TestCase "name" "desc" "blah" Domain.TestCaseSimple "info"
      case2   = Domain.TestCase "name2" "desc2" "blah2" Domain.TestCaseZipped "info"
      time    = read "2014-06-09 12:55:27.959203 UTC"
      asg     = Domain.Assignment "name" "desc" Domain.emptyAspects time time

  shrink "Test Case end-to-end story."
    (do ioTest "Test Case end-to-end case" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          ts <- dbStep $ saveTestScript c script
          a  <- dbStep $ saveCourseAssignment c asg
          tc <- dbStep $ saveTestCase ts a case1
          case1' <- dbStep $ loadTestCase tc
          assertEquals case1 case1' "Saved and load test cases were different"
          a' <- dbStep $ assignmentOfTestCase tc
          assertEquals a a' "Assignment of the test case was wrong"
          ts' <- dbStep $ testScriptOfTestCase tc
          assertEquals ts ts' "Test script of the test case was wrong"
          dbStep $ modifyTestCase tc case2
          case2' <- dbStep $ loadTestCase tc
          assertEquals case2 case2' "Test case was not modified correctly"
          ts2 <- dbStep $ saveTestScript c script2
          dbStep $ modifyTestScriptOfTestCase tc ts2
          ts3 <- dbStep $ testScriptOfTestCase tc
          assertEquals ts2 ts3 "Test script of the test case did not get modified"
          dbStep $ removeTestCaseAssignment tc a
          tc2 <- dbStep $ testCaseOfAssignment a
          assertEquals Nothing tc2 "Test case of the assignment was wrong"

          return ())
    (do return ()) -- TODO
  return ()

#endif
