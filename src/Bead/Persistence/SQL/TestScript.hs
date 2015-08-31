{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.TestScript where

import qualified Data.Text as Text

import           Database.Persist.Sql

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON

#ifdef TEST
import           Bead.Persistence.SQL.Course

import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, shrink, equals)
#endif

-- * Test Scripts

-- Saves the test script for the given course
saveTestScript :: Domain.CourseKey -> Domain.TestScript -> Persist Domain.TestScriptKey
saveTestScript courseKey testScript = do
  testScriptKey <- insert (fromDomainValue testScript)
  insert (TestScriptsOfCourse (toEntityKey courseKey) testScriptKey)
  return $! toDomainKey testScriptKey

-- Load the test script from the database
loadTestScript :: Domain.TestScriptKey -> Persist Domain.TestScript
loadTestScript testScriptKey = do
  mTestScript <- get (toEntityKey testScriptKey)
  return $!
    maybe (persistError "loadTestScript" $ "Test Script is not found:" ++ show testScriptKey)
          toDomainValue
          mTestScript

-- Returns the course of the test script
courseOfTestScript :: Domain.TestScriptKey -> Persist Domain.CourseKey
courseOfTestScript key = do
  courses <- selectList [TestScriptsOfCourseTestScript ==. (toEntityKey key)] []
  case courses of
    []  -> persistError "courseOfTestScript" $ "No courses were found. " ++ show key
    [c] -> return $! toDomainKey $ testScriptsOfCourseCourse $ entityVal c
    _   -> persistError "courseOfTestScript" $ "Multiple courses were found. " ++ show key

-- Updates the test script for the given test script key
modifyTestScript :: Domain.TestScriptKey -> Domain.TestScript -> Persist ()
modifyTestScript key script = do
  update (toEntityKey key) $ Domain.withTestScript script encodeTestScriptType
    $ \name desc notes script type_ ->
        [ TestScriptName           =. Text.pack name
        , TestScriptDescription    =. Text.pack desc
        , TestScriptNotes          =. Text.pack notes
        , TestScriptScript         =. Text.pack script
        , TestScriptTestScriptType =. type_
        ]

#ifdef TEST

testScriptTests = do
  shrink "Test Script end-to-end story."
    (do ioTest "Test Script end-to-end test" $ runSql $ do
          initDB
          c <- saveCourse course
          t <- saveTestScript c script
          script' <- loadTestScript t
          equals script script' "The scripts were differents."
          c' <- courseOfTestScript t
          equals c c' "The course keys were differents."
          modifyTestScript t script2
          script2' <- loadTestScript t
          equals script2 script2' "The script was not modified."
    )

    (do ioTest "Save and load test script" $ runSql $ do
          initDB
          c <- saveCourse course
          t <- saveTestScript c script
          script' <- loadTestScript t
          equals script script' "The scripts were differents."
        ioTest "Course of the test script" $ runSql $ do
          initDB
          c <- saveCourse course
          t <- saveTestScript c script
          c' <- courseOfTestScript t
          equals c c' "The course keys were differents."
        ioTest "Modify the test script" $ runSql $ do
          initDB
          c <- saveCourse course
          t <- saveTestScript c script
          modifyTestScript t script2
          script2' <- loadTestScript t
          equals script2 script2' "The script was not modified."
    )

#endif
