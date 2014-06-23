module Bead.Persistence.SQL.TestJob where

import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS
import           Bead.Persistence.SQL.Submission
import           Bead.Persistence.SQL.TestCase
import           Bead.Persistence.SQL.TestScript

-- * Test Jobs

-- Saves the test job for the test daemon
saveTestJob :: Domain.SubmissionKey -> Persist ()
saveTestJob sk = do
  mTestCaseKey <- assignmentOfSubmission sk >>= testCaseOfAssignment
  case mTestCaseKey of
    Nothing -> return ()
    Just tk -> do
      submission <- loadSubmission sk
      testCase <- loadTestCase tk
      testScript <- testScriptOfTestCase tk >>= loadTestScript
      FS.saveTestJob sk submission testScript testCase

