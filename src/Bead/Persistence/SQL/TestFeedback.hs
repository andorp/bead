module Bead.Persistence.SQL.TestFeedback where

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Entities (Persist)
import qualified Bead.Persistence.SQL.FileSystem as FS

insertTestFeedback :: Domain.SubmissionKey -> Domain.FeedbackInfo -> Persist ()
insertTestFeedback = FS.insertTestFeedback

testFeedbacks :: Persist [(Domain.SubmissionKey, Domain.Feedback)]
testFeedbacks = FS.testFeedbacks

deleteTestFeedbacks :: Domain.SubmissionKey -> Persist ()
deleteTestFeedbacks = FS.deleteTestFeedbacks

testIncomingDataDir = FS.testIncomingDataDir
