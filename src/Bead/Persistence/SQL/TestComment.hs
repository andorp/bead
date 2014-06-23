module Bead.Persistence.SQL.TestComment where

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Entities (Persist)
import qualified Bead.Persistence.SQL.FileSystem as FS

insertTestComment :: Domain.SubmissionKey -> String -> Persist ()
insertTestComment = FS.insertTestComment

testComments :: Persist [(Domain.SubmissionKey, Domain.Comment)]
testComments = FS.testComments

deleteTestComment :: Domain.SubmissionKey -> Persist ()
deleteTestComment = FS.deleteTestComment

