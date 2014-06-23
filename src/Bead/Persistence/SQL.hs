{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL (
    module Bead.Persistence.SQL.Assignment
  , module Bead.Persistence.SQL.Comment
  , module Bead.Persistence.SQL.Course
  , module Bead.Persistence.SQL.Entities
  , module Bead.Persistence.SQL.Evaluation
  , module Bead.Persistence.SQL.Group
  , module Bead.Persistence.SQL.Init
  , module Bead.Persistence.SQL.Registration
  , module Bead.Persistence.SQL.Submission
  , module Bead.Persistence.SQL.TestCase
  , module Bead.Persistence.SQL.TestComment
  , module Bead.Persistence.SQL.TestJob
  , module Bead.Persistence.SQL.TestScript
  , module Bead.Persistence.SQL.User
#ifdef TEST
  , tests
#endif
  ) where

import Bead.Persistence.SQL.Assignment
import Bead.Persistence.SQL.Comment
import Bead.Persistence.SQL.Course
import Bead.Persistence.SQL.Entities (Persist)
import Bead.Persistence.SQL.Evaluation
import Bead.Persistence.SQL.Group
import Bead.Persistence.SQL.Init
import Bead.Persistence.SQL.Registration
import Bead.Persistence.SQL.Submission
import Bead.Persistence.SQL.TestCase
import Bead.Persistence.SQL.TestComment
import Bead.Persistence.SQL.TestJob
import Bead.Persistence.SQL.TestScript
import Bead.Persistence.SQL.User

import Test.Themis.Test (Test)

#ifdef TEST
tests :: Test ()
tests = do
  courseAdminTests
  groupTests
  testScriptTests
  assignmentTests
  testCaseTests
  submissionTests
  evaluationTests
  commentTests
  userRegistrationTests

#endif
