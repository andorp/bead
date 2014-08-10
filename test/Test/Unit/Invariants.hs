{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.Invariants (
    tests
  ) where

-- * Bead imports

import Control.Monad (join)

-- * Test Framework imports

import           Test.HUnit hiding (Test(..), Assertion (..))
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic
import           Test.Framework (Test(..), testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Themis.Test (runTest)
import           Test.Themis.Provider.TestFramework (buildTestSet)

import           Bead.Configuration (initTaskAssertions)
import qualified Bead.Controller.Pages as P (invariants)
import qualified Bead.Daemon.Logout as LD (unitTests)
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as A
import qualified Bead.Domain.Entity.Feedback as F
import qualified Bead.Domain.RolePermission as RP (invariants)
import qualified Bead.Persistence.NoSQLDirFile as L (unitTests)
import qualified Bead.Persistence.Persist as P (persistTests)
import qualified Bead.Persistence.Relations as PR (persistRelationsTests)
import           Bead.Invariants
import qualified Bead.View.Snap.Content.All as VA (invariants)
import           Bead.View.Snap.Content.Home
import qualified Bead.View.Snap.DataBridge as DB (dataBridgeTests)
import qualified Bead.View.Snap.EmailTemplate as E (unitTests)
import qualified Bead.View.Snap.Pagelets as VP (invariants)
import qualified Bead.View.Snap.RouteOf as R (routeOfInvariants)
import qualified Bead.View.Snap.Routing as R (routingInvariants)
import qualified Bead.View.Snap.Session as VS (unitTests)
import qualified Bead.View.Snap.TemplateAndComponentNames as TC (unitTests)
import qualified Bead.View.Snap.Validators as V (assertEmailAddress)
import           Test.Quick.PageGen()
import           Test.Quick.RolePermissionGen()


unitTestGroup :: String -> UnitTests -> Test
unitTestGroup name (UnitTests ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (assertBool desc testcase)) ts

ioUnitTestGroup :: String -> UnitTestsM IO -> Test
ioUnitTestGroup name (UnitTestsM ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (testcase >>= assertBool desc)) ts

invariantsGroup :: (Show i, Arbitrary i) => String -> Invariants i -> Test
invariantsGroup name (Invariants is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc prop) is

invariantsIO2Group
  :: (Show a, Show b, Arbitrary a, Arbitrary b)
  => String -> InvariantsM2 IO a b -> Test
invariantsIO2Group name (InvariantsM2 is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc (\a b -> monadicIO (run (prop a b)))) is

assertionTestGroup :: (Show a, Eq a) => String -> [Assertion a] -> Test
assertionTestGroup name as = testGroup name
  $ map (assertionMap createTestCase createOracleTestCase) as
  where
    createTestCase n f e =
      testCase n $ assertBool (join ["Found: ", show f, " expected: ", show e]) (f == e)

    createOracleTestCase n f o =
      testCase n $
        assertBool (join ["Found: ", show f, " The oracle does not accept the result."]) (o f)

-- * Unit tests

tests = [
    invariantsGroup "Route Of" R.routeOfInvariants
  , invariantsGroup "Rounting" R.routingInvariants
  , invariantsGroup "Page invariants" P.invariants
  , unitTestGroup   "Logout daemon" LD.unitTests
  , invariantsGroup "Role permission invariants" RP.invariants
  , invariantsGroup "Content handler definitions" VA.invariants
  , unitTestGroup   "NoSQL untilities" L.unitTests
  , invariantsGroup "Pages need to have link text" VP.invariants
  , unitTestGroup   "Page Session Keys" VS.unitTests
  , invariantsGroup "Role invariants" E.roleInvariants
  , unitTestGroup   "Assignment active period" A.assignmentTests
  , unitTestGroup   "Hungarian letter comparism" E.compareHunTests
  , unitTestGroup   "Template and components" TC.unitTests
  , ioUnitTestGroup "Email template tests" E.unitTests
  , assertionTestGroup "Email address" V.assertEmailAddress
  , assertionTestGroup "Home page binary results" sumBinaryResultTests
  , assertionTestGroup "Home page percentage results" sumPercentageResultTests
--  , assertionTestGroup "Home page calc results" calculateSubmissionResultTests: TODO define results
  , assertionTestGroup "Command line and configuration" initTaskAssertions
  ] ++ themisTests

themisTests = runTest buildTestSet $ do
  A.asgTests
  P.persistTests
  PR.persistRelationsTests
  DB.dataBridgeTests
  F.feedbackTests
