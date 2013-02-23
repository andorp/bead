module Test.Unit.Invariants (
    routeOfTests
  , pageTests
  ) where

-- * Bead imports

import Bead.Invariants

-- * Test Framework imports

import Test.HUnit hiding (Test(..))
import Test.QuickCheck.Arbitrary
import Test.Framework (Test(..), testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

-- * Generators

import Test.Quick.PageGen

-- * Unit tests and invariants import from Bead modules

import qualified Bead.View.Snap.RouteOf as R (unitTests)
import qualified Bead.Controller.Pages as P (invariants)



unitTestGroup :: String -> UnitTests -> Test
unitTestGroup name (UnitTests ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (assertBool desc testcase)) ts

invariantsGroup :: (Show i, Arbitrary i) => String -> Invariants i -> Test
invariantsGroup name (Invariants is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc prop) is

routeOfTests = unitTestGroup "Route Of" R.unitTests

pageTests = invariantsGroup "Page invariants" P.invariants
