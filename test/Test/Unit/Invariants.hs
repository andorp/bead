module Test.Unit.Invariants (
    tests
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
import Test.Quick.RolePermissionGen

-- * Unit tests and invariants import from Bead modules

import qualified Bead.View.Snap.RouteOf as R (invariants)
import qualified Bead.Controller.Pages as P (invariants, unitTests)
import qualified Bead.Domain.RolePermission as RP (invariants)
import qualified Bead.View.Snap.Content.All as VA (invariants)
import qualified Bead.View.Snap.Pagelets as VP (invariants)
import qualified Bead.View.Snap.Session as VS (invariants)


unitTestGroup :: String -> UnitTests -> Test
unitTestGroup name (UnitTests ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (assertBool desc testcase)) ts

invariantsGroup :: (Show i, Arbitrary i) => String -> Invariants i -> Test
invariantsGroup name (Invariants is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc prop) is
  
-- * Unit tests

tests = [
    invariantsGroup "Route Of" R.invariants
  , invariantsGroup "Page invariants" P.invariants
  , invariantsGroup "Role permission invariants" RP.invariants
  , invariantsGroup "Content handler definitions" VA.invariants
  , invariantsGroup "Pages need to have link text" VP.invariants
  , invariantsGroup "Page Session Cookie values" VS.invariants
  , unitTestGroup   "Page unit tests" P.unitTests
  ]


