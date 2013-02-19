module Test.Unit.Invariants (
    routeOfTests
  ) where

import Bead.Invariants

import Test.HUnit hiding (Test(..))
import Test.Framework (Test(..), testGroup)
import Test.Framework.Providers.HUnit

import qualified Bead.View.Snap.RouteOf as R (unitTests)

unitTestGroup :: String -> UnitTests -> Test
unitTestGroup name (UnitTests ts) = testGroup name
  $ map (\(desc, b) -> testCase desc (assertBool desc b)) ts

routeOfTests = unitTestGroup "Route Of" R.unitTests


