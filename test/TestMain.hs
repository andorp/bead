module Main where

import Test.Framework (defaultMain)

-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants

tests = [
    Test.Unit.Persistence.TestNoSQLDir.tests
  , Test.Unit.Invariants.routeOfTests
  , Test.Unit.Invariants.pageTests
  ]

main = defaultMain tests
