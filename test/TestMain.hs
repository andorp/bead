module Main where

import Test.Framework (defaultMain)

-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories

tests = [
    Test.Unit.Persistence.TestNoSQLDir.tests
  , Test.Unit.Invariants.routeOfTests
  , Test.Unit.Invariants.pageTests
  , Test.Unit.Invariants.rolePermissionTests
  , Test.UserStories.TestStories.tests
  ]

main = defaultMain tests
