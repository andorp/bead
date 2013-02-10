module Main where

import Test.Framework (defaultMain)

-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir

tests = [
    Test.Unit.Persistence.TestNoSQLDir.tests
  ]

main = defaultMain tests
