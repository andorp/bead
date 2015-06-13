module Main where

import Test.Tasty
import Test.Tasty.TestSet

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories
import qualified Test.Quick.Persistence

main = do
  Test.Tasty.defaultMain $ buildTestTree "" $ do
    Test.Unit.Invariants.tests
    Test.Unit.Persistence.TestNoSQLDir.noSqlDirTests
    Test.UserStories.TestStories.userStoryTests
    Test.Quick.Persistence.complexTests
