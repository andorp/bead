module Main where

import Control.Monad (join)

-- Test cases

import Test.Tasty
import Test.Tasty.TestSet

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories
import qualified Test.Quick.Persistence

tests args =
  join [
      (ifPresent "unit" Test.Unit.Invariants.tests)
    ]
  where
    ifPresent a xs =
      if (elem a args)
        then xs
        else []

main = do
  Test.Tasty.defaultMain $ buildTestTree "" $ do
    Test.Unit.Invariants.tastyTests
    Test.Unit.Persistence.TestNoSQLDir.noSqlDirTests
    Test.UserStories.TestStories.userStoryTests
    Test.Quick.Persistence.complexTests
