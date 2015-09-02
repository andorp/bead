module Main where

import Test.Tasty
import Test.Tasty.TestSet

import qualified Test.Unit.Module
import qualified Test.Unit.Persistence
import qualified Test.Unit.UserStory
import qualified Test.Property.Persistence

main = do
  Test.Tasty.defaultMain $ buildTestTree "" $ do
    Test.Unit.Module.tests
    Test.Unit.Persistence.tests
    Test.Unit.UserStory.tests
    Test.Property.Persistence.tests

{-
main = do
  Test.Property.Persistence.createTestData 1
-}
