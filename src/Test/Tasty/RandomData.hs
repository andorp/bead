module Test.Tasty.RandomData (
    RandomData(..)
  ) where

-- Reimports the quickcheck's arbitrary module.

import Test.QuickCheck.Gen

class RandomData a where
  positive :: Gen a
  negative :: Gen a

