module Test.Tasty.Arbitrary (
    alpha
  , alphaNum
  , num
  , enumGen
  , module Test.QuickCheck.Arbitrary
  , module Test.QuickCheck.Gen
  ) where

-- Reimports the quickcheck's arbitrary module.

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

alpha = elements ['a' .. 'z']
num   = elements ['0' .. '9']

alphaNum = oneof [alpha, num]

enumGen :: (Enum e) => Gen e
enumGen = elements [(toEnum 0) .. ]
