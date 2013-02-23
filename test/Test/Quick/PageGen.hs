module Test.Quick.PageGen where

import Bead.Controller.Pages

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Quick.EnumGen



instance Arbitrary Page where
  arbitrary = enumElements

