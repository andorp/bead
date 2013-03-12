module Test.Quick.RolePermissionGen where

import Bead.Domain.Entities (Role(..), Permission(..), PermissionObject(..))

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Test.Quick.EnumGen


instance Arbitrary Role where
  arbitrary = enumGenerator

instance Arbitrary Permission where
  arbitrary = enumGenerator

instance Arbitrary PermissionObject where
  arbitrary = enumGenerator
