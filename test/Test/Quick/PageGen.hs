module Test.Quick.PageGen where

import Control.Monad (liftM)

import Bead.Controller.Pages
import Bead.Domain.Entities (Role(..))

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Quick.EnumGen
import Test.Quick.RolePermissionGen


instance Arbitrary Page where
  arbitrary = enumGenerator

data MenuNavigation = MenuNavigation Role [Page]
  deriving (Show)

instance Arbitrary MenuNavigation where
  arbitrary = do
    r <- arbitrary
    sized $ \n -> liftM (MenuNavigation r . take n) $ menuNavigation r Login

genMenuNavigation :: Gen MenuNavigation
genMenuNavigation = arbitrary

menuStep :: Role -> Page -> Gen Page
menuStep r = elements . filter (flip elem contentPages) . menuPages r

menuNavigation :: Role -> Page -> Gen [Page]
menuNavigation r p = do
  p' <- menuStep r p
  liftM (p:) $ menuNavigation r p'
