{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.TemplateAndComponentNames where

-- This module contains information about templates and
-- fields in the type safe manner.

import Data.String
import Data.ByteString.Char8

import qualified Bead.Controller.Pages as P

-- * Type safe declarations

class SnapFieldName f where
  fieldName :: f -> ByteString

-- * Component names

data LoginComp
  = Username ByteString
  | Password ByteString

instance SnapFieldName LoginComp where
  fieldName (Username f) = f
  fieldName (Password f) = f

loginUsername = Username "login"
loginPassword = Password "password"

data RegistrationComp
  = RegFamilyName ByteString
  | RegEmailAddress ByteString

instance SnapFieldName RegistrationComp where
  fieldName (RegFamilyName f)   = f
  fieldName (RegEmailAddress f) = f

registrationFamilyName   = RegFamilyName   "reg_family_name"
registrationEmailAddress = RegEmailAddress "reg_email_address"

newtype ExerciseForm
  = ExerciseForm ByteString

instance SnapFieldName ExerciseForm where
  fieldName (ExerciseForm f) = f

exerciseForm = ExerciseForm "exercise"

-- * Template names

newtype LoginTemp = LoginTemp ByteString
  deriving (Eq)

loginTemp = LoginTemp "login"

