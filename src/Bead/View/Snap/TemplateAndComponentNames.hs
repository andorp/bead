{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.TemplateAndComponentNames where

-- This module contains information about templates and
-- fields in the type safe manner.

import Data.String
import Data.ByteString.Char8

import qualified Bead.Controller.Pages as P

-- * Type safe declarations

class SnapFieldName f where
  fieldName :: (IsString s) => f -> s

-- * Component names

data LoginComp
  = Username String
  | Password String

instance SnapFieldName LoginComp where
  fieldName (Username f) = fromString f
  fieldName (Password f) = fromString f

loginUsername = Username "login"
loginPassword = Password "password"

data RegistrationComp
  = RegFamilyName   String
  | RegEmailAddress String

instance SnapFieldName RegistrationComp where
  fieldName (RegFamilyName f)   = fromString f
  fieldName (RegEmailAddress f) = fromString f

registrationFamilyName   = RegFamilyName   "reg_family_name"
registrationEmailAddress = RegEmailAddress "reg_email_address"

data ExerciseForm
  = ExerciseForm     String
  | ExerciseKeyField String

instance SnapFieldName ExerciseForm where
  fieldName (ExerciseForm f) = fromString f
  fieldName (ExerciseKeyField e) = fromString e

exerciseForm = ExerciseForm "exercise"
exerciseKey  = ExerciseKeyField "exercise-key"

data CoursesForm
  = CoursesKey  String
  | CoursesForm String

instance SnapFieldName CoursesForm where
  fieldName (CoursesForm f) = fromString f
  fieldName (CoursesKey  f) = fromString f

coursesForm = CoursesForm "courses"
coursesKey  = CoursesKey "courses-key"

data CourseFormInfo
  = CourseKeyInfo  String
  | CourseFormInfo String

instance SnapFieldName CourseFormInfo where
  fieldName (CourseKeyInfo  f) = fromString f
  fieldName (CourseFormInfo f) = fromString f

courseKeyInfo  = CourseKeyInfo  "course-key"
courseFormInfo = CourseFormInfo "course"

-- * Template names

newtype LoginTemp = LoginTemp String
  deriving (Eq)

loginTemp = LoginTemp "login"

