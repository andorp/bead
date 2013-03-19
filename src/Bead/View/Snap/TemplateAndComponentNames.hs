{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Bead.View.Snap.TemplateAndComponentNames where

-- This module contains information about templates and
-- fields in the type safe manner.

-- Haskell imports
import Data.String
import Control.Monad (join)
import qualified Bead.Controller.Pages as P

-- Test imports
import Bead.Invariants (UnitTests(..))
import Data.Set (Set)
import qualified Data.Set as Set

-- * Type safe declarations

class SnapFieldName f where
  fieldName :: (IsString s) => f -> s

newtype SubmitButton = SubmitButton { sbFieldName :: String }

instance SnapFieldName SubmitButton where
  fieldName = fromString . sbFieldName

-- * Component names

data LoginComp
  = UsernameField { lcFieldName :: String }
  | PasswordField { lcFieldName :: String }

instance SnapFieldName LoginComp where
  fieldName = fromString . lcFieldName

loginUsername = UsernameField "login"
loginPassword = PasswordField "password"
loginSubmitBtn = SubmitButton  "login-submit"

data RegistrationComp
  = RegFamilyName   { rFieldName :: String }
  | RegEmailAddress { rFieldName :: String }

instance SnapFieldName RegistrationComp where
  fieldName = fromString . rFieldName

registrationFamilyName   = RegFamilyName   "reg_family_name"
registrationEmailAddress = RegEmailAddress "reg_email_address"

data ExerciseForm
  = ExerciseForm     { eFieldName :: String }
  | ExerciseKeyField { eFieldName :: String }

instance SnapFieldName ExerciseForm where
  fieldName = fromString . eFieldName

exerciseForm = ExerciseForm "exercise"
exerciseKey  = ExerciseKeyField "exercise-key"

data CoursesForm
  = CoursesKey  { csFieldName :: String }
  | CoursesForm { csFieldName :: String }

instance SnapFieldName CoursesForm where
  fieldName = fromString . csFieldName

coursesForm = CoursesForm "courses"
coursesKey  = CoursesKey "courses-key"

data CourseFormInfo
  = CourseKeyInfo   { cFieldName :: String }
  | CourseFormInfo  { cFieldName :: String }
  | CourseNameField { cFieldName :: String }
  | CourseCodeField { cFieldName :: String }
  | CourseDescField { cFieldName :: String }

instance SnapFieldName CourseFormInfo where
  fieldName = fromString . cFieldName

courseKeyInfo  = CourseKeyInfo  "course-key"
courseFormInfo = CourseFormInfo "course"
courseCodeField = CourseCodeField "course-code"
courseNameField = CourseNameField "course-name"
courseDescField = CourseDescField "course-desc"

newtype GroupKeyName
  = GroupKeyName { gkFieldName :: String }

instance SnapFieldName GroupKeyName where
  fieldName = fromString . gkFieldName

groupKeyName = GroupKeyName "group-key"

data GroupField
  = GroupCodeField { gFieldName :: String }
  | GroupDescField { gFieldName :: String }
  | GroupNameField { gFieldName :: String }

instance SnapFieldName GroupField where
  fieldName = fromString . gFieldName

groupCodeField = GroupCodeField "group-code"
groupNameField = GroupNameField "group-name"
groupDescField = GroupDescField "group-desc"

data UserField
  = UserField  { uFieldName :: String }
  | UserEmailField { uFieldName :: String }
  | UserRoleField  { uFieldName :: String }
  | UserFamilyNameField { uFieldName :: String }

instance SnapFieldName UserField where
  fieldName = fromString . uFieldName

usernameField  = UserField "username"
userEmailField = UserEmailField "useremail"
userRoleField  = UserRoleField "userrole"
userFamilyNameField = UserFamilyNameField "userfamilyname"

menuId :: P.Page -> String
menuId P.Login          = "link-login"
menuId P.Logout         = "link-logout"
menuId P.Home           = "link-home"
menuId P.Profile        = "link-profile"
menuId P.Users          = "link-users"
menuId P.UserDetails    = "link-userdetails"
menuId P.Course         = "link-course"
menuId P.Courses        = "link-courses"
menuId P.Group          = "link-group"
menuId P.Groups         = "link-groups"
menuId P.Exercise       = "link-exercise"
menuId P.ClosedExam     = "link-closedexam"
menuId P.Error          = "link-error"
menuId P.SubmitExam     = "link-submitexam"
menuId P.Evaulation     = "link-evaulation"
menuId P.Training       = "link-training"
menuId P.Admin          = "link-admin"
menuId P.CreateExercise = "link-create-exercise"
menuId P.CreateCourse   = "link-create-course"
menuId P.CreateGroup    = "link-create-group"

instance SnapFieldName P.Page where
  fieldName = fromString . menuId

-- * Template names

newtype LoginTemp = LoginTemp String
  deriving (Eq)

loginTemp = LoginTemp "login"

-- * Unit tests

data SFN = forall n . SnapFieldName n => SFN n

instance SnapFieldName SFN where
  fieldName (SFN n) = fieldName n

fieldList :: [String]
fieldList = map fieldName $ join [
  [ SFN loginUsername,  SFN loginPassword,   SFN registrationFamilyName, SFN registrationEmailAddress
  , SFN exerciseForm,   SFN exerciseKey,     SFN coursesForm,            SFN coursesKey
  , SFN courseFormInfo, SFN courseCodeField, SFN courseNameField,        SFN courseDescField
  , SFN groupKeyName,   SFN groupCodeField,  SFN groupNameField,         SFN groupDescField
  , SFN usernameField,  SFN courseKeyInfo,   SFN userEmailField,         SFN userFamilyNameField
  , SFN userRoleField,  SFN loginSubmitBtn
  ], (map SFN P.allPages)
  ]

unitTests = UnitTests [
    ( "Field names must be unique"
      , ((Set.size . Set.fromList $ fieldList) == (length fieldList)) )
  ]
