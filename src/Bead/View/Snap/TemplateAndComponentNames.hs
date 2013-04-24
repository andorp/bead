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
menuId P.Error          = "link-error"
menuId P.Administration = "link-admin"
menuId P.CourseAdmin    = "link-course-admin"
menuId P.EvaulationTable = "link-evaulation-table"
menuId P.Evaulation      = "link-evaulation"
menuId P.Submission      = "link-submission"
menuId P.SubmissionList  = "link-submission-list"
menuId P.UserSubmissions = "link-user-submissions"
menuId P.ModifyEvaulation = "link-modify-evaulation"
menuId P.SubmissionDetails = "link-submission-details"
menuId P.GroupRegistration = "link-group-registration"
menuId P.CreateCourse = "link-create-course"
menuId P.UserDetails = "link-user-details"
menuId P.AssignCourseAdmin = "link-assign-course-admin"
menuId P.CreateGroup = "link-create-group"
menuId P.AssignProfessor = "link-assign-professor"
menuId P.NewGroupAssignment  = "link-new-group-assignment"
menuId P.NewCourseAssignment = "link-new-course-assignment"
menuId P.ModifyAssignment = "link-modify-assignment"

instance SnapFieldName P.Page where
  fieldName = fromString . menuId

data AssignmentField
  = AssignmentDescField { aFieldName :: String }
  | AssignmentNameField { aFieldName :: String }
  | AssignmentTCsField { aFieldName :: String }
  | AssignmentTypeField { aFieldName :: String }
  | AssignmentStartField { aFieldName :: String }
  | AssignmentEndField { aFieldName :: String }
  | AssignmentKeyField { aFieldName :: String }
  | AssignmentEvField { aFieldName :: String }

assignmentNameField  = AssignmentNameField  "asg-name"
assignmentDescField  = AssignmentDescField  "asg-desc"
assignmentTCsField   = AssignmentTCsField   "asg-tcs"
assignmentTypeField  = AssignmentTypeField  "asg-type"
assignmentStartField = AssignmentStartField "asg-start"
assignmentEndField   = AssignmentEndField   "asg-end"
assignmentKeyField   = AssignmentKeyField   "asg-key"
assignmentEvField    = AssignmentEvField    "asg-ev"

instance SnapFieldName AssignmentField where
  fieldName = fromString . aFieldName

data AssignCourseAdminField
  = SelectedCourse { acFieldName :: String }
  | SelectedCourseAdmin { acFieldName :: String }

selectedCourse = SelectedCourse "selected-course"
selectedCourseAdmin = SelectedCourseAdmin "selected-course-admin"

instance SnapFieldName AssignCourseAdminField where
  fieldName = fromString . acFieldName

data AssignCourseProfessorField
  = SelectedProfessor { cpFieldName :: String }
  | SelectedGroup { cpFieldName :: String }

selectedGroup = SelectedGroup "selected-group"
selectedProfessor = SelectedProfessor "selected-professor"

instance SnapFieldName AssignCourseProfessorField where
  fieldName = fromString . cpFieldName

data GroupRegistrationField
  = GroupRegistrationField { grFieldName :: String }

groupRegistrationField = GroupRegistrationField "group-registration"

instance SnapFieldName GroupRegistrationField where
  fieldName = fromString . grFieldName

data SubmissionField
  = SubmissionTextField { sfFieldName :: String }
  | SubmissionKeyField { sfFieldName :: String }

submissionTextField = SubmissionTextField "submission-text"
submissionKeyField  = SubmissionKeyField  "submission-key"

instance SnapFieldName SubmissionField where
  fieldName = fromString . sfFieldName

data EvaulationField
  = EvaulationValueField { evFieldName :: String }
  | EvaulationStateField { evFieldName :: String }
  | EvaulationKeyField   { evFieldName :: String }

evaulationValueField = EvaulationValueField "evaulation"
evaulationStateField = EvaulationStateField "evaulation-state"
evaulationKeyField   = EvaulationKeyField "evaulation-key"

instance SnapFieldName EvaulationField where
  fieldName = fromString . evFieldName

data CommentField
  = CommentKeyField   { ckFieldName :: String }
  | CommentValueField { ckFieldName :: String }

commentKeyField = CommentKeyField "comment-key"
commentValueField = CommentValueField "comment-value"

instance SnapFieldName CommentField where
  fieldName = fromString . ckFieldName

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
  , SFN userRoleField,  SFN loginSubmitBtn,  SFN assignmentDescField,    SFN assignmentTCsField
  , SFN selectedCourse, SFN selectedCourseAdmin,       SFN groupRegistrationField, SFN evaulationValueField
  , SFN assignmentTypeField, SFN assignmentStartField, SFN assignmentEndField,     SFN evaulationStateField
  , SFN assignmentKeyField, SFN assignmentEvField,     SFN submissionKeyField,     SFN evaulationKeyField
  , SFN commentKeyField,SFN commentValueField
  ], (map SFN P.allPages)
  ]

unitTests = UnitTests [
    ( "Field names must be unique"
      , ((Set.size . Set.fromList $ fieldList) == (length fieldList)) )
  ]
