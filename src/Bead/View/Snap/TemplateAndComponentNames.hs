{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.TemplateAndComponentNames where

-- This module contains information about templates and
-- fields in the type safe manner.

-- Haskell imports
import Data.String
import Control.Monad (join)
import Bead.View.Snap.Fay.HookIds
import qualified Bead.Controller.Pages as P

-- Test imports
#ifdef TEST
import Bead.Invariants (UnitTests(..))
#endif
import Data.Set (Set)
import qualified Data.Set as Set

-- * Type safe declarations

class SnapFieldName f where
  fieldName :: (IsString s) => f -> s

class SnapClassName c where
  className :: (IsString s) => c -> s

class SnapFormId f where
  formId :: (IsString s) => f -> s

instance SnapFormId FormId where
  formId = fromString . rFormId

newtype SubmitButton = SubmitButton { sbFieldName :: String }

instance SnapFieldName SubmitButton where
  fieldName = fromString . sbFieldName

newtype FieldName = FieldName String

instance SnapFieldName FieldName where
  fieldName (FieldName f) = fromString f

-- * Component names

instance SnapFieldName LoginField where
  fieldName = fromString . lcFieldName

loginSubmitBtn = SubmitButton "login-submit"
regSubmitBtn   = SubmitButton "reg-submit"
pwdSubmitBtn   = SubmitButton "pwd-submit"
regGroupSubmitBtn = SubmitButton "reg-group-submit"
createGroupBtn    = SubmitButton "crt-group-submit"
createCourseBtn = SubmitButton "crt-course-submit"
assignBtn = SubmitButton "asg-assign-submit"
selectBtn = SubmitButton "select-submit"
saveEvalBtn = SubmitButton "save-eval-submit"
saveSubmitBtn = SubmitButton "save-submit-button"
submitSolutionBtn = SubmitButton "submit-solution-btn"
commentBtn = SubmitButton "comment-submit-btn"
saveChangesBtn = SubmitButton "save-changes-btn"
assignGroupAdminBtn = SubmitButton "asg-group-admin-submit"
changeProfileBtn = SubmitButton "change-profile"
changePasswordBtn = SubmitButton "change-password"

instance SnapFieldName RegistrationComp where
  fieldName = fromString . rFieldName

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
  | GroupEvalField { gFieldName :: String }

instance SnapFieldName GroupField where
  fieldName = fromString . gFieldName

groupCodeField = GroupCodeField "group-code"
groupNameField = GroupNameField "group-name"
groupDescField = GroupDescField "group-desc"
groupEvalField = GroupEvalField "group-eval"

data UserField
  = UserField  { uFieldName :: String }
  | UserEmailField { uFieldName :: String }
  | UserRoleField  { uFieldName :: String }
  | UserFamilyNameField { uFieldName :: String }
  | UserTimeZoneField { uFieldName :: String }

instance SnapFieldName UserField where
  fieldName = fromString . uFieldName

usernameField  = UserField "username"
userEmailField = UserEmailField "useremail"
userRoleField  = UserRoleField "userrole"
userFamilyNameField = UserFamilyNameField "userfamilyname"
userTimeZoneField = UserTimeZoneField "usertimezone"

instance SnapFieldName ChangePwdField where
  fieldName = fromString . cpf

menuId :: P.Page -> String
menuId P.Login          = "link-login"
menuId P.Logout         = "link-logout"
menuId P.Home           = "link-home"
menuId P.Profile        = "link-profile"
menuId P.Error          = "link-error"
menuId P.Administration = "link-admin"
menuId P.CourseAdmin    = "link-course-admin"
menuId P.EvaluationTable = "link-evaluation-table"
menuId (P.Evaluation _)  = "link-evaluation"
menuId P.Submission      = "link-submission"
menuId P.SubmissionList  = "link-submission-list"
menuId P.UserSubmissions = "link-user-submissions"
menuId (P.ModifyEvaluation _ _) = "link-modify-evaluation"
menuId (P.SubmissionDetails _ _)  = "link-submission-details"
menuId P.GroupRegistration = "link-group-registration"
menuId P.CreateCourse = "link-create-course"
menuId P.UserDetails = "link-user-details"
menuId P.AssignCourseAdmin = "link-assign-course-admin"
menuId P.CreateGroup = "link-create-group"
menuId P.AssignGroupAdmin = "link-assign-group-admin"
menuId P.NewGroupAssignment  = "link-new-group-assignment"
menuId P.NewCourseAssignment = "link-new-course-assignment"
menuId P.ModifyAssignment = "link-modify-assignment"
menuId P.ChangePassword = "link-change-password"
menuId P.SetUserPassword = "link-set-user-password"
menuId (P.CommentFromEvaluation _) = "link-comment-from-evaluation"
menuId (P.CommentFromModifyEvaluation _ _) = "link-comment-from-modify-evaluation"

instance SnapFieldName P.Page where
  fieldName = fromString . menuId

data AssignmentField
  = AssignmentDescField { aFieldName :: String }
  | AssignmentNameField { aFieldName :: String }
  | AssignmentTCsField { aFieldName :: String }
  | AssignmentTypeField { aFieldName :: String }
  | AssignmentKeyField { aFieldName :: String }
  | AssignmentEvField { aFieldName :: String }

assignmentNameField  = AssignmentNameField  "asg-name"
assignmentDescField  = AssignmentDescField  "asg-desc"
assignmentTCsField   = AssignmentTCsField   "asg-tcs"
assignmentTypeField  = AssignmentTypeField  "asg-type"
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

data AssignCourseGroupAdminField
  = SelectedGroupAdmin { cpFieldName :: String }
  | SelectedGroup      { cpFieldName :: String }

selectedGroup = SelectedGroup "selected-group"
selectedGroupAdmin = SelectedGroupAdmin "selected-group-admin"

instance SnapFieldName AssignCourseGroupAdminField where
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

data EvaluationField
  = EvaluationValueField { evFieldName :: String }
  | EvaluationStateField { evFieldName :: String }
  | EvaluationKeyField   { evFieldName :: String }

evaluationValueField = EvaluationValueField "evaluation"
evaluationKeyField   = EvaluationKeyField "evaluation-key"

instance SnapFieldName EvaluationField where
  fieldName = fromString . evFieldName

data CommentField
  = CommentKeyField   { ckFieldName :: String }
  | CommentValueField { ckFieldName :: String }

commentKeyField = CommentKeyField "comment-key"
commentValueField = CommentValueField "comment-value"

instance SnapFieldName CommentField where
  fieldName = fromString . ckFieldName

newtype ChangeLanguageField = ChangeLanguageField { clgFieldName :: String }

changeLanguageField = ChangeLanguageField "change-language"
userLanguageField = ChangeLanguageField "user-change-language"

instance SnapFieldName ChangeLanguageField where
  fieldName = fromString . clgFieldName

data TableName = TableName {
    tName :: String
  }

instance SnapFieldName TableName where
  fieldName = fromString . tName

availableAssignmentsTable = TableName "available-assignments"
submissionTableName = TableName "submission-table"
registrationTable = TableName "reg-form-table"
resetPasswordTable = TableName "rst-pwd-table"
profileTable = TableName "profile-table"
changePasswordTable = TableName "change-password-table"

-- * Template names

newtype LoginTemp = LoginTemp String
  deriving (Eq)

loginTemp = LoginTemp "login"

-- * Class names

data TableClassName = TableClassName {
    tcName :: String
  }

instance SnapClassName TableClassName where
  className = fromString . tcName

evaluationClassTable = TableClassName "evaluation-table"
submissionListTable = TableClassName "submission-list-table"
groupSubmissionTable = TableClassName "group-submission-table"
userSubmissionTable = TableClassName "user-submission-table"
assignmentTable = TableClassName "assignment-table"

data DivClassName = DivClassName {
    divClass :: String
  }

instance SnapClassName DivClassName where
  className = fromString . divClass

submissionListDiv = DivClassName "submission-list-div"

instance SnapFieldName HookId where
  fieldName = fromString . hookId

instance SnapClassName HookClass where
  className = fromString . hookClass

#ifdef TEST

-- * Unit tests

data SFN = forall n . SnapFieldName n => SFN n
         | forall n . SnapFormId n    => SFI n

instance SnapFieldName SFN where
  fieldName (SFN n) = fieldName n
  fieldName (SFI n) = formId n

data SCN = forall n . SnapClassName n => SCN n

instance SnapClassName SCN where
  className (SCN n) = className n

fieldList :: [String]
fieldList = map fieldName $ join [
  [ SFN loginUsername,  SFN loginPassword,   SFN regFullName, SFN regEmailAddress, SFN regTimeZoneField
  , SFN exerciseForm,   SFN exerciseKey,     SFN coursesForm,            SFN coursesKey
  , SFN courseFormInfo, SFN courseCodeField, SFN courseNameField,        SFN courseDescField
  , SFN groupKeyName,   SFN groupCodeField,  SFN groupNameField,         SFN groupDescField
  , SFN usernameField,  SFN courseKeyInfo,   SFN userEmailField,         SFN userFamilyNameField
  , SFN userRoleField,  SFN loginSubmitBtn,  SFN assignmentDescField,    SFN assignmentTCsField
  , SFN selectedCourse, SFN selectedCourseAdmin,       SFN groupRegistrationField, SFN evaluationValueField
  , SFN assignmentTypeField, SFN assignmentStartField, SFN assignmentEndField,     SFN evaluationResultField
  , SFN assignmentKeyField, SFN assignmentEvField,     SFN submissionKeyField,     SFN evaluationKeyField
  , SFN commentKeyField,SFN commentValueField, SFN regSubmitBtn, SFN regGroupSubmitBtn, SFN createGroupBtn
  , SFN assignGroupAdminBtn, SFN createCourseBtn, SFN assignBtn, SFN selectBtn, SFN saveEvalBtn
  , SFN saveSubmitBtn, SFN submitSolutionBtn, SFN commentBtn, SFN saveChangesBtn
  , SFN availableAssignmentsTable, SFN submissionTableName, SFN groupEvalField, SFN profileTable
  , SFN changePasswordTable, SFN oldPasswordField, SFN newPasswordField, SFN newPasswordAgainField
  , SFN assignmentStartDateField, SFN assignmentEndDateField
  , SFN assignmentStartHourField, SFN assignmentStartMinField
  , SFN assignmentEndHourField, SFN assignmentEndMinField
  , SFN assignmentStartDefaultDate, SFN assignmentStartDefaultHour, SFN assignmentStartDefaultMin
  , SFN assignmentEndDefaultDate, SFN assignmentEndDefaultHour, SFN assignmentEndDefaultMin
  , SFN studentNewPwdField, SFN studentNewPwdAgainField, SFN pctHelpMessage, SFN changeLanguageField
  , SFN userLanguageField

  , SFN createCourseForm, SFN evaluationTypeSelection, SFN evaluationTypeValue, SFN startDateDivId
  , SFN evalTypeSelectionDiv, SFN registrationTable, SFN createGroupForm, SFN endDateDivId
  , SFN evaluationPercentageDiv, SFN regUserRegKey, SFN regToken, SFN pwdSubmitBtn
  , SFN resetPasswordTable, SFN regPasswordAgain, SFN changeProfileBtn, SFN changePasswordBtn
  , SFN userTimeZoneField, SFN assignmentForm, SFI changePwdForm, SFI setStudentPwdForm

  , SFI regForm, SFI loginForm, SFI regFinalForm
  ]
--  ], (map SFN P.allPages)
  ]

classList :: [String]
classList = map className [
    SCN evaluationClassTable, SCN groupSubmissionTable, SCN assignmentTable
  , SCN submissionListTable, SCN submissionListDiv, SCN datePickerClass, SCN minuteSpinnerClass
  , SCN hourSpinnerClass
  ]

names = fieldList ++ classList

unitTests = UnitTests [
    ( "Field names must be unique"
      , ((Set.size . Set.fromList $ names) == (length names)) )
  ]

#endif
