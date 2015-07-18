module Bead.View.Fay.HookIds where

import Prelude

data HookId = HookId { hookId :: String }

createCourseForm = HookId "create-course-form"
createGroupForm = HookId "create-group-form"
assignmentForm = HookId "assignment-form"
evaluationTypeSelection = HookId "eval-type-selection"
evaluationTypeValue = HookId "eval-type-value"
evalTypeSelectionDiv = HookId "eval-type-selection-div"

startDateDivId = HookId "start-date-div"
endDateDivId = HookId "end-date-div"

assignmentStartField = HookId "asg-start"
assignmentStartDefaultDate = HookId "asg-start-def-date"
assignmentStartDefaultHour = HookId "asg-start-def-hour"
assignmentStartDefaultMin = HookId "asg-start-def-min"
assignmentEndField = HookId "asg-end"
assignmentEndDefaultDate = HookId "asg-end-def-date"
assignmentEndDefaultHour = HookId "asg-end-def-hour"
assignmentEndDefaultMin = HookId "asg-end-def-min"
assignmentStartDateField = HookId "asg-start-date"
assignmentStartHourField = HookId "asg-start-hour"
assignmentStartMinField = HookId "asg-start-min"
assignmentEndDateField = HookId "asg-end-date"
assignmentEndHourField = HookId "asg-end-hour"
assignmentEndMinField = HookId "asg-end-min"

evaluationPercentageDiv = HookId "evaluation-pct-div"
evaluationResultField = HookId "evaluation-result"

pctHelpMessage = HookId "pct-help-message"

evCommentOnlyText = HookId "ev-comment-only-text"

data HookClass = HookClass { hookClass :: String }

datePickerClass = HookClass "datepicker"
hourSpinnerClass = HookClass "hourspinner"
minuteSpinnerClass = HookClass "minutespinner"
seeMoreClass = HookClass "seemore"
seeLessClass = HookClass "seeless"
moreClass = HookClass "more"
moreButtonClass = HookClass "morebutton"
lessButtonClass = HookClass "lessbutton"

data LoginField = LoginField { lcFieldName :: String }

loginUsername = LoginField "login"
loginPassword = LoginField "password"
regPasswordAgain = LoginField "password_again"

newtype ChangePwdField = ChangePwdField { cpf :: String }

oldPasswordField = ChangePwdField "old-password-field"
newPasswordField = ChangePwdField "new-password-field"
newPasswordAgainField = ChangePwdField "new-password-again-field"
studentNewPwdField = ChangePwdField "new-stn-pwd-field"
studentNewPwdAgainField = ChangePwdField "new-stn-pwd-again-field"

data RegistrationComp = RegComp { rFieldName :: String }

regFullName     = RegComp "reg_full_name"
regEmailAddress = RegComp "reg_email_address"
regUserRegKey   = RegComp "reg_user_reg_key"
regToken        = RegComp "reg_token"
regLanguage     = RegComp "reg_language"
regTimeZoneField = RegComp "reg_time_zone"

newtype SubmissionField = SubmissionField { sfFieldName :: String }

submissionTextField = SubmissionField "submission-text"
submissionFileField = SubmissionField "submission-file"
submissionKeyField  = SubmissionField "submission-key"
submissionPwdField  = SubmissionField "submission-pwd"

data FormId = FormId { rFormId :: String }

loginForm = FormId "login-form"
regForm = FormId "reg-form"
regFinalForm = FormId "reg-final-form"
changePwdForm = FormId "change-password-form"
setStudentPwdForm = FormId "set-student-ped-form"
submissionForm = FormId "submission-form"
