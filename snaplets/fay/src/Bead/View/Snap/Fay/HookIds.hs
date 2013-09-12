module Bead.View.Snap.Fay.HookIds where

import Prelude

data HookId = HookId { hookId :: String }

createCourseForm = HookId "create-course-form"
createGroupForm = HookId "create-group-form"
evaulationTypeSelection = HookId "eval-type-selection"
evaulationTypeValue = HookId "eval-type-value"
evalTypeSelectionDiv = HookId "eval-type-selection-div"

startDateDivId = HookId "start-date-div"
endDateDivId = HookId "end-date-div"

assignmentStartField = HookId "asg-start"
assignmentEndField = HookId "asg-end"

evaulationPercentageDiv = HookId "evaulation-pct-div"
evaulationResultField = HookId "evaulation-result"

data HookClass = HookClass { hookClass :: String }

datePickerClass = HookClass "datepicker"
hourSpinnerClass = HookClass "hourspinner"
minuteSpinnerClass = HookClass "minutespinner"

data LoginField = LoginField { lcFieldName :: String }

loginUsername = LoginField "login"
loginPassword = LoginField "password"
regPasswordAgain = LoginField "password_again"

data RegistrationComp = RegComp { rFieldName :: String }

regFullName     = RegComp "reg_full_name"
regEmailAddress = RegComp "reg_email_address"
regUserRegKey   = RegComp "reg_user_reg_key"
regToken        = RegComp "reg_token"

data FormId = FormId { rFormId :: String }

loginForm = FormId "login-form"
regForm = FormId "reg-form"
regFinalForm = FormId "reg-final-form"
