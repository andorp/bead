module Bead.View.Snap.Fay.HookIds where

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

data HookClass = HookClass { hookClass :: String }

datePickerClass = HookClass "datepicker"
hourSpinnerClass = HookClass "hourspinner"
minuteSpinnerClass = HookClass "minutespinner"

