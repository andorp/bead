module Bead.View.Fay.Hooks where

import Prelude

import Bead.View.Fay.HookIds

data EvaluationHook = EvaluationHook {
    evFormId        :: String
  , evSelectionId   :: String
  , evHiddenValueId :: String
  , evSelectionDivId :: String
  , evHelpMessageId  :: String
  }

{-
createCourseHook = EvaluationHook {
    evFormId        = hookId createCourseForm
  , evSelectionId   = hookId evaluationTypeSelection
  , evHiddenValueId = hookId evaluationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  , evHelpMessageId  = hookId pctHelpMessage
  }

createGroupHook = EvaluationHook {
    evFormId      = hookId createGroupForm
  , evSelectionId = hookId evaluationTypeSelection
  , evHiddenValueId = hookId evaluationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  , evHelpMessageId  = hookId pctHelpMessage
  }
-}

assignmentEvTypeHook = EvaluationHook {
    evFormId      = hookId assignmentForm
  , evSelectionId = hookId evaluationTypeSelection
  , evHiddenValueId = hookId evaluationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  , evHelpMessageId  = hookId pctHelpMessage
  }

data DateTimePickerHook = DateTimePickerHook {
    dtDivId         :: String
  , dtHiddenInputId :: String
  , dtDatePickerId  :: String
  , dtHourPickerId  :: String
  , dtMinPickerId   :: String
  , dtDefaultDate   :: String
  , dtDefaultHour   :: String
  , dtDefaultMin    :: String
  }

startDateTimeHook = DateTimePickerHook {
    dtDivId = hookId startDateDivId
  , dtHiddenInputId = hookId assignmentStartField
  , dtDatePickerId = hookId assignmentStartDateField
  , dtHourPickerId = hookId assignmentStartHourField
  , dtMinPickerId  = hookId assignmentStartMinField
  , dtDefaultDate = hookId assignmentStartDefaultDate
  , dtDefaultHour = hookId assignmentStartDefaultHour
  , dtDefaultMin  = hookId assignmentStartDefaultMin
  }

endDateTimeHook = DateTimePickerHook {
    dtDivId = hookId endDateDivId
  , dtHiddenInputId = hookId assignmentEndField
  , dtDatePickerId = hookId assignmentEndDateField
  , dtHourPickerId = hookId assignmentEndHourField
  , dtMinPickerId  = hookId assignmentEndMinField
  , dtDefaultDate = hookId assignmentEndDefaultDate
  , dtDefaultHour = hookId assignmentEndDefaultHour
  , dtDefaultMin  = hookId assignmentEndDefaultMin
  }



data PercentageHook = PercentageHook {
    ptDivId :: String
  , ptHiddenInputId :: String
  }

evaluationPctHook = PercentageHook {
    ptDivId = hookId evaluationPercentageDiv
  , ptHiddenInputId = hookId evaluationResultField
  }

