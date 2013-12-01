module Bead.View.Snap.Fay.Hooks where

import Prelude

import Bead.View.Snap.Fay.HookIds

data EvaulationHook = EvaulationHook {
    evFormId        :: String
  , evSelectionId   :: String
  , evHiddenValueId :: String
  , evSelectionDivId :: String
  , evHelpMessageId  :: String
  }

createCourseHook = EvaulationHook {
    evFormId        = hookId createCourseForm
  , evSelectionId   = hookId evaulationTypeSelection
  , evHiddenValueId = hookId evaulationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  , evHelpMessageId  = hookId pctHelpMessage
  }

createGroupHook = EvaulationHook {
    evFormId      = hookId createGroupForm
  , evSelectionId = hookId evaulationTypeSelection
  , evHiddenValueId = hookId evaulationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  , evHelpMessageId  = hookId pctHelpMessage
  }

data DateTimePickerHook = DateTimePickerHook {
    dtDivId         :: String
  , dtHiddenInputId :: String
  , dtDatePickerId  :: String
  , dtDefaultDate   :: String
  , dtDefaultHour   :: String
  , dtDefaultMin    :: String
  }

startDateTimeHook = DateTimePickerHook {
    dtDivId = hookId startDateDivId
  , dtHiddenInputId = hookId assignmentStartField
  , dtDatePickerId = hookId assignmentStartDateField
  , dtDefaultDate = hookId assignmentStartDefaultDate
  , dtDefaultHour = hookId assignmentStartDefaultHour
  , dtDefaultMin  = hookId assignmentStartDefaultMin
  }

endDateTimeHook = DateTimePickerHook {
    dtDivId = hookId endDateDivId
  , dtHiddenInputId = hookId assignmentEndField
  , dtDatePickerId = hookId assignmentEndDateField
  , dtDefaultDate = hookId assignmentEndDefaultDate
  , dtDefaultHour = hookId assignmentEndDefaultHour
  , dtDefaultMin  = hookId assignmentEndDefaultMin
  }



data PercentageHook = PercentageHook {
    ptDivId :: String
  , ptHiddenInputId :: String
  }

evaulationPctHook = PercentageHook {
    ptDivId = hookId evaulationPercentageDiv
  , ptHiddenInputId = hookId evaulationResultField
  }

