module Bead.View.Snap.Fay.Hooks where

import Bead.View.Snap.Fay.HookIds

data EvaulationHook = EvaulationHook {
    evFormId        :: String
  , evSelectionId   :: String
  , evHiddenValueId :: String
  , evSelectionDivId :: String
  }

createCourseHook = EvaulationHook {
    evFormId        = hookId createCourseForm
  , evSelectionId   = hookId evaulationTypeSelection
  , evHiddenValueId = hookId evaulationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  }

createGroupHook = EvaulationHook {
    evFormId      = hookId createGroupForm
  , evSelectionId = hookId evaulationTypeSelection
  , evHiddenValueId = hookId evaulationTypeValue
  , evSelectionDivId = hookId evalTypeSelectionDiv
  }



data DateTimePickerHook = DateTimePickerHook {
    dtDivId         :: String
  , dtHiddenInputId :: String
  }

startDateTimeHook = DateTimePickerHook {
    dtDivId = hookId startDateDivId
  , dtHiddenInputId = hookId assignmentStartField
  }

endDateTimeHook = DateTimePickerHook {
    dtDivId = hookId endDateDivId
  , dtHiddenInputId = hookId assignmentEndField
  }



data PercentageHook = PercentageHook {
    ptDivId :: String
  , ptHiddenInputId :: String
  }

evaulationPctHook = PercentageHook {
    ptDivId = hookId evaulationPercentageDiv
  , ptHiddenInputId = hookId evaulationResultField
  }


