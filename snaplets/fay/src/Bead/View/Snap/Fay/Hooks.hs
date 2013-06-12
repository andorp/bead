module Bead.View.Snap.Fay.Hooks (
    EvaulationHook
  , evFormId
  , evSelectionId
  , evHiddenValueId
  , evSelectionDivId
  , createCourseHook
  , createGroupHook
  ) where

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
