module Bead.View.Snap.Fay.JSON.ClientSide where

import FFI
import Prelude
import Data.Data
import Bead.Domain.Shared.Evaluation

toEvResultJSON :: EvResult -> String
toEvResultJSON = ffi "JSON.stringify(%1)"
