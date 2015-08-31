{-# LANGUAGE TypeFamilies #-}
module Bead.View.Command.Refresh where

import           Bead.Shared.Command
import           Bead.View.Command.Fayax


refresh = FCRefresh ()

instance Command Refresh where
  type Answer Refresh = RefreshReply
  fayaxInteract p = do
    return . HSuccess $ RefreshReply { timestamp = 0 } -- XXX : constant

