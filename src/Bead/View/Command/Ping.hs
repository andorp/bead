{-# LANGUAGE TypeFamilies #-}
module Bead.View.Command.Ping where

import qualified Data.Text as Text
import           Bead.Shared.Command
import           Bead.View.Command.Fayax


ping = FCPing ()

instance Command Ping where
  type Answer Ping = Pong
  fayaxInteract p = do
    let pingmsg = Text.unpack $ pingmessage p
        pongmsg = Text.pack   $ concat ["PONG: ", pingmsg ]
    return . HSuccess $ Pong { pongmessage = pongmsg }
