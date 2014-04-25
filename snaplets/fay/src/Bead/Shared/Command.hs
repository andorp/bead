{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Shared.Command where

import           Data.Data
import           Fay.Text

#ifdef FAY
import           Prelude
import           FFI
#endif

-- * Commands

data Ping = Ping { pingmessage :: Text }
  deriving (Data, Read, Show, Typeable)

-- * Results

data Pong = Pong { pongmessage :: Text }
  deriving (Data, Read, Show, Typeable)

-- Fayax Commands ADT

data FayaxCommand a
  = FCPing a
  deriving (Eq, Show)

fayaxCommandCata
  ping
  c = case c of
    FCPing x -> ping x

fayaxCmdValue = fayaxCommandCata
  id

-- Command constants
fayaxCmdConsts
  ping
  = fayaxCommandCata
      (const $ FCPing ping)

-- Command submit button ids
fayaxSubmitId = fayaxCmdConsts
  "pingsubmit"
