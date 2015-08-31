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

data Refresh = Refresh
  deriving (Data, Read, Show, Typeable)

-- * Results

data Pong = Pong { pongmessage :: Text }
  deriving (Data, Read, Show, Typeable)

data RefreshReply = RefreshReply { timestamp :: Int }
  deriving (Data, Read, Show, Typeable)

-- Fayax Commands ADT

data FayaxCommand a
  = FCPing a
  | FCRefresh a
  deriving (Eq, Show)

#ifndef FAY
instance Functor FayaxCommand where
  fmap f (FCPing x)    = FCPing (f x)
  fmap f (FCRefresh x) = FCRefresh (f x)
#endif

fayaxCommandCata
  ping
  refresh
  c = case c of
    FCPing x -> ping x
    FCRefresh x -> refresh x

fayaxCmdValue = fayaxCommandCata
  id
  id

-- Command constants
fayaxCmdConsts
  ping
  refresh
  = fayaxCommandCata
      (const $ FCPing ping)
      (const $ FCRefresh refresh)

-- Command submit button ids
fayaxSubmitId = fayaxCmdConsts
  "pingsubmit"
  "refreshsubmit"
