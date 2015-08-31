{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Command.Fayax (
    Command(..)
  , HandlerResult(..)
  , module Data.Data
  ) where

import           Data.Data

import           Bead.View.BeadContext (BeadHandler)
import           Bead.View.LoggedInFilter

-- The command is received through ajax request and the answer of
-- that command is send as the response to the client.
class (Data c, Read c, Show (Answer c)) => Command c where
  type Answer c
  fayaxInteract :: c -> BeadHandler (HandlerResult (Answer c))
