{-# LANGUAGE TemplateHaskell #-}
module Bead.View.Translation.Entries where

import Bead.View.Translation.Base

$(generateTranslationEntries labels)
