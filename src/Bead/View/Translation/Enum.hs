{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Bead.View.Translation.Enum (
    Enum(..)
  , Bounded(..)
  , Translation(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Bead.View.Translation.Base
import           Bead.View.Translation.Entries

translationList = $(generateTranslationEnumList labels)

translationMaxIndex = (length translationList) - 1

translationToMap = Map.fromList $ zip [0..] translationList
translationFromMap = Map.fromList $ zip translationList [0..]

trToEnum n = maybe
  (error (concat ["There is no Translation key for the ", show n]))
  (id)
  (Map.lookup n translationToMap)

trFromEnum n = maybe
  (error (concat ["There is no int for the ", show n]))
  (id)
  (Map.lookup n translationFromMap)

instance Bounded (Translation ()) where
  minBound = trToEnum 0
  maxBound = trToEnum translationMaxIndex

instance Enum (Translation ()) where
  toEnum = trToEnum
  fromEnum = trFromEnum
