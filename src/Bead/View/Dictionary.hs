{-# LANGUAGE DeriveDataTypeable #-}
module Bead.View.Dictionary (
    Entry(..)
  , I18N
  , Dictionary
  , Dictionaries -- A map from a language to dictionary and dictionary info
  , dictionariesCata -- The template function for the dictionaries values
  , DictionaryInfo(..) -- Information about a dictionary
  , dictionaryInfoCata -- The template function for the dictionaryInfo values
  , DictionaryFile(..) -- Represend a dictionary on the file system
  , Language(..) -- A language name for a dictionary
  , languageCata -- The template function for the language values
  , dictionary
  , unDictionary
  , dictionaryFromDFile -- Creates a Dictionary from the DictionaryFile structure
  , dictionaryFileToInfo -- Reads out the icon file name
  , idDictionary
  , (<|)
  ) where

-- Haskell imports

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Control.Arrow ((&&&))

-- Bead imports

import Bead.Domain.Entities (Language(..), languageCata)
import Bead.View.Translation

-- * Definitions

-- Dictionary is just a translation
newtype Dictionary = Dictionary { unDictionary :: I18N }

instance Show Dictionary where
  show _ = "Dictionary"

-- The dictionary info contains information about a
-- dictionary, it is only the name of the icon file
-- for a dictionary
data DictionaryInfo = DictionaryInfo {
    icon :: String
  , languageName :: String
  }

dictionaryInfoCata f (DictionaryInfo icon langName) = f icon langName

-- Dictionaries is a map from a given language to a Dictionary and
-- a dictionary info pair
type Dictionaries = Map Language (Dictionary, DictionaryInfo)

dictionariesCata f dictionaries = f dictionaries

-- A dictionary file entry is just a translation value,
-- which represents the translation key and the translated
-- value for that key
type Entry = Translation String

data DictionaryFile = DictionaryFile {
    iconFile :: String -- Icon file path within the static folder
  , langCode :: String -- The name of the language for a dictionary
  , langName :: String -- The displayable name of the language for a dictioanry
  , entries  :: [Entry] -- The entry list for a dictionary
  } deriving (Show, Read, Typeable)

dictionaryFileCata f (DictionaryFile iconFile langCode langName entries) =
  f iconFile langCode langName entries

(<|) :: (String -> Translation String) -> String -> Translation String
(<|) = ($)

-- Creates a new dictionary from the entries of the dictionary file,
-- if no translation key is found in the entries, the original value
-- would be used as the text of the translation
dictionaryFromDFile :: DictionaryFile -> Dictionary
dictionaryFromDFile = dictionaryFileCata $ \_icon _langCode _langName entries ->
  Dictionary {
    unDictionary = \key ->
      let key' = createKey key
      in maybe (trans key) trans . Map.lookup key' . Map.fromList $ map (createKey &&& id) entries
  }
  where
    createKey :: Translation String -> Translation ()
    createKey (T (n,_)) = T (n,())

-- Reads out the icon file name into the dictionary info
dictionaryFileToInfo :: DictionaryFile -> DictionaryInfo
dictionaryFileToInfo = dictionaryFileCata $ \icon _langCode langName _entires ->
  DictionaryInfo icon langName

dictionary :: I18N -> Dictionary
dictionary = Dictionary

idDictionary :: Dictionary
idDictionary = Dictionary trans
