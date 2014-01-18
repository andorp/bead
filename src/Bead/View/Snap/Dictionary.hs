module Bead.View.Snap.Dictionary (
    Entry(..)
  , I18N
  , Dictionary
  , Dictionaries
  , DictionaryFile(..) -- Represend a dictionary on the file system
  , Language(..)
  , dictionary
  , unDictionary
  , dictionaryFromDFile -- Creates a Dictionary from the DictionaryFile structure
  , idDictionary
  ) where

-- Haskell imports

import Data.String
import Data.Maybe (maybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import System.IO

-- Bead imports

import Bead.Domain.Types (readMaybe)
import Bead.View.Snap.Translation

-- * Definitions

-- I18N is a mapping from a given translation key
-- to the actual translation of the value
type I18N = Translation String -> String

-- Dictionary is just a translation
newtype Dictionary = Dictionary { unDictionary :: I18N }

instance Show Dictionary where
  show _ = "Dictionary"

newtype Language = Language String
  deriving (Eq, Show, Read, Ord)

type Dictionaries = Map Language Dictionary

-- A dictionary file entry is just a translation value,
-- which represents the translation key and the translated
-- value for that key
type Entry = Translation String

data DictionaryFile = DictionaryFile {
    iconFile :: String -- Icon file path within the static folder
  , language :: String -- The name of the language for this dictionary
  , entries  :: [Entry] -- The entry list for the dictionary
  } deriving (Show, Read)

dictionaryFileCata f (DictionaryFile iconFile language entries) =
  f iconFile language entries

-- Creates a new dictionary from the entries of the dictionary file,
-- if no translation key is found in the entries, the original value
-- would be used as the text of the translation
dictionaryFromDFile :: DictionaryFile -> Dictionary
dictionaryFromDFile = dictionaryFileCata $ \_icon _lang entries ->
  Dictionary {
    unDictionary = \key ->
      let key' = createKey key
      in maybe (trans key) trans . Map.lookup key' . Map.fromList $ map (createKey &&& id) entries
  }
  where
    createKey k = k { trans = () }

dictionary :: I18N -> Dictionary
dictionary = Dictionary

idDictionary :: Dictionary
idDictionary = Dictionary trans

