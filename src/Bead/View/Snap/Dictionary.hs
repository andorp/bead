module Bead.View.Snap.Dictionary (
    Entry(..)
  , I18N
  , Dictionary
  , Dictionaries
  , Language(..)
  , dictionary
  , unDictionary
  , fromEntries
  , fileDictionary
  , idDictionary
  ) where

-- Haskell imports

import Data.String
import Data.Maybe (maybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<$>))
import System.IO

-- Bead imports

import Bead.Domain.Types (readMaybe)
import Bead.View.Snap.Translation

-- * Definitions

type I18N = Translation String -> String

newtype Dictionary = Dictionary { unDictionary :: I18N }

instance Show Dictionary where
  show _ = "Dictionary"

newtype Language = Language String
  deriving (Eq, Show, Ord)

type Dictionaries = Map Language Dictionary

data Entry = Entry {
    original    :: String
  , translation :: String
  } deriving (Show, Read)

dictionary :: I18N -> Dictionary
dictionary = Dictionary

fromEntries :: [Entry] -> Dictionary
fromEntries _ = idDictionary
{-
fromEntries es = Dictionary {
    unDictionary = \k -> maybe k id . Map.lookup k . Map.fromList . map p $ es
  } where
      p (Entry o t) = (o,t)
-}

fileDictionary :: FilePath -> IO (Maybe Dictionary)
fileDictionary = (((fromEntries <$>) .  readMaybe) <$>) . readFile

idDictionary :: Dictionary
idDictionary = Dictionary trans

