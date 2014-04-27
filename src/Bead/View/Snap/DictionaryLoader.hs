module Bead.View.Snap.DictionaryLoader where

import           Control.Applicative ((<$>))
import           Control.Arrow ((&&&))
import           Control.Monad (filterM)
import qualified Data.Map as Map
import           Data.Maybe
import           System.Directory
import           System.FilePath (takeExtension, joinPath)

import           Bead.View.Snap.Dictionary
import           Bead.Domain.Types

-- Reads up all the dictionary files from a given directory, and
-- creates the dictionaries map
loadDictionaries :: FilePath -> IO Dictionaries
loadDictionaries d = do
  fs <- ((map fullPath  . filter dictionaryFile) <$> getDirectoryContents d) >>= (filterM doesFileExist)
  ds <- catMaybes <$> mapM loadDictionary fs
  return . Map.fromList
         . map ((Language . langCode) &&& (dictionaryFromDFile &&& dictionaryFileToInfo))
         $ ds
    where
      fullPath f = joinPath [d,f]
      dictionaryFile = (".dict"==) . takeExtension

      loadDictionary :: FilePath -> IO (Maybe DictionaryFile)
      loadDictionary f = readMaybe <$> readFile f

