module Bead.View.Snap.DictionaryLoader where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Control.Monad (filterM, mapM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.IO
import System.Directory
import System.FilePath (takeExtension, joinPath, takeBaseName)

import Bead.View.Snap.Dictionary
import Bead.Domain.Types

loadDictionaries :: FilePath -> IO Dictionaries
loadDictionaries d = do
  fs <- ((map fullPath  . filter dictionaryFile) <$> getDirectoryContents d) >>= (filterM doesFileExist)
  ds <- catMaybes <$> mapM loadDictionary fs
  return . Map.fromList . map ((Language . language) &&& dictionaryFromDFile) $ ds
    where
      fullPath f = joinPath [d,f]
      dictionaryFile = (".dict"==) . takeExtension

loadDictionary :: FilePath -> IO (Maybe DictionaryFile)
loadDictionary f = readMaybe <$> readFile f

