module Bead.View.Snap.DictionaryLoader where

import Control.Arrow ((***))
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
  ds <- (map (id *** fromJust) . filter (isJust . snd)) <$> mapM loadDictionary fs
  return . Map.fromList . map (lang *** id) $ ds
    where
      fullPath f = joinPath [d,f]
      dictionaryFile = (".dict"==) . takeExtension
      lang = Language . takeBaseName

loadDictionary :: FilePath -> IO (FilePath, Maybe Dictionary)
loadDictionary f = do
  s <- readFile f
  return (f, fromEntries <$> readMaybe s)

