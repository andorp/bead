{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (FilePath, joinPath)

import Bead.Controller.ServiceContext as S
import Bead.View.Snap.Application
import Bead.View.Snap.AppInit
import Bead.View.Snap.Dictionary (Dictionaries)
import Bead.View.Snap.DictionaryLoader (loadDictionaries)
import Bead.View.Snap.Logger
import Bead.Persistence.Persist (initPersistence, isPersistenceSetUp)
import qualified Bead.Controller.Logging as L
import qualified Bead.Persistence.NoSQLDir as P

import Snap
import Snap.Snaplet

import Data.Map (Map)
import qualified Data.Map as Map


c :: L.Logger -> IO ServiceContext
c logger = do
  userContainer <- ioUserContainer
  isPersistSetUp <- isPersistenceSetUp P.noSqlDirPersist
  case isPersistSetUp of
    True -> return ()
    False -> initPersistence P.noSqlDirPersist
  S.serviceContext P.noSqlDirPersist userContainer logger

main :: IO ()
main = do
  userActionLogs <- createSnapLogger $ joinPath ["log", "useractions.log"]

  context <- c (snapLogger userActionLogs)

  let dictionaryDir = "dic"
  dExist <- doesDirectoryExist dictionaryDir
  dictionaries <- case dExist of
    True -> loadDictionaries dictionaryDir
    False -> return Map.empty

  putStrLn $ "Found dictionaries: " ++ (show $ Map.keys dictionaries)

  args <- getArgs
  let newAdminUser = case args of
                       [usr,pwd] -> Just (usr,pwd)
                       _         -> Nothing
  serveSnaplet defaultConfig (appInit newAdminUser context dictionaries)
  stopLogger userActionLogs
