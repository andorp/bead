{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.FilePath (FilePath, joinPath)
import System.IO (hFlush, stdout)

import Bead.Configuration
import Bead.Controller.ServiceContext as S
import Bead.View.Snap.Application
import Bead.View.Snap.AppInit
import Bead.View.Snap.Dictionary (Dictionaries)
import Bead.View.Snap.DictionaryLoader (loadDictionaries)
import Bead.View.Snap.Logger
import Bead.Persistence.Persist (initPersistence, isPersistenceSetUp)
import qualified Bead.Controller.Logging as L
import qualified Bead.Persistence.NoSQLDir as P

import Snap hiding (Config(..))
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

-- Reads the command line arguments, interprets the init tasks and start
-- the service with the given config
main :: IO ()
main = do
  args <- getArgs
  case initTasks args of
    Left usage -> do
      progName <- getProgName
      putStrLn $ substProgName progName usage
    Right tasks -> do
      newAdminUser <- interpretTasks tasks
      config <- readConfiguration "bead.config"
      startService config newAdminUser

interpretTasks :: [InitTask] -> IO (Maybe (String, String))
interpretTasks tasks = case elem CreateAdmin tasks of
  False -> return Nothing
  True  -> fmap Just readAdminUser

readAdminUser :: IO (String, String)
readAdminUser = do
  putStrLn "Creating admin user"
  putStr "Admin user: " >> hFlush stdout
  usr <- getLine
  putStr "Password: " >> hFlush stdout
  pwd <- getLine
  return (usr, pwd)

startService :: Config -> Maybe (String, String) -> IO ()
startService config newAdminUser = do
  userActionLogs <- createSnapLogger . userActionLogFile $ config

  context <- c (snapLogger userActionLogs)

  let dictionaryDir = "dic"
  dExist <- doesDirectoryExist dictionaryDir
  dictionaries <- case dExist of
    True -> loadDictionaries dictionaryDir
    False -> return Map.empty

  putStrLn $ "Found dictionaries: " ++ (show $ Map.keys dictionaries)

  serveSnaplet defaultConfig (appInit config newAdminUser context dictionaries)
  stopLogger userActionLogs

