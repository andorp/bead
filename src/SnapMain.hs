{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.FilePath (FilePath, joinPath)
import System.IO (hFlush, hSetEcho, stdout, stdin)

import Bead.Configuration
import Bead.Controller.ServiceContext as S
import Bead.View.Snap.Application
import Bead.View.Snap.AppInit
import Bead.View.Snap.Dictionary (Dictionaries)
import Bead.View.Snap.DictionaryLoader (loadDictionaries)
import Bead.View.Snap.Logger
import Bead.View.Snap.Validators hiding (toLower)
import Bead.Persistence.Persist (initPersistence, isPersistenceSetUp)
import qualified Bead.Controller.Logging as L
import qualified Bead.Persistence.NoSQLDir as P

import Snap hiding (Config(..))
import Snap.Snaplet

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

-- Creates a service context that includes the given logger
createContext :: L.Logger -> IO ServiceContext
createContext logger = do
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
  hSetEcho stdin True
  args <- getArgs
  config <- readConfiguration beadConfigFileName
  newAdminUser <- either (const $ return Nothing) interpretTasks (initTasks args)
  startService config newAdminUser

interpretTasks :: [InitTask] -> IO (Maybe (String, String))
interpretTasks tasks = case elem CreateAdmin tasks of
  False -> return Nothing
  True  -> fmap Just readAdminUser

readAdminUser :: IO (String, String)
readAdminUser = do
  putStrLn "Creating admin user, all characters are converted to lower case."
  putStrLn "Username must be in the neptun code format."
  usr <- readUsername
  hSetEcho stdin False
  putStr "Password: " >> hFlush stdout
  pwd <- getLine
  putStrLn ""
  putStr "Password again: " >> hFlush stdout
  pwd2 <- getLine
  putStrLn ""
  hSetEcho stdin True
  case pwd == pwd2 of
    True  -> return (usr, pwd)
    False -> do
      putStrLn "Passwords do not match!"
      readAdminUser
  where
    readUsername = do
      putStr "Admin User: " >> hFlush stdout
      usr <- fmap (map toUpper) getLine
      validate
        isUsername
        usr
        -- Valid username
        (return usr)
        -- Invalid username
        (\msg -> do putStrLn $ "Username format is invalid! " ++ msg
                    readUsername)

startService :: Config -> Maybe (String, String) -> IO ()
startService config newAdminUser = do
  userActionLogs <- createSnapLogger . userActionLogFile $ config

  context <- createContext (snapLogger userActionLogs)

  let dictionaryDir = "dic"
  dExist <- doesDirectoryExist dictionaryDir
  dictionaries <- case dExist of
    True -> loadDictionaries dictionaryDir
    False -> return Map.empty

  putStrLn $ "Found dictionaries: " ++ (show $ Map.keys dictionaries)

  serveSnaplet defaultConfig (appInit config newAdminUser context dictionaries)
  stopLogger userActionLogs
