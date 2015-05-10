{-# LANGUAGE CPP #-}
module Bead.Config (
    InitTask(..)
  , Config(..)
#ifdef LDAPEnabled
  , LDAPLoginConfig(..)
  , ldapLoginConfig
#else
  , StandaloneLoginConfig(..)
  , standaloneLoginConfig
#endif
  , defaultConfiguration
  , configCata
  , initTasks
  , Usage
  , substProgName
  , readConfiguration
#ifdef TEST
  , initTaskAssertions
#endif
  ) where

import Control.Monad (join)

import System.Directory (doesFileExist)

import Bead.Config.Configuration
import Bead.Config.Parser

#ifdef TEST
import Bead.Invariants
#endif

-- Represents the hostname (and/or port) of the bead server
type Hostname = String
type Second   = Int

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  exist <- doesFileExist path
  case exist of
    False -> do
      putStrLn "Configuration file does not exist"
      putStrLn "!!! DEFAULT CONFIGURATION IS USED !!!"
      return defaultConfiguration
    True  -> do
      content <- readFile path
      case parseYamlConfig content of
        Left err -> do
          putStrLn "Configuration is not parseable"
          putStrLn "!!! DEFAULT CONFIGURATION IS USED !!!"
          putStrLn $ "Reason: " ++ err
          return defaultConfiguration
        Right c -> return c

-- Consumes the argument list and produces a task list
-- Produces Left "usage function" if invalid options or extra arguments is given
-- otherwise Right "tasklist"
initTasks :: [String] -> Either Usage [InitTask]
initTasks arguments = case filter ((/='-') . head) arguments of
  []        -> Right []
  ["admin"] -> Right [CreateAdmin]
  _         -> Left $ Usage (\p -> join [p, " [OPTION...] [admin]"])

#ifdef TEST
initTaskAssertions = [
    Assertion "Empty config list"   (initTasks [])     (Right [])
  , Assertion "Create admin option" (initTasks ["admin"]) (Right [CreateAdmin])
  , AssertPredicate "Two options"             (initTasks ["admin","b"]) isLeft
  ] where
      isLeft (Left _) = True
      isLeft _        = False
#endif
