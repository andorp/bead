{-# LANGUAGE CPP #-}
module Bead.Configuration (
    InitTask(..)
  , Config(..)
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

import System.FilePath (joinPath)
import System.Directory (doesFileExist)

import Bead.Domain.Types (readMaybe)

#ifdef TEST
import Bead.Invariants
#endif

-- Represents initalizer tasks to do before launch the service
data InitTask = CreateAdmin
  deriving (Show, Eq)

-- * Configuration

-- Represents the hostname (and/or port) of the bead server
type Hostname = String
type Second   = Int

-- Represents the system parameters stored in a
-- configuration file
data Config = Config {
    -- Place of log messages coming from the UserStory layer
    -- Entries about the actions performed by the user
    userActionLogFile :: FilePath
    -- Session time out on the client side, the lifetime of a valid
    -- value stored in cookies. Measured in seconds, nonnegative value
  , sessionTimeout :: Second
    -- The hostname of the server, this hostname is placed in the registration emails
  , emailHostname :: Hostname
    -- The value for from field for every email sent by the system
  , emailFromAddress :: String
    -- The default language of the login page if there is no language set in the session
  , defaultLoginLanguage :: String
    -- The default regular expression for the user registration
  , usernameRegExp :: String
    -- The example that satisfies the given regexp for the username. These are
    -- rendered to the user as examples on the GUI.
  , usernameRegExpExample :: String
  } deriving (Eq, Show, Read)

-- The defualt system parameters
defaultConfiguration = Config {
    userActionLogFile = joinPath ["log", "useractions.log"]
  , sessionTimeout    = 1200
  , emailHostname     = "http://127.0.0.1:8000"
  , emailFromAddress  = "noreply@bead.org"
  , defaultLoginLanguage = "en"
  , usernameRegExp = "^[A-Za-z0-9]{6}$"
  , usernameRegExpExample = "QUER42"
  }

configCata f (Config useraction timeout host from loginlang regexp reexample) =
  f useraction timeout host from loginlang regexp reexample

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  exist <- doesFileExist path
  case exist of
    False -> do
      putStrLn "Configuration file does not exist"
      return defaultConfiguration
    True  -> do
      content <- readFile path
      case readMaybe content of
        Nothing -> do
          putStrLn "Configuration is not parseable"
          return defaultConfiguration
        Just c -> return c

-- Represents a template for the usage message
newtype Usage = Usage (String -> String)

instance Show Usage where
  show _ = "Usage (...)"

instance Eq Usage where
  _ == _ = False

usageFold :: ((String -> String) -> a) -> Usage -> a
usageFold g (Usage f) = g f

-- Produces an usage string, substituting the given progname into the template
substProgName :: String -> Usage -> String
substProgName name = usageFold ($ name)

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
