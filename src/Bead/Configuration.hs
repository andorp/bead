{-# LANGUAGE CPP #-}
module Bead.Configuration (
    InitTask(..)
  , Config(..)
  , defaultConfiguration
  , configFold
  , initTasks
  , Usage
  , substProgName
#ifdef TEST
  , initTaskAssertions
#endif
  ) where

import Control.Monad (join)

import System.Console.GetOpt
import System.FilePath (FilePath, joinPath)

#ifdef TEST
import Bead.Invariants
#endif

-- Represents initalizer tasks to do before launch the service
data InitTask = CreateAdmin
  deriving (Show, Eq)

-- * Configuration

-- Represents the system parameters stored in a
-- configuration file
data Config = Config {
    -- Place of log messages coming from the UserStory layer
    -- Entries about the actions performed by the user
    userActionLogFile :: FilePath
    -- Session time out on the client side, the lifetime of a valid
    -- value stored in cookies. Measured in seconds, nonnegative value
  , sessionTimeout :: Int
  } deriving (Eq, Show, Read)

-- The defualt system parameters
defaultConfiguration = Config {
    userActionLogFile = joinPath ["log", "useractions.log"]
  , sessionTimeout    = 1200
  }

configFold :: (FilePath -> Int -> a) -> Config -> a
configFold f (Config l s) = f l s

-- Represents a template for the usage message
newtype Usage = Usage (String -> String)

instance Show Usage where
  show _ = "Usage (...)"

instance Eq Usage where
  a == b = False

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
