{-# LANGUAGE CPP #-}
module Bead.Configuration (
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
    -- The directory where all the timezone informations can be found
    -- Eg: /usr/share/zoneinfo/
  , timeZoneInfoDirectory :: FilePath
    -- The maximum upload size of a file given in Kbs
  , maxUploadSizeInKb :: Int
#ifdef LDAPEnabled
  , loginConfig :: LDAPLoginConfig
#else
  , loginConfig :: StandaloneLoginConfig
#endif
  } deriving (Eq, Show, Read)

configCata fcfg f (Config useraction timeout host from loginlang tz up cfg) =
  f useraction timeout host from loginlang tz up (fcfg cfg)

-- Login configuration that is used in standalone registration and login mode
data StandaloneLoginConfig = StandaloneLoginConfig {
    -- The default regular expression for the user registration
    usernameRegExp :: String
    -- The example that satisfies the given regexp for the username. These are
    -- rendered to the user as examples on the GUI.
  , usernameRegExpExample :: String
  } deriving (Eq, Show, Read)

standaloneLoginConfig f (StandaloneLoginConfig reg exp) = f reg exp

-- Login configuration that is used in LDAP registration and login mode
data LDAPLoginConfig = LDAPLoginConfig {
    -- File which contains a non ldap authenticated users, if there is no need to this file
    -- Nothing us used
    nonLDAPUsersFile :: Maybe FilePath
    -- The default timezone for a newly registered LDAP user
  , defaultRegistrationTimezone :: String
  } deriving (Eq, Show, Read)

ldapLoginConfig f (LDAPLoginConfig file tz) = f file tz

-- The defualt system parameters
defaultConfiguration = Config {
    userActionLogFile = joinPath ["log", "useractions.log"]
  , sessionTimeout    = 1200
  , emailHostname     = "http://127.0.0.1:8000"
  , emailFromAddress  = "noreply@bead.org"
  , defaultLoginLanguage = "en"
  , timeZoneInfoDirectory = "/usr/share/zoneinfo"
  , maxUploadSizeInKb = 128
  , loginConfig = defaultLoginConfig
  }

defaultLoginConfig =
#ifdef LDAPEnabled
  LDAPLoginConfig {
      nonLDAPUsersFile = Nothing
    , defaultRegistrationTimezone = "UTC"
    }
#else
  StandaloneLoginConfig {
      usernameRegExp = "^[A-Za-z0-9]{6}$"
    , usernameRegExpExample = "QUER42"
    }
#endif


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
      case readMaybe content of
        Nothing -> do
          putStrLn "Configuration is not parseable"
          putStrLn "!!! DEFAULT CONFIGURATION IS USED !!!"
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
