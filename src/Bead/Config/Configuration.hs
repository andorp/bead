{-# LANGUAGE CPP #-}
module Bead.Config.Configuration (
    InitTask(..)
  , Config(..)
#ifdef SSO
  , SSOLoginConfig(..)
  , sSOLoginConfig
#else
  , StandaloneLoginConfig(..)
  , standaloneLoginConfig
#endif
  , defaultConfiguration
  , configCata
  , Usage(..)
  , substProgName
#ifdef MYSQL
  , MySQLConfig(..)
#else
  , FilePersistConfig(..)
#endif
  ) where

import System.FilePath (joinPath)
import System.Directory (doesFileExist)

import Bead.Domain.Types (readMaybe)

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
    -- The default timezone for a newly registered user
  , defaultRegistrationTimezone :: String
    -- The directory where all the timezone informations can be found
    -- Eg: /usr/share/zoneinfo/
  , timeZoneInfoDirectory :: FilePath
    -- The maximum upload size of a file given in Kbs
  , maxUploadSizeInKb :: Int
    -- Simple login configuration
#ifdef SSO
  , loginConfig :: SSOLoginConfig
#else
  , loginConfig :: StandaloneLoginConfig
#endif
#ifdef MYSQL
  , persistConfig :: MySQLConfig
#else
  , persistConfig :: FilePersistConfig
#endif
  } deriving (Eq, Show, Read)

configCata fcfg f (Config useraction timeout host from dll dtz tz up cfg pcfg) =
  f useraction timeout host from dll dtz tz up (fcfg cfg) pcfg

#ifdef MYSQL
data MySQLConfig = MySQLConfig {
    mySQLDbName   :: String
  , mySQLHost     :: String
  , mySQLPort     :: Int
  , mySQLUser     :: String
  , mySQLPass     :: String
  , mySQLPoolSize :: Int
  } deriving (Eq, Read, Show)
#else
data FilePersistConfig = FilePersistConfig
  deriving (Eq, Read, Show)
#endif

#ifdef SSO
-- Login configuration that is used in single sign-on (SSO)
data SSOLoginConfig = SSOLoginConfig {
    -- Query timeout (in seconds)
    sSOTimeout       :: Int
    -- Number of query threads
  , sSOThreads       :: Int
    -- A format string that tells how to query LDAP attributes
    -- on the given system
  , sSOQueryCommand  :: String
    -- Key for UserID in LDAP
  , sSOUserIdKey     :: String
    -- Key for the user's full name in LDAP
  , sSOUserNameKey   :: String
    -- Key for the user's email address in LDAP
  , sSOUserEmailKey  :: String
    -- Enable login through a direct link, without SSO
  , sSODeveloperMode :: Bool
  } deriving (Eq, Show, Read)

sSOLoginConfig f (SSOLoginConfig timeout threads cmd uik unk uek dev)
  = f timeout threads cmd uik unk uek dev
#else
-- Login configuration that is used in standalone registration and login mode
data StandaloneLoginConfig = StandaloneLoginConfig {
    -- The default regular expression for the user registration
    usernameRegExp :: String
    -- The example that satisfies the given regexp for the username. These are
    -- rendered to the user as examples on the GUI.
  , usernameRegExpExample :: String
  } deriving (Eq, Show, Read)

standaloneLoginConfig f (StandaloneLoginConfig reg exp) = f reg exp
#endif

-- The defualt system parameters
defaultConfiguration = Config {
    userActionLogFile = joinPath ["log", "useractions.log"]
  , sessionTimeout    = 1200
  , defaultLoginLanguage = "en"
  , defaultRegistrationTimezone = "UTC"
  , timeZoneInfoDirectory = "/usr/share/zoneinfo"
  , maxUploadSizeInKb = 128
  , loginConfig = defaultLoginConfig
  , persistConfig = defaultPersistConfig
  }

defaultLoginConfig =
#ifdef SSO
  SSOLoginConfig {
      sSOTimeout = 5
    , sSOThreads = 4
    , sSOQueryCommand = "ldapsearch"
    , sSOUserIdKey = "uid"
    , sSOUserNameKey = "name"
    , sSOUserEmailKey = "email"
    , sSODeveloperMode = False
    }
#else
  StandaloneLoginConfig {
      usernameRegExp = "^[A-Za-z0-9]{6}$"
    , usernameRegExpExample = "QUER42"
    }
#endif

#ifdef MYSQL
defaultPersistConfig = MySQLConfig {
    mySQLDbName   = "bead"
  , mySQLHost     = "localhost"
  , mySQLPort     = 3306
  , mySQLUser     = "root"
  , mySQLPass     = "password"
  , mySQLPoolSize = 30
  }
#else
defaultPersistConfig = FilePersistConfig
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
