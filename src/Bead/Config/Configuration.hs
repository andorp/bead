{-# LANGUAGE CPP #-}
module Bead.Config.Configuration (
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
#ifdef EmailEnabled
    -- The hostname of the server, this hostname is placed in the registration emails
  , emailHostname :: Hostname
    -- The value for from field for every email sent by the system
  , emailFromAddress :: String
#endif
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
#ifdef LDAPEnabled
  , loginConfig :: LDAPLoginConfig
#else
  , loginConfig :: StandaloneLoginConfig
#endif
#ifdef MYSQL
  , persistConfig :: MySQLConfig
#else
  , persistConfig :: FilePersistConfig
#endif
  } deriving (Eq, Show, Read)

#ifdef EmailEnabled
configCata fcfg f (Config useraction timeout host from dll dtz tz up cfg pcfg) =
  f useraction timeout host from dll dtz tz up (fcfg cfg) pcfg
#else
configCata fcfg f (Config useraction timeout dll dtz tz up cfg pcfg) =
  f useraction timeout dll dtz tz up (fcfg cfg) pcfg
#endif

#ifdef MYSQL
data MySQLConfig = MySQLConfig {
    mySQLDbName :: String
  , mySQLHost   :: String
  , mySQLPort   :: Int
  , mySQLUser   :: String
  , mySQLPass   :: String
  } deriving (Eq, Read, Show)
#else
data FilePersistConfig = FilePersistConfig
  deriving (Eq, Read, Show)
#endif

#ifdef LDAPEnabled
-- Login configuration that is used in LDAP registration and login mode
data LDAPLoginConfig = LDAPLoginConfig {
    -- File which contains a non ldap authenticated users, if there is no need to this file
    -- Nothing us used
    nonLDAPUsersFile :: Maybe FilePath
    -- The temporary directory for the ldap tickets
  , ticketTemporaryDir :: FilePath
    -- LDAP Timeout in seconds
  , ldapTimeout :: Int
    -- The number of threads for LDAP login
  , noOfLDAPThreads :: Int
    -- LDAP Key for the UserID
  , userIdKey :: String
    -- LDAP Key for the user's full name
  , userNameKey :: String
    -- LDAP Key for the user's email address
  , userEmailKey :: String
  } deriving (Eq, Show, Read)

ldapLoginConfig f (LDAPLoginConfig file tmpdir timeout threads uik unk uek)
  = f file tmpdir timeout threads uik unk uek
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
#ifdef EmailEnabled
  , emailHostname     = "http://127.0.0.1:8000"
  , emailFromAddress  = "noreply@bead.org"
#endif
  , defaultLoginLanguage = "en"
  , defaultRegistrationTimezone = "UTC"
  , timeZoneInfoDirectory = "/usr/share/zoneinfo"
  , maxUploadSizeInKb = 128
  , loginConfig = defaultLoginConfig
  , persistConfig = defaultPersistConfig
  }

defaultLoginConfig =
#ifdef LDAPEnabled
  LDAPLoginConfig {
      nonLDAPUsersFile = Nothing
    , ticketTemporaryDir = "/tmp/"
    , ldapTimeout = 5
    , noOfLDAPThreads = 4
    , userIdKey = "uid"
    , userNameKey = "name"
    , userEmailKey = "email"
    }
#else
  StandaloneLoginConfig {
      usernameRegExp = "^[A-Za-z0-9]{6}$"
    , usernameRegExpExample = "QUER42"
    }
#endif

#ifdef MYSQL
defaultPersistConfig = MySQLConfig {
    mySQLDbName = "bead"
  , mySQLHost   = "localhost"
  , mySQLPort   = 3306
  , mySQLUser   = "root"
  , mySQLPass   = "password"
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
