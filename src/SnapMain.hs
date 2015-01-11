{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Char (toUpper)
import qualified Data.Char as Char

import           Snap hiding (Config(..))
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hFlush, hSetEcho, stdout, stdin)
import           System.IO.Temp (createTempDirectory)
import           Text.Regex.TDFA

import           Bead.Configuration
import qualified Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as S
import           Bead.Daemon.Email
import           Bead.Daemon.LDAP
import           Bead.Daemon.Logout
import           Bead.Daemon.TestAgent
import           Bead.Domain.Entities (UserRegInfo(..))
import           Bead.Domain.TimeZone (utcZoneInfo)
import           Bead.Persistence.Initialization
import qualified Bead.Persistence.Persist as Persist (Config, defaultConfig, createPersistInit, createPersistInterpreter)
import           Bead.View.BeadContextInit
import           Bead.View.Logger
import           Bead.View.Validators hiding (toLower)


-- Creates a service context that includes the given logger
createContext :: L.Logger -> Persist.Config -> IO ServiceContext
createContext logger cfg = do
  userContainer <- ioUserContainer
  init <- Persist.createPersistInit cfg
  isPersistSetUp <- isSetUp init
  case isPersistSetUp of
    True -> return ()
    False -> initPersist init
  interpreter <- Persist.createPersistInterpreter cfg
  S.serviceContext userContainer logger interpreter

-- Reads the command line arguments, interprets the init tasks and start
-- the service with the given config
main :: IO ()
main = do
  hSetEcho stdin True
  args <- getArgs
  config <- readConfiguration beadConfigFileName
  printConfigInfo config
  checkConfig config
  newAdminUser <- either (const $ return Nothing) (interpretTasks config) (initTasks args)
  startService config newAdminUser

-- Prints out the actual server configuration
printConfigInfo :: Config -> IO ()
printConfigInfo = configCata loginCfg $ \logfile timeout hostname fromEmail loginlang zoneInfoDir up lcfg -> do
  configLn $ "Log file: " ++ logfile
  configLn $ concat ["Session timeout: ", show timeout, " seconds"]
  configLn $ "Hostname included in emails: " ++ hostname
  configLn $ "FROM Address included in emails: " ++ fromEmail
  configLn $ "Default login language: " ++ loginlang
  configLn $ "TimeZone informational dir: " ++ zoneInfoDir
  configLn $ concat ["Maximum size of a file to upload: ", show up, "K"]
  lcfg
  where
    configLn s = putStrLn ("CONFIG: " ++ s)
    loginCfg =
#ifdef LDAPEnabled
      ldapLoginConfig $ \file tz -> do
        configLn $ "Non LDAP Users config file: " ++ show file
        configLn $ "Default registration timezone: " ++ show tz
#else
      standaloneLoginConfig $ \regexp example -> do
        configLn $ "Username regular expression for the registration: " ++ regexp
        configLn $ "Username example for the regular expression: " ++ example
#endif

-- Check if the configuration is valid
checkConfig :: Config -> IO ()
checkConfig cfg = do
  check (maxUploadSizeInKb cfg > 0)
    "The maximum upload size must be non-negative!"

#ifdef LDAPEnabled
#else
  -- Check the given username example against the given username regexp, if the
  -- example does not match with the regepx quit with an exit failure.
  let loginCfg = loginConfig cfg
  check (usernameRegExpExample loginCfg =~ usernameRegExp loginCfg)
    "Given username example does not match with the given pattern!"
#endif

  checkIO (doesDirectoryExist (timeZoneInfoDirectory cfg))
    "The given time-zone info directory"

#ifdef LDAPEnabled
  let loginCfg = loginConfig cfg
  maybe
    (return ())
    (\fp -> checkIO (doesFileExist fp)
               "The given non LDAP Users configuration file")
    (nonLDAPUsersFile loginCfg)
#endif

  configCheck "Config is OK."
  where
    check pred msg = when (not pred) $ do
      configCheck $ "ERROR: " ++ msg
      configCheck $ "There can be more errors. The check fails at the first."
      exitFailure

    checkIO pred msg = do
      p <- pred
      check p msg

    configCheck s = putStrLn $ "CONFIG CHECK: " ++ s

interpretTasks :: Config -> [InitTask] -> IO InitTasks
interpretTasks cfg tasks = case elem CreateAdmin tasks of
  False -> return Nothing
  True  -> fmap Just (readAdminUser cfg)

-- Read user information from stdin, validates the username, the passwords and email fields
readAdminUser :: Config -> IO UserRegInfo
readAdminUser cfg = do
  putStrLn "Creating admin user, all characters are converted to lower case."
  putStrLn "Username must be in the defined format, which is given in the config."
  usr <- readUsername
  email <- readEmail
  fullName <- readFullname
  pwd      <- readPassword "Password: "
  pwdAgain <- readPassword "Password Again: "
  hSetEcho stdin True
  case pwd == pwdAgain of
    -- All the validators are passed, the registration can be done
    True  -> return (UserRegInfo (usr, pwd, email, fullName, utcZoneInfo))
    False -> do
      putStrLn "Passwords do not match!"
      readAdminUser cfg
  where
    putStrFlush msg = putStr msg >> hFlush stdout

    readFullname = do
      putStrFlush "Full name: "
      getLine

    readPassword msg = do
      putStrFlush msg
      hSetEcho stdin False
      pwd <- getLine
      putStrLn ""
      hSetEcho stdin True
      validate
        isPassword
        pwd
        (return pwd)
        (\msg' -> do putStrLn msg'
                     readPassword msg)

    readEmail = do
      putStrFlush "Email address: "
      email <- getLine
      validate
        isEmailAddress
        email
        (return email) -- Valid email
        (\msg -> do putStrLn msg
                    readEmail)

    readUsername = do
      putStrFlush "Admin User: "
      usr <- fmap (map toUpper) getLine
      validate
        isUsername
        usr
        -- Valid username
        (if (isValidUsername usr)
           then return usr
           else do putStrLn "Username does not match the given regexp!"
                   readUsername)
        -- Invalid username
        (\msg -> do putStrLn msg
                    readUsername)
      where
        isValidUsername usr =
#ifdef LDAPEnabled
          and [length usr > 0, all Char.isAlphaNum usr]
#else
          usr =~ (usernameRegExp $ loginConfig cfg)
#endif

startService :: Config -> InitTasks -> IO ()
startService config initTasks = do
  userActionLogs <- creating "logger" $ createSnapLogger . userActionLogFile $ config
  let userActionLogger = snapLogger userActionLogs

  context <- creating "service context" $ createContext userActionLogger Persist.defaultConfig

  tempDir <- creating "temporary directory" createBeadTempDir

  creating "test comments agent" $ startTestCommentsAgent userActionLogger 30 5 {-s-} context

  logoutDaemon <- creating "logout daemon" $
    startLogoutDaemon userActionLogger (sessionTimeout config) 30 {-s-} (userContainer context)

  emailDaemon <- creating "email daemon" $
    startEmailDaemon userActionLogger

  ldapDaemon <- creating "ldap daemon" $
    startLDAPDaemon userActionLogger (LDAPDaemonConfig { tempDir="/tmp/", timeOut=5, noOfWorkers=4 })

  let daemons = Daemons logoutDaemon emailDaemon ldapDaemon

  serveSnaplet defaultConfig (beadContextInit config initTasks context daemons tempDir)
  stopLogger userActionLogs
  removeDirectoryRecursive tempDir
  where
    creating name m = do
      putStr $ concat ["Creating ", name, " ... "]
      x <- m
      putStrLn "DONE"
      return $! x

-- Creates a temporary directory for the bead in the system's temp dir
createBeadTempDir :: IO FilePath
createBeadTempDir = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp "bead."
