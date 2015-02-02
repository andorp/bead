{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Char (toUpper)
import qualified Data.Char as Char
import           Data.Maybe

import           Snap hiding (Config(..))
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hFlush, hSetEcho, stdout, stdin)
import           System.IO.Temp (createTempDirectory)
import           Text.Regex.TDFA

import           Bead.Config
import qualified Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as S
#ifdef EmailEnabled
import           Bead.Daemon.Email
#endif
#ifdef LDAPEnabled
import           Bead.Daemon.LDAP
#endif
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
printConfigInfo = configCata loginConfigPart $ \logfile timeout hostname fromEmail dll dtz zoneInfoDir up lcfg -> do
  configLn $ "Log file: " ++ logfile
  configLn $ concat ["Session timeout: ", show timeout, " seconds"]
  configLn $ "Hostname included in emails: " ++ hostname
  configLn $ "FROM Address included in emails: " ++ fromEmail
  configLn $ "Default login language: " ++ dll
  configLn $ "Default time zone: " ++ dtz
  configLn $ "TimeZone informational dir: " ++ zoneInfoDir
  configLn $ concat ["Maximum size of a file to upload: ", show up, "K"]
  lcfg
  where
    configLn s = putStrLn ("CONFIG: " ++ s)
    loginConfigPart =
#ifdef LDAPEnabled
      ldapLoginConfig $ \file tmpdir timeout threads uik unk uek -> do
         configLn $ "Non LDAP Users config file: " ++ show file
         configLn $ "Temporary directory for the LDAP tickets: " ++ show tmpdir
         configLn $ "Timeout for an LDAP login request: " ++ show timeout
         configLn $ "Number of LDAP authenticator threads: " ++ show threads
         configLn $ "LDAP key for the UserID: " ++ show uik
         configLn $ "LDAP key for the User's full name: " ++ show unk
         configLn $ "LDAP key for the User's email: " ++ show uek
#else
      standaloneLoginConfig $ \regexp example -> do
         configLn $ "Username regular expression for the registration: " ++ regexp
         configLn $ "Username example for the regular expression: " ++ example
#endif

-- Check if the configuration is valid
checkConfig :: Config -> IO ()
checkConfig cfg = do
  check (not $ null $ defaultRegistrationTimezone cfg)
    "The default registration time zone is empty"
  check (maxUploadSizeInKb cfg > 0)
    "The maximum upload size must be non-negative!"

  let loginCfgPart =
#ifdef LDAPEnabled
        -- LDAP: Check if there is a given non-ldap users file exist
        ldapLoginConfig $ \file tmpdir timeout threads uik unk uek -> do
          when (isJust file) $ checkIO (doesFileExist $ fromJust file) "The given non LDAP Users configuration file"
          checkIO (doesDirectoryExist tmpdir) "The given LDAP ticket directory does not exist"
          check (timeout > 0) "LDAP timeout is less or equal to zero"
          check (threads > 0) "LDAP thread number is less or equals to zero"
          check (not $ null uik) "LDAP UID key is empty"
          check (not $ null unk) "LDAP User's fullname key is empty"
          check (not $ null uek) "LDAP User's email key is empty"
#else
        -- Standalone: Check the given username example against the given username regexp, if the
        -- example does not match with the regepx quit with an exit failure.
        standaloneLoginConfig $ \usernameRegExp usernameRegExpExample -> do
          check (usernameRegExpExample =~ usernameRegExp)
            "Given username example does not match with the given pattern!"
#endif

  checkIO (doesDirectoryExist (timeZoneInfoDirectory cfg))
    "The given time-zone info directory"

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

#ifdef EmailEnabled
  emailDaemon <- creating "email daemon" $
    startEmailDaemon userActionLogger
#endif

#ifdef LDAPEnabled
  ldapDaemon <- creating "ldap daemon" $
    startLDAPDaemon userActionLogger $ ldapDaemonConfig $ loginConfig config
#endif

#ifdef LDAPEnabled
#ifdef EmailEnabled
  let daemons = Daemons logoutDaemon emailDaemon ldapDaemon
#else
  let daemons = Daemons logoutDaemon ldapDaemon
#endif
#else
#ifdef EmailEnabled
  let daemons = Daemons logoutDaemon emailDaemon
#else
  let daemons = Daemons logoutDaemon
#endif
#endif

  serveSnaplet defaultConfig (beadContextInit config initTasks context daemons tempDir)
  stopLogger userActionLogs
  removeDirectoryRecursive tempDir
  where
    creating name m = do
      putStr $ concat ["Creating ", name, " ... "]
      x <- m
      putStrLn "DONE"
      return $! x

#ifdef LDAPEnabled
    defaultLDAPConfig = LDAPDaemonConfig "" 0 0 "" "" ""

    ldapDaemonConfig =
      ldapLoginConfig $ \_file tmpdir timeout threads uik unk uek ->
        LDAPDaemonConfig {
          tempDir = tmpdir,
          timeOut = timeout,
          noOfWorkers = threads,
          uidKey = uik,
          nameKey = unk,
          emailKey = uek
        }
#endif

-- Creates a temporary directory for the bead in the system's temp dir
createBeadTempDir :: IO FilePath
createBeadTempDir = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp "bead."
