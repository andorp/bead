{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Concurrent
import           Data.Char (toUpper)

import           Snap hiding (Config(..))
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.Environment (getArgs)
import           System.IO (hFlush, hSetEcho, stdout, stdin)
import           System.IO.Temp (createTempDirectory)

import           Bead.Configuration
import qualified Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as S
import           Bead.Controller.UserStories (runUserStory, testAgentComments)
import           Bead.Daemon.EmailDaemon
import           Bead.Daemon.LogoutDaemon
import           Bead.Domain.Entities (UserRegInfo(..), TimeZone(..))
import           Bead.Persistence.Persist (initPersistence, isPersistenceSetUp)
import           Bead.View.Snap.AppInit
import           Bead.View.Snap.Logger
import           Bead.View.Snap.Validators hiding (toLower)
import           Bead.View.Snap.Translation (trans)


-- Creates a service context that includes the given logger
createContext :: L.Logger -> IO ServiceContext
createContext logger = do
  userContainer <- ioUserContainer
  isPersistSetUp <- isPersistenceSetUp
  case isPersistSetUp of
    True -> return ()
    False -> initPersistence
  S.serviceContext userContainer logger

-- Reads the command line arguments, interprets the init tasks and start
-- the service with the given config
main :: IO ()
main = do
  hSetEcho stdin True
  args <- getArgs
  config <- readConfiguration beadConfigFileName
  newAdminUser <- either (const $ return Nothing) interpretTasks (initTasks args)
  printConfigInfo config
  startService config newAdminUser

-- Prints out the actual server configuration
printConfigInfo :: Config -> IO ()
printConfigInfo cfg = do
  putStrLn $ "Log file: " ++ userActionLogFile cfg
  putStrLn $ concat ["Session timeout: ", show $ sessionTimeout cfg, " seconds"]
  putStrLn $ "Hostname included in emails: " ++ emailHostname cfg
  putStrLn $ "FROM Address included in emails: " ++ emailFromAddress cfg
  putStrLn $ "Default login language: " ++ defaultLoginLanguage cfg
  putStrLn $ "Username regular expression for the registration: " ++ usernameRegExp cfg

interpretTasks :: [InitTask] -> IO AppInitTasks
interpretTasks tasks = case elem CreateAdmin tasks of
  False -> return Nothing
  True  -> fmap Just readAdminUser

-- Read user information from stdin, validates the username, the passwords and email fields
readAdminUser :: IO UserRegInfo
readAdminUser = do
  putStrLn "Creating admin user, all characters are converted to lower case."
  putStrLn "Username must be in the neptun code format."
  usr <- readUsername
  email <- readEmail
  fullName <- readFullname
  pwd      <- readPassword "Password: "
  pwdAgain <- readPassword "Password Again: "
  hSetEcho stdin True
  case pwd == pwdAgain of
    -- All the validators are passed, the registration can be done
    True  -> return (UserRegInfo (usr, pwd, email, fullName, UTC))
    False -> do
      putStrLn "Passwords do not match!"
      readAdminUser
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
        (return usr)
        -- Invalid username
        (\msg -> do putStrLn msg
                    readUsername)

startService :: Config -> AppInitTasks -> IO ()
startService config appInitTasks = do
  userActionLogs <- creating "logger" $ createSnapLogger . userActionLogFile $ config
  let userActionLogger = snapLogger userActionLogs

  context <- creating "service context" $ createContext userActionLogger

  tempDir <- creating "temporary directory" createBeadTempDir

  creating "test comments agent" $ startTestCommentsAgent userActionLogger 30 5 {-s-} context

  logoutDaemon <- creating "logout daemon" $
    startLogoutDaemon userActionLogger (sessionTimeout config) 30 {-s-} (userContainer context)

  emailDaemon <- creating "email daemon" $
    startEmailDaemon userActionLogger

  let daemons = Daemons logoutDaemon emailDaemon

  serveSnaplet defaultConfig (appInit config appInitTasks context daemons tempDir)
  stopLogger userActionLogs
  removeDirectoryRecursive tempDir
  where
    creating name m = do
      putStr $ concat ["Creating ", name, " ... "]
      x <- m
      putStrLn "DONE"
      return x

-- Creates a temporary directory for the bead in the system's temp dir
createBeadTempDir :: IO FilePath
createBeadTempDir = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp "bead."

-- Starts a thread that polls the persistence layer for new test agent comments in every w seconds
-- and places them into the right place.
startTestCommentsAgent :: L.Logger -> Int -> Int -> ServiceContext -> IO ()
startTestCommentsAgent logger initWait wait context = do
  let agent = do threadDelay (secToMicroSec wait)
                 ((runUserStory context trans TestAgent testAgentComments) >> return ()) `catch` someException
                 agent
  forkIO $ do
    threadDelay (secToMicroSec initWait)
    agent
  return ()
  where
    secToMicroSec = (* 1000000)

    someException :: SomeException -> IO ()
    someException e = do
      (L.log logger L.ERROR (show e)) `catch` (loggerException e)
      return ()
      where
        loggerException :: SomeException -> SomeException -> IO ()
        loggerException original e = do
          print original
          print e
