{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.AppInit (
    appInit
  , beadConfigFileName
  , AppInitTasks
  , Daemons(..)
  ) where

import qualified Data.Map as Map

import           Snap hiding (Config(..))
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SafeJsonFile
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Session.Backends.CookieSession
import           System.FilePath ((</>))
import           System.Directory

import           Bead.Configuration (Config(..))
import           Bead.Controller.ServiceContext as S
import           Bead.Daemon.Email
import           Bead.Daemon.Logout
import           Bead.Domain.Entities (UserRegInfo)

import           Bead.Domain.TimeZone
import           Bead.View.Snap.Application as A
import           Bead.View.Snap.DataDir
import           Bead.View.Snap.Dictionary (Language(..))
import           Bead.View.Snap.DictionaryLoader (loadDictionaries)
import           Bead.View.Snap.Registration (createAdminUser)
import           Bead.View.Snap.Routing


beadConfigFileName :: String
beadConfigFileName = "bead.config"

iconFileName :: String
iconFileName = "icon.ico"

usersJson :: String
usersJson = "users.json"

-- During the initialization what other tasks need to be done.
-- Just userRegInfo means that a new admin user should be craeted
-- Nothing means there is no additional init task to be done.
type AppInitTasks = Maybe UserRegInfo

-- The collection of the daemons that are neccesary to create the
-- application
data Daemons = Daemons {
    logoutDaemon :: LogoutDaemon
  , emailDaemon  :: EmailDaemon
  }

appInit :: Config -> Maybe UserRegInfo -> ServiceContext -> Daemons -> FilePath -> SnapletInit App App
appInit config user s daemons tempDir = makeSnaplet "bead" description dataDir $ do

  copyDataContext

  let dictionaryDir = "lang"
  dExist <- liftIO $ doesDirectoryExist dictionaryDir
  dictionaries <- case dExist of
    True -> liftIO $ loadDictionaries dictionaryDir
    False -> return Map.empty

  -- TODO: Use a start logger
  liftIO $ putStrLn $ "Found dictionaries: " ++ (show $ Map.keys dictionaries)

  case user of
    Nothing -> return ()
    Just userRegInfo ->
      liftIO $ createAdminUser (persistInterpreter s) usersJson userRegInfo

  sm <- nestSnaplet "session" sessionManager $
          initCookieSessionManager "cookie" "session" (Just (sessionTimeout config))

  as <- nestSnaplet "auth" auth $
          initSafeJsonFileAuthManager defAuthSettings sessionManager usersJson

  ss <- nestSnaplet "context" A.serviceContext $ contextSnaplet s (logoutDaemon daemons)

  ds <- nestSnaplet "dictionary" dictionaryContext $
          dictionarySnaplet dictionaries (Language $ defaultLoginLanguage config)

  se <- nestSnaplet "sendemail" sendEmailContext (emailSenderSnaplet config (emailDaemon daemons))

  rp <- nestSnaplet "randompassword" randomPasswordContext passwordGeneratorSnaplet

  fs <- nestSnaplet "fay" fayContext $ initFay

  ts <- nestSnaplet "tempdir" tempDirContext $ tempDirectorySnaplet tempDir

  cs <- nestSnaplet "config" configContext $ configurationServiceContext config

  un <- nestSnaplet "usernamechecker" checkUsernameContext $ regexpUsernameChecker config

  timeZoneConverter <- liftIO $ createTimeZoneConverter (timeZoneInfoDirectory config)
  tz <- nestSnaplet "timezoneconveter" timeZoneContext $ createTimeZoneContext timeZoneConverter

  addRoutes (routes config)
  wrapSite (<|> pages)

  return $ App sm as ss ds se rp fs ts cs un tz
  where
    description = "The BEAD website"

    dataDir = Just referenceDataDir

-- | Creating data context in the current directory,
--   copying reference files.
copyDataContext :: Initializer b v ()
copyDataContext = do
  reference <- liftIO referenceDataDir
  dataDir   <- getSnapletFilePath
  let skips = [beadConfigFileName, iconFileName]
  liftIO $ copyFiles skips reference dataDir
  return ()

-- | Copy and update files if missing or outdated, skip the ones
-- from the outdate check that names are the same as in the skipped list
copyFiles :: [FilePath] -> FilePath -> FilePath -> IO ()
copyFiles skips src dst = do
  dstExist <- doesDirectoryExist dst
  unless dstExist $ createDirectory dst
  contents <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) contents
  forM_ xs $ \name -> do
    let srcPath = src </> name
        dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyFiles skips srcPath dstPath
      else do
        dstFileExist <- doesFileExist dstPath
        when (not dstFileExist || name `notElem` skips) $ do
          doCopy <- if dstFileExist
            then do
              srcDate <- getModificationTime srcPath
              dstDate <- getModificationTime dstPath
              return $ dstDate < srcDate
            else return True
          when doCopy $ copyFile srcPath dstPath
