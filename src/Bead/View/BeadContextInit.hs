{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Bead.View.BeadContextInit (
    beadContextInit
  , beadConfigFileName
  , InitTasks
  , Daemons(..)
  , usersJson
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Snap hiding (Config(..))
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SafeJsonFile
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Session.Backends.CookieSession
import           System.FilePath ((</>))
import           System.Directory

import           Bead.Config
import           Bead.Controller.ServiceContext as S hiding (serviceContext)
#ifdef EmailEnabled
import           Bead.Daemon.Email
#endif
#ifdef LDAPEnabled
import           Bead.Daemon.LDAP
#endif
import           Bead.Daemon.Logout
import           Bead.Domain.Entities (UserRegInfo, Username(..))

import           Bead.Domain.TimeZone
import           Bead.View.BeadContext hiding (ldapDaemon)
import           Bead.View.DataDir
import           Bead.View.Dictionary (Language(..))
import           Bead.View.DictionaryLoader (loadDictionaries)
import           Bead.View.Registration (createAdminUser)
import           Bead.View.Routing


beadConfigFileName :: String
beadConfigFileName = "bead.config"

iconFileName :: String
iconFileName = "icon.ico"

usersJson :: String
usersJson = "users.json"

-- During the initialization what other tasks need to be done.
-- Just userRegInfo means that a new admin user should be craeted
-- Nothing means there is no additional init task to be done.
type InitTasks = Maybe UserRegInfo

-- The collection of the daemons that are neccesary to create the
-- application
data Daemons = Daemons {
    logoutDaemon :: LogoutDaemon
#ifdef EmailEnabled
  , emailDaemon  :: EmailDaemon
#endif
#ifdef LDAPEnabled
  , ldapDaemon   :: LDAPDaemon
#endif
  }

beadContextInit :: Config -> ServiceContext -> Daemons -> FilePath -> SnapletInit BeadContext BeadContext
beadContextInit config s daemons tempDir = makeSnaplet "bead" description dataDir $ do
  copyDataContext

  sm <- nestSnaplet "session" sessionManager $
          initCookieSessionManager "cookie" "session" (Just (sessionTimeout config))

  as <- nestSnaplet "auth" auth $
          initSafeJsonFileAuthManager defAuthSettings sessionManager usersJson

  ss <- nestSnaplet "context" serviceContext $ contextSnaplet s (logoutDaemon daemons)

  let dictionaryDir = "lang"
  dExist <- liftIO $ doesDirectoryExist dictionaryDir
  dictResult <- case dExist of
    True -> liftIO $ loadDictionaries dictionaryDir
    False -> return $ Right $ Map.empty

  liftIO $ putStrLn "Searching for dictionaries..."
  ds <- case dictResult of
    Left err -> error $ "ERROR: Conflicts while processing dictionaries:\n\n" ++ err
    Right dictionaries -> do
      -- TODO: Use a start logger
      liftIO $ putStrLn $ "Found dictionaries: " ++ (show $ Map.keys dictionaries)
      nestSnaplet "dictionary" dictionaryContext $
          dictionarySnaplet dictionaries (Language $ defaultLoginLanguage config)

#ifdef EmailEnabled
  se <- nestSnaplet "sendemail" sendEmailContext (emailSenderSnaplet config (emailDaemon daemons))
#endif

#ifndef LDAPEnabled
  rp <- nestSnaplet "randompassword" randomPasswordContext passwordGeneratorSnaplet
#endif

  fs <- nestSnaplet "fay" fayContext $ initFay

  ts <- nestSnaplet "tempdir" tempDirContext $ tempDirectorySnaplet tempDir

  cs <- nestSnaplet "config" configContext $ configurationServiceContext config

#ifndef LDAPEnabled
  un <- nestSnaplet "usernamechecker" checkUsernameContext $ regexpUsernameChecker config
#endif

  timeZoneConverter <- liftIO $ createTimeZoneConverter (timeZoneInfoDirectory config)

  tz <- nestSnaplet "timezoneconveter" timeZoneContext $ createTimeZoneContext timeZoneConverter

  dl <- nestSnaplet "debuglogger" debugLoggerContext $ createDebugLogger

#ifdef LDAPEnabled
  ldap <- do
    nonLDAPUsers <- liftIO $
      loadNonLDAPUsers (nonLDAPUsersFile $ loginConfig config)
    nestSnaplet "ldap-config" ldapContext $
      createLDAPContext (LDAP nonLDAPUsers $ ldapDaemon daemons)
#endif

  addRoutes (routes config)

  wrapSite (<|> pages)

  return $
#ifdef LDAPEnabled
#ifdef EmailEnabled
    BeadContext sm as ss ds se fs ts cs tz dl ldap
#else
    BeadContext sm as ss ds fs ts cs tz dl ldap
#endif
#else
#ifdef EmailEnabled
    BeadContext sm as ss ds se rp fs ts cs un tz dl
#else
    BeadContext sm as ss ds rp fs ts cs un tz dl
#endif
#endif
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

-- Creates an LDAP config if the users file is given, otherwise
-- creates an empty config.
loadNonLDAPUsers :: Maybe FilePath -> IO (Set.Set Username)
loadNonLDAPUsers Nothing = return $ Set.empty
loadNonLDAPUsers (Just fp) = do
  users <- fmap (Set.fromList . map username . lines) $ readFile fp
  return $! users
  where
    username = Username . map toUpper
