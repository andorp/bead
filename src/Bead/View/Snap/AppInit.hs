{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.AppInit (appInit) where

import Snap hiding (Config(..))
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Fay
import Data.Maybe (maybe)
import System.FilePath (joinPath)

import Bead.Configuration (Config(..))
import Bead.View.Snap.TemplateAndComponentNames
import Bead.Controller.ServiceContext as S

import Bead.View.Snap.Application as A
import Bead.View.Snap.Dictionary (Dictionaries)
import Bead.View.Snap.PageHandlers (routes)
import Bead.View.Snap.Registration (createAdminUser)
import Bead.View.Snap.ErrorPage (msgErrorPage)
import Bead.View.Snap.DataDir

import System.FilePath ((</>))
import System.Directory


appInit :: Config -> Maybe (String, String) -> ServiceContext -> Dictionaries -> SnapletInit App App
appInit config user s d = makeSnaplet "bead" description dataDir $ do

  copyDataContext

  case user of
    Nothing        -> return ()
    Just (usr,pwd) -> liftIO $ S.scRunPersist s $ \p -> createAdminUser p "users.json" usr pwd

  sm <- nestSnaplet "session" sessionManager $
          initCookieSessionManager "cookie" "session" (Just (sessionTimeout config))

  as <- nestSnaplet "auth" auth $
          initJsonFileAuthManager defAuthSettings sessionManager "users.json"

  ss <- nestSnaplet "context" A.serviceContext $ contextSnaplet s

  ds <- nestSnaplet "dictionary" dictionaryContext $ dictionarySnaplet d

  se <- nestSnaplet "sendemail" sendEmailContext emailSenderSnaplet

  rp <- nestSnaplet "randompassword" randomPasswordContext passwordGeneratorSnaplet

  fs <- nestSnaplet "fay" fayContext $ initFay

  addRoutes (routes config)
  wrapSite (<|> msgErrorPage "Invalid address")

  return $ App sm as ss ds se rp fs
  where
    description = "The BEAD website"

    dataDir = Just referenceDataDir

-- | Creating data context in the current directory,
--   copying reference files.
copyDataContext :: Initializer b v ()
copyDataContext = do
  reference <- liftIO referenceDataDir
  dataDir   <- getSnapletFilePath
  liftIO $ copyFiles reference dataDir
  return ()

-- | Copy files if missing or outdated
copyFiles :: FilePath -> FilePath -> IO ()
copyFiles src dst = do
  dstExist <- doesDirectoryExist dst
  unless dstExist $ createDirectory dst
  contents <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) contents
  forM_ xs $ \name -> do
    let srcPath = src </> name
        dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyFiles srcPath dstPath
      else do
        dstFileExist <- doesFileExist dstPath
        doCopy <- if dstFileExist then do
                       srcDate <- getModificationTime srcPath
                       dstDate <- getModificationTime dstPath
                       return $ dstDate < srcDate
                     else return True
        when doCopy $ copyFile srcPath dstPath
