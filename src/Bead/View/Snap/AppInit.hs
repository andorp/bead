{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.AppInit where

import Snap
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Fay
import Data.Maybe (maybe)
import System.FilePath (joinPath)

import Bead.View.Snap.TemplateAndComponentNames
import Bead.Controller.ServiceContext as S

import Bead.View.Snap.Application as A
import Bead.View.Snap.Dictionary (Dictionaries)
import Bead.View.Snap.PageHandlers (routes)
import Bead.View.Snap.Registration (createAdminUser)

appInit :: Maybe (String, String) -> ServiceContext -> Dictionaries -> SnapletInit App App
appInit user s d = makeSnaplet "bead" description Nothing $ do

  case user of
    Nothing        -> return ()
    Just (usr,pwd) -> liftIO $ S.scRunPersist s $ \p -> createAdminUser p "users.json" usr pwd

  sm <- nestSnaplet "session" sessionManager $ initCookieSessionManager
          (joinPath ["config","site_cookie"]) "session" (Just 3600)
  as <- nestSnaplet "auth" auth $
          initJsonFileAuthManager defAuthSettings sessionManager "users.json"
  ss <- nestSnaplet "context" A.serviceContext $ contextSnaplet s

  ds <- nestSnaplet "dictionary" dictionaryContext $ dictionarySnaplet d

  fs <- nestSnaplet "fay" fayContext $ initFay

  addRoutes routes

  return $ App sm as ss ds fs
  where
    description = "The BEAD website"
