{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bead.Controller.ServiceContext as S
import Bead.View.Snap.Application
import Bead.View.Snap.AppInit
import Bead.View.Snap.PageHandlers
import Bead.Persistence.Persist (initPersistence, isPersistenceSetUp)
import qualified Bead.Controller.Logging as L
import qualified Bead.Persistence.NoSQLDir as P

import Snap
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Control.Monad.Trans
import Control.Lens.TH


import qualified Data.IORef as Ref

c :: IO ServiceContext
c = do
  userContainer <- ioUserContainer
  isPersistSetUp <- isPersistenceSetUp P.noSqlDirPersist
  case isPersistSetUp of
    True -> return ()
    False -> initPersistence P.noSqlDirPersist
  return $ S.serviceContext P.noSqlDirPersist userContainer L.mockLogger

main :: IO ()
main = do
  context <- c
  serveSnaplet defaultConfig (appInit context)
