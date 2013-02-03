{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.Application where

import Snap
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Control.Lens.TH
import Data.IORef
import System.FilePath (joinPath)

import qualified Data.Text as DT
import qualified Text.XmlHtml as X

import Bead.View.Snap.TemplateAndComponentNames
import Bead.Controller.ServiceContext

-- * Mini snaplet : Service context

newtype SnapletServiceContext = SnapletServiceContext (IORef ServiceContext)

contextSnaplet :: ServiceContext -> SnapletInit b SnapletServiceContext
contextSnaplet s = makeSnaplet
  "ServiceContext"
  "A snaplet providing the service context of the user stories"
  Nothing $
  liftIO $ do
    ref <- newIORef s
    return $! SnapletServiceContext ref

getServiceContextRef :: Handler b SnapletServiceContext (IORef ServiceContext)
getServiceContextRef = do
  (SnapletServiceContext r) <- get
  return r

getServiceContext :: Handler b SnapletServiceContext ServiceContext
getServiceContext = do
  SnapletServiceContext ref <- get
  context <- liftIO $ readIORef ref
  return context

-- * Application

data App = App {
    _sessionManager :: Snaplet SessionManager
  , _auth           :: Snaplet (AuthManager App)
  , _serviceContext :: Snaplet SnapletServiceContext
  }

makeLenses ''App

