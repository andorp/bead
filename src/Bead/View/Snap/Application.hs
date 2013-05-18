{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.Application where

import Snap
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Fay
import Control.Lens.TH
import Data.IORef
import qualified Data.Map as Map

import qualified Data.Text as DT
import qualified Text.XmlHtml as X

import Bead.View.Snap.Dictionary
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

getServiceContext :: Handler b SnapletServiceContext ServiceContext
getServiceContext = do
  SnapletServiceContext ref <- get
  liftIO . readIORef $ ref

-- * Mini snaplet : Dictionary snaplet

newtype DictionaryContext = DictionaryContext (IORef Dictionaries)

dictionarySnaplet :: Dictionaries -> SnapletInit a DictionaryContext
dictionarySnaplet d = makeSnaplet
  "Dictionaries"
  "A snaplet providing the i18 dictionary context"
  Nothing $ liftIO $ do
    ref <- newIORef d
    return $! DictionaryContext ref

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> Handler b DictionaryContext (Maybe Dictionary)
getDictionary l = do
  DictionaryContext ref <- get
  m <- liftIO . readIORef $ ref
  return $ Map.lookup l m

-- * Application

data App = App {
    _sessionManager :: Snaplet SessionManager
  , _auth           :: Snaplet (AuthManager App)
  , _serviceContext :: Snaplet SnapletServiceContext
  , _dictionaryContext :: Snaplet DictionaryContext
  , _fayContext     :: Snaplet Fay
  }

makeLenses ''App

