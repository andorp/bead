{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Auth.Backends.SafeJsonFile where

{-
Safe Json File authentication is needed, becuase there is a bug
in the original json authentication module. The flushing out
the cache to the json file is not a threadsafe.

This module only uses MVar to preserve the type safety
-}

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Text
import System.Directory

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session

import Web.ClientSession (getKey)

newtype SafeAuthBackend a = SafeAuthBackend (MVar a)

authBackend :: SafeAuthBackend a -> MVar a
authBackend (SafeAuthBackend m) = m

mkSafeAuthManager :: IAuthBackend a => a -> IO (SafeAuthBackend a)
mkSafeAuthManager b = do
  mvar <- newMVar b
  return $! SafeAuthBackend mvar

instance (IAuthBackend b) => IAuthBackend (SafeAuthBackend b) where
  save s a = withMVar (authBackend s) (\backend -> save backend a)
  lookupByUserId s u = withMVar (authBackend s) (\backend -> lookupByUserId backend u)
  lookupByLogin  s l = withMVar (authBackend s) (\backend -> lookupByLogin backend l)
  lookupByRememberToken s t = withMVar (authBackend s) (\backend -> lookupByRememberToken backend t)
  destroy s u = withMVar (authBackend s) (\backend -> destroy backend u)

initSafeJsonFileAuthManager
  :: AuthSettings
  -> SnapletLens b SessionManager
  -> FilePath
  -> SnapletInit b (AuthManager b)
initSafeJsonFileAuthManager s l db = do
  makeSnaplet
    "SafeJsonFileAuthManager"
    "A snaplet providing user authentication using a JSON-file backend in threadsafe manner"
    Nothing $ liftIO $ do
      rng <- liftIO mkRNG
      key <- getKey (asSiteKey s)
      jsonMgr <- mkJsonAuthMgr db
      safeMgr <- mkSafeAuthManager jsonMgr
      return $! AuthManager {
          backend = safeMgr
        , session = l
        , activeUser = Nothing
        , minPasswdLen = asMinPasswdLen s
        , rememberCookieName = asRememberCookieName s
        , rememberPeriod = asRememberPeriod s
        , siteKey = key
        , lockout = asLockout s
        , randomNumberGenerator = rng
        }
