{-# LANGUAGE FlexibleInstances #-}
module Bead.Controller.ServiceContext (
    UsrToken
  , UserToken(..)
  , UserState(..)
  , userStateCata
  , userNotLoggedIn
  , userRole
  , getStatus
  , setStatus
  , clearStatus
  , InRole(..)
  , actualPage
  , UserContainer(..)
  , ServiceContext(..)
  , serviceContext
  , scRunPersist
  , ioUserContainer
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities as Entities
import Bead.Persistence.Persist
import Bead.Controller.Pages as Pages
import Bead.Controller.Logging

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IORef (IORef)
import qualified Data.IORef as Ref
import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.Transaction.TIO (TIO)

import Bead.View.Snap.Translation

newtype UsrToken = UsrToken (Username, String)
  deriving (Show, Eq, Ord)

class UserToken u where
  userToken :: u -> UsrToken

instance UserToken (Username, String) where
  userToken (u,t) = UsrToken (u,t)

data UserState
  = UserNotLoggedIn
  | Registration
  | TestAgent
  | UserState {
    user :: Username -- Username
  , page :: PageDesc -- The page descriptor of the last requested one
  , name :: String   -- User's full name
  , role :: Role     -- User's role
  , token :: String  -- Token for the active user session
  , timezone :: TimeZone -- Timezone of the user
  , status :: Maybe (Translation String) -- The last status message
  } deriving (Show)

userStateCata
  userNotLoggedIn
  registration
  testAgent
  userState
  s = case s of
    UserNotLoggedIn -> userNotLoggedIn
    Registration -> registration
    TestAgent -> testAgent
    UserState u p n r t tz s -> userState u p n r t tz s

userNotLoggedIn :: UserState
userNotLoggedIn = UserNotLoggedIn

-- Converts the user state to a Role
userRole :: UserState -> Either OutsideRole Role
userRole = userStateCata
  (Left EmptyRole) -- userNotLoggedIn
  (Left RegRole)   -- registration
  (Left TestAgentRole) -- testAgent
  (\_u _p _n role _t _tz _s -> Right role) -- userState

-- Produces a new user state from the old one, setting
-- the status message to the given one
setStatus msg = userStateCata UserNotLoggedIn Registration TestAgent userState where
  userState u p n r t tz _ = UserState u p n r t tz (Just msg)

-- Produces the status message of the UserState, otherwise Nothing
getStatus = userStateCata Nothing Nothing Nothing status where
  status _ _ _ _ _ _ s = s

-- Produces a new status expect that the status message is cleared.
clearStatus = userStateCata UserNotLoggedIn Registration TestAgent userState where
  userState u p n r t tz _ = UserState u p n r t tz Nothing

instance UserToken UserState where
  userToken = userStateCata
    (UsrToken (Username "UNL", "UNL")) -- userNotLoggedIn
    (UsrToken (Username "REG", "REG")) -- registration
    (UsrToken (Username "TA", "TA"))   -- testAgent
    (\user _p _n _r token _tz _s -> UsrToken (user, token))

instance InRole UserState where
  isAdmin = userStateCata False False False (\_u _p _n role _t _tz _s -> isAdmin role)
  isCourseAdmin = userStateCata False False False (\_u _p _n role _t _tz _s -> Entities.isCourseAdmin role)
  isGroupAdmin = userStateCata False False False (\_u _p _n role _t _tz _s -> isGroupAdmin role)
  isStudent = userStateCata False False False (\_u _p _n role _t _tz _s -> isStudent role)

-- | The actual page that corresponds to the user's state
actualPage :: UserState -> Page ()
actualPage = userStateCata login' login' login' (\_u page _n _r _t _tz _s -> page)
  where
    login' = login ()

data UserContainer a = UserContainer {
    isUserLoggedIn :: UsrToken -> IO Bool
  , userLogsIn     :: UsrToken -> a -> IO ()
  , userLogsOut    :: UsrToken -> IO ()
  , userData       :: UsrToken -> IO (Maybe a)
  , modifyUserData :: UsrToken -> (a -> a) -> IO ()
  }

data ServiceContext = ServiceContext {
    persist       :: MVar ()
  , userContainer :: UserContainer UserState
  , logger        :: Logger
  }

serviceContext :: UserContainer UserState -> Logger -> IO ServiceContext
serviceContext u l = do
  mp <- newMVar ()
  return $ ServiceContext mp u l

-- Runs the IO computation, after having the access to the persistent
-- layer
scRunPersist :: ServiceContext -> IO a -> IO a
scRunPersist sc m =
  modifyMVar (persist sc) $ \p -> do
    x <- m
    return (p,x)

ioUserContainer :: IO (UserContainer a)
ioUserContainer = do
  v <- newMVar Map.empty

  let mvIsUserLoggedIn name = modifyMVar v $ \m -> do
        let isMember = Map.member name m
        return (m, isMember)

      mvUserLogsIn name val = modifyMVar v $ \m -> do
        -- Performace: Do not copy the whole value, again
        -- and again, just the reference to it.
        ref <- Ref.newIORef val
        let m' = Map.insert name ref m
        return (m', ())

      mvUserLogsOut name = modifyMVar v $ \m -> do
        let m' = Map.delete name m
        return (m', ())

      mvUserData name = modifyMVar v $ \m -> do
        x <- case Map.lookup name m of
          Nothing  -> return Nothing
          Just ref -> liftM Just $ Ref.readIORef ref
        return (m, x)

      mvModifyUserData name f = modifyMVar v $ \m -> do
        case Map.lookup name m of
          Nothing  -> return ()
          Just ref -> Ref.modifyIORef ref f
        return (m, ())

  return UserContainer {
      isUserLoggedIn = mvIsUserLoggedIn
    , userLogsIn     = mvUserLogsIn
    , userLogsOut    = mvUserLogsOut
    , userData       = mvUserData
    , modifyUserData = mvModifyUserData
    }
