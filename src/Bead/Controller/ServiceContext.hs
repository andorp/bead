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
import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Controller.Pages
import Bead.Controller.Logging

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IORef (IORef)
import qualified Data.IORef as Ref
import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.Transaction.TIO (TIO)

newtype UsrToken = UsrToken (Username, String)
  deriving (Show, Eq, Ord)

class UserToken u where
  userToken :: u -> UsrToken

instance UserToken (Username, String) where
  userToken (u,t) = UsrToken (u,t)

data UserState
  = UserNotLoggedIn
  | Registration
  | UserState {
    user :: Username
  , page :: Page
  , name :: String
  , role :: Role
  , token :: String
  , timezone :: TimeZone
  , status :: Maybe String
  } deriving (Show)

userStateCata
  userNotLoggedIn
  registration
  userState
  s = case s of
    UserNotLoggedIn -> userNotLoggedIn
    Registration -> registration
    UserState u p n r t tz s -> userState u p n r t tz s

userNotLoggedIn :: UserState
userNotLoggedIn = UserNotLoggedIn

userRole :: UserState -> Either OutsideRole Role
userRole UserNotLoggedIn = Left EmptyRole
userRole Registration    = Left RegRole
userRole s@(UserState{}) = Right . role $ s

-- Produces a new user state from the old one, setting
-- the status message to the given one
setStatus msg = userStateCata UserNotLoggedIn Registration userState where
  userState u p n r t tz _ = UserState u p n r t tz (Just msg)

-- Produces the status message of the UserState, otherwise Nothing
getStatus = userStateCata Nothing Nothing status where
  status _ _ _ _ _ _ s = s

-- Produces a new status expect that the status message is cleared.
clearStatus = userStateCata UserNotLoggedIn Registration userState where
  userState u p n r t tz _ = UserState u p n r t tz Nothing

instance UserToken UserState where
  userToken UserNotLoggedIn = error "Impossible: userToken UserNotLoggedIn"
  userToken u = UsrToken (user u, token u)

instance InRole UserState where
  isAdmin       UserNotLoggedIn = False
  isAdmin       s = isAdmin . role $ s

  isCourseAdmin UserNotLoggedIn = False
  isCourseAdmin s = isCourseAdmin . role $ s

  isProfessor   UserNotLoggedIn = False
  isProfessor   s = isProfessor . role $ s

  isStudent     UserNotLoggedIn = False
  isStudent     s = isStudent . role $ s


-- | The actual page that corresponds to the user's state
actualPage :: UserState -> Page
actualPage UserNotLoggedIn = Login
actualPage u               = page u

data UserContainer a = UserContainer {
    isUserLoggedIn :: UsrToken -> IO Bool
  , userLogsIn     :: UsrToken -> a -> IO ()
  , userLogsOut    :: UsrToken -> IO ()
  , userData       :: UsrToken -> IO (Maybe a)
  , modifyUserData :: UsrToken -> (a -> a) -> IO ()
  }

data ServiceContext = ServiceContext {
    persist       :: MVar Persist
  , userContainer :: UserContainer UserState
  , logger        :: Logger
  }

serviceContext :: Persist -> UserContainer UserState -> Logger -> IO ServiceContext
serviceContext p u l = do
  mp <- newMVar p
  return $ ServiceContext mp u l

scRunPersist :: ServiceContext -> (Persist -> IO a) -> IO a
scRunPersist sc m =
  modifyMVar (persist sc) $ \p -> do
    x <- m p
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
