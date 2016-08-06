{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Bead.Controller.ServiceContext (
    UsrToken
  , UserToken(..)
  , UserState(..)
  , StatusMessage(..)
  , statusMessage
  , userStateCata
  , userNotLoggedIn
  , userRole
  , getStatus
  , setStatus
  , clearStatus
  , usernameInState
  , InRole(..)
  , actualPage
  , UserContainer(..)
  , ServiceContext(..)
  , serviceContext
  , ioUserContainer
  ) where

import qualified Data.Map as Map

import           Control.Concurrent.STM

import           Bead.Controller.Pages as Pages
import           Bead.Controller.Logging
import           Bead.Domain.Entities as Entities
import           Bead.View.Translation
import qualified Bead.Persistence.Persist as Persist

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
  | NotificationAgent
  | UserState {
    user :: Username -- Username
  , uid  :: Uid
  , page :: PageDesc -- The page descriptor of the last requested one
  , name :: String   -- User's full name
  , role :: Role     -- User's role
  , token :: String  -- Token for the active user session
  , timezone :: TimeZoneName -- Timezone of the user
  , status :: Maybe (StatusMessage (Translation String)) -- The last status message
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
    UserState u ui p n r t tz s -> userState u ui p n r t tz s

userNotLoggedIn :: UserState
userNotLoggedIn = UserNotLoggedIn

-- Converts the user state to a Role
userRole :: UserState -> Either OutsideRole Role
userRole = userStateCata
  (Left EmptyRole) -- userNotLoggedIn
  (Left RegRole)   -- registration
  (Left TestAgentRole) -- testAgent
  (\_u _ui _p _n role _t _tz _s -> Right role) -- userState

-- Produces a new user state from the old one, setting
-- the status message to the given one
setStatus msg = userStateCata UserNotLoggedIn Registration TestAgent userState where
  userState u ui p n r t tz _ = UserState u ui p n r t tz (Just msg)

-- Produces the status message of the UserState, otherwise Nothing
getStatus = userStateCata Nothing Nothing Nothing status where
  status _ _ _ _ _ _ _ s = s

-- Produces a new status expect that the status message is cleared.
clearStatus = userStateCata UserNotLoggedIn Registration TestAgent userState where
  userState u ui p n r t tz _ = UserState u ui p n r t tz Nothing

-- Returns a username stored in the user state, or a description
-- string for the state
usernameInState = userStateCata
  (Username "NotLoggedIn")
  (Username "Registration")
  (Username "TestAgent")
  (\user _ui _p _n _r _t _tz _s -> user)

instance UserToken UserState where
  userToken = userStateCata
    (UsrToken (Username "UNL", "UNL")) -- userNotLoggedIn
    (UsrToken (Username "REG", "REG")) -- registration
    (UsrToken (Username "TA", "TA"))   -- testAgent
    (\user _ui _p _n _r token _tz _s -> UsrToken (user, token))

instance InRole UserState where
  isAdmin = userStateCata False False False (\_u _ui _p _n role _t _tz _s -> isAdmin role)
  isCourseAdmin = userStateCata False False False (\_u _ui _p _n role _t _tz _s -> Entities.isCourseAdmin role)
  isGroupAdmin = userStateCata False False False (\_u _ui _p _n role _t _tz _s -> isGroupAdmin role)
  isStudent = userStateCata False False False (\_u _ui _p _n role _t _tz _s -> isStudent role)

-- | The actual page that corresponds to the user's state
actualPage :: UserState -> PageDesc
actualPage = userStateCata login' login' login' (\_u _ui page _n _r _t _tz _s -> page)
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
    userContainer :: UserContainer UserState
  , logger        :: Logger
  , persistInterpreter :: Persist.Interpreter
  }

serviceContext :: UserContainer UserState -> Logger -> Persist.Interpreter -> IO ServiceContext
serviceContext u l i = do
  return $ ServiceContext u l i

ioUserContainer :: IO (UserContainer a)
ioUserContainer = do
  v <- newTVarIO Map.empty

  let mvIsUserLoggedIn name = atomically $
        fmap (Map.member name) (readTVar v)

      mvUserLogsIn name val = atomically $
        withTVar v (Map.insert name val)

      mvUserLogsOut name = atomically $
        withTVar v (Map.delete name)

      mvUserData name = atomically $ do
        fmap (Map.lookup name) (readTVar v)

      mvModifyUserData name f = atomically $ do
        m <- readTVar v
        case Map.lookup name m of
          Nothing -> return ()
          Just x  -> writeTVar v (Map.insert name (f x) m)

  return UserContainer {
      isUserLoggedIn = mvIsUserLoggedIn
    , userLogsIn     = mvUserLogsIn
    , userLogsOut    = mvUserLogsOut
    , userData       = mvUserData
    , modifyUserData = mvModifyUserData
    }
  where
    withTVar :: TVar a -> (a -> a) -> STM ()
    withTVar var f = do
      x <- readTVar var
      writeTVar var (f x)

