module Bead.Controller.ServiceContext where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Controller.Pages
import Bead.Controller.Logging

{-
import Data.Set (Set)
import qualified Data.Set as Set
-}

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IORef (IORef)
import qualified Data.IORef as Ref
import Control.Concurrent.MVar
import Control.Monad (liftM)

type CourseName = String
type GroupName  = String

-- TODO: UTF8 named strings
data UserState
  = UserNotLoggedIn
  | UserState {
    user :: Username
  , page :: Page
  , name :: String
  , role :: Role
  , availablePages :: [Page]
  , pageRenderData :: PageRenderData
  }

-- | The actual page that corresponds to the user's state
actualPage :: UserState -> Page
actualPage UserNotLoggedIn = Login
actualPage u               = page u

data PageRenderData
  = NoPageData
  | HomePageData
    { courses :: [Stored Encrypted CourseName]
    , groups :: [Stored Encrypted GroupName]
    , exams :: [Stored Encrypted ExamInfo]
    }

isHomePageData :: PageRenderData -> Bool
isHomePageData (HomePageData {}) = True
isHomePageData _                 = False

data UserContainer a = UserContainer {
    isUserLoggedIn :: Username -> IO Bool
  , userLogsIn     :: Username -> a -> IO ()
  , userLogsOut    :: Username -> IO ()
  , userData       :: Username -> IO (Maybe a)
  , modifyUserData :: Username -> (a -> a) -> IO ()
  }

data ServiceContext = ServiceContext {
    persist       :: Persist
  , userContainer :: UserContainer UserState
  , logger        :: Logger
  }

serviceContext :: Persist -> UserContainer UserState -> Logger -> ServiceContext
serviceContext = ServiceContext

{-
ioUserContainer :: IO UserContainer
ioUserContainer = do
  p <- Ref.newIORef Set.empty

  let ioIsUserLoggedIn name = do
        set <- Ref.readIORef p
        return $ Set.member name set

      ioUserLogsIn name = Ref.modifyIORef p $ Set.insert name

      ioUserLogsOut name = Ref.modifyIORef p $ Set.delete name

  return UserContainer {
      isUserLoggedIn = ioIsUserLoggedIn
    , userLogsIn     = ioUserLogsIn
    , userLogsOut    = ioUserLogsOut
    }
-}
    
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

mockUserContainer = UserContainer {
    isUserLoggedIn = \_ -> return False
  , userLogsIn  = \_ _ -> return ()
  , userLogsOut = \_ -> return ()
  , userData    = \_ -> return Nothing
  , modifyUserData = \_ _ -> return ()
  }
