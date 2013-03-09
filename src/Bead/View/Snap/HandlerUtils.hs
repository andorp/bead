{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.HandlerUtils (
    logMessage
  , withUserState
  , withUserStateE
  , errorPageHandler
  , runStory  -- For logged in user
  , runStoryE -- For logged in user
  , getParamE
  , HandlerError(..)
  , ContentHandlerError
  , module Control.Monad.Error
  ) where

-- Bead imports

import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Application
import Bead.View.Snap.Session


-- Haskell imports

import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad.Error
import qualified Data.ByteString.Char8 as B

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session



newtype ContentHandlerError = ContentHandlerError (Maybe String)
  deriving (Show)

instance Error ContentHandlerError where
  noMsg  = ContentHandlerError Nothing
  strMsg = ContentHandlerError . Just

type HandlerError a b c = ErrorT ContentHandlerError (Handler a b) c


-- | The 'logMessage' logs a message at a given level using the service context logger
logMessage :: LogLevel -> String -> Handler App b ()
logMessage lvl msg = do
  context <- withTop serviceContext $ getServiceContext
  liftIO $ L.log (logger context) lvl msg

userState :: Handler App b UserState
userState = do
  context   <- withTop serviceContext $ getServiceContext
  mUsername <- withTop sessionManager $ usernameFromSession
  case mUsername of
    Nothing -> do
      logMessage ERROR "User is not logged in the session"
      error "User is not logged in the session"
    Just user -> do
      let users = userContainer context
      userData <- liftIO $ users `userData` user
      case userData of
        Nothing -> do
          logMessage ERROR "No data found for the user"
          error "No data found for the user"
        Just ud -> return ud

-- TODO: Show some error
errorPageHandler :: T.Text -> Handler App b ()
errorPageHandler msg = undefined -- blaze errorPage


withUserState :: (UserState -> Handler App b c) -> Handler App b c
withUserState h = userState >>= h

withUserStateE :: (UserState -> HandlerError App b c) -> HandlerError App b c
withUserStateE h = do
  u <- lift userState
  h u

getParamE :: String -> (String -> a) -> String -> HandlerError App b a
getParamE p f msg = do
  x <- lift . getParam . B.pack $ p
  case x of
    Nothing -> throwError . strMsg $ msg
    Just y  -> return . f . B.unpack $ y

runStoryE :: S.UserStory a -> HandlerError App b a
runStoryE story = do
  x <- lift . runStory $ story
  case x of
    Left e  -> throwError . strMsg . S.userErrorMsg $ e
    Right y -> return y

-- | Runs a user story for authenticated user and saves the new user state
--   into the service context
runStory :: S.UserStory a -> Handler App b (Either S.UserError a)
runStory story = withTop serviceContext $ do
  result <- serviceContextAndUserData $ \context users authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      ustate <- liftIO $ userData users unameFromAuth
      case ustate of
        Nothing -> return . Left . strMsg $ "The user was not authenticated: " ++ show unameFromAuth
        Just state -> do
          eResult <- liftIO $ S.runUserStory context state story
          case eResult of
            Left e -> return . Left $ e
            Right (a,state') -> do
              liftIO $ modifyUserData users unameFromAuth (const state')
              saveActPage state'
              return $ Right a
  case result of
    Left msg -> return . Left . strMsg . show $ msg
    Right x -> return x

  where
    saveActPage state = withTop sessionManager $ do
      setActPageInSession $ page state
      commitSession
      touchSession

    serviceContextAndUserData
      :: (ServiceContext -> UserContainer UserState -> AuthUser -> Handler App SnapletServiceContext a)
      -> Handler App SnapletServiceContext (Either String a)
    serviceContextAndUserData f = do
      context <- getServiceContext
      let users = userContainer context
      um <- withTop auth $ currentUser
      case um of
        Nothing -> return . Left $ "Unauthenticated user"
        Just authUser -> liftM Right $ f context users authUser

