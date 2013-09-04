{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.HandlerUtils (
    logMessage
  , withUserState
  , runStory  -- For logged in user
  , runStoryE -- For logged in user
  , registrationStory
  , getParameter
  , getJSONParam
  , i18nE
  , blazeI18n
  , renderPagelet
  , renderDynamicPagelet
  , setInSessionE
  , setReqParamInSession
  , sessionToken
  , userState
  , logout
  , HandlerError(..)
  , ContentHandlerError
  , contentHandlerError
  , contentHandlerErrorMap
  , module Control.Monad.Error
  ) where

-- Bead imports

import Bead.Controller.ServiceContext hiding (serviceContext, name)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Application
import Bead.View.Snap.Session
import Bead.View.Snap.DataBridge
import Bead.View.Snap.Dictionary
import Bead.View.Snap.Pagelets (Pagelet, runPagelet, runDynamicPagelet)
import Bead.View.Snap.RouteOf (ReqParam(..))

-- Haskell imports

import Data.String (IsString(..))
import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad.Error
import qualified Data.ByteString.Char8 as B

-- Snap and Blaze imports

import Text.Blaze.Html5 (Html)
import Snap hiding (get)
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth hiding (logout)
import qualified Snap.Snaplet.Auth as A (logout)
import Snap.Snaplet.Session

-- Fay imports

import Bead.View.Snap.Fay.JSON.ServerSide


newtype ContentHandlerError = ContentHandlerError (Maybe String)
  deriving (Show)

instance Error ContentHandlerError where
  noMsg  = ContentHandlerError Nothing
  strMsg = ContentHandlerError . Just

contentHandlerError :: String -> ContentHandlerError
contentHandlerError = ContentHandlerError . Just

contentHandlerErrorMap :: (Maybe String -> a) -> ContentHandlerError -> a
contentHandlerErrorMap f (ContentHandlerError x) = f x

type HandlerError a b c = ErrorT ContentHandlerError (Handler a b) c


-- | The 'logMessage' logs a message at a given level using the service context logger
logMessage :: LogLevel -> String -> Handler App b ()
logMessage lvl msg = do
  context <- withTop serviceContext $ getServiceContext
  liftIO $ L.log (logger context) lvl msg

sessionToken :: Handler App b String
sessionToken = T.unpack <$> (withTop sessionManager $ csrfToken)

userState :: (Error e) => ErrorT e (Handler App b) UserState
userState = do
  context   <- lift $ withTop serviceContext $ getServiceContext
  mUsername <- lift $ withTop sessionManager $ usernameFromSession
  token     <- lift $ sessionToken
  case mUsername of
    Nothing -> do
      lift $ logMessage ERROR "User is not logged in the session"
      throwError . strMsg $ "User is not logged in the session"
    Just user -> do
      let users = userContainer context
      userData <- liftIO $ users `userData` (userToken (user, token))
      case userData of
        Nothing -> do
          lift $ logMessage ERROR "No data found for the user"
          throwError . strMsg $ "No data found for the user"
        Just ud -> return ud

-- TODO: Show some error
errorPageHandler :: T.Text -> Handler App b ()
errorPageHandler msg = error "errorPageHandler: undefined" -- blaze errorPage

i18nE :: (IsString s) => HandlerError App b (String -> s)
i18nE = do
  lang <- lift . withTop sessionManager $ languageFromSession
  when (isNothing lang) . throwError . strMsg $ "Language was not defined in session"
  -- If the dictionary is not found for the language stored in session
  -- the identical dictionary is returned. The fromString is necessary
  -- for the Attribute names and values used in html templating engines
  d <- lift . withTop dictionaryContext . getDictionary . fromJust $ lang
  return (fromString . (unDictionary $ maybe idDictionary id d))

blazeI18n :: (I18N -> Html) -> HandlerError App b ()
blazeI18n h = i18nE >>= blaze . h

renderPagelet :: Pagelet -> HandlerError App b ()
renderPagelet p = i18nE >>= blaze . (runPagelet p)

renderDynamicPagelet :: Pagelet -> HandlerError App b ()
renderDynamicPagelet p = i18nE >>= blaze . (runDynamicPagelet p)

withUserState :: (UserState -> HandlerError App b c) -> HandlerError App b c
withUserState = (userState >>=)

getParameter :: Parameter a -> HandlerError App b a
getParameter param = do
  reqParam <- getParam . B.pack . name $ param
  when (isNothing reqParam) . throwError . strMsg . notFound $ param
  let v     = B.unpack . fromJust $ reqParam
      value = decode param v
  when (isNothing value) . throwError . strMsg . decodeError param $ v
  return . fromJust $ value

getJSONParam :: (Data a) => String -> String -> HandlerError App b a
getJSONParam param msg = do
  x <- getParam . B.pack $ param
  case x of
    Nothing -> throwError . strMsg $ msg
    Just y  -> case decodeFromFay . B.unpack $ y of
      Nothing -> throwError . strMsg $ "Decoding error"
      Just z  -> return z

setReqParamInSession :: ReqParam -> HandlerError App b ()
setReqParamInSession (ReqParam (k,v)) = setInSessionE k v

setInSessionE :: String -> String -> HandlerError App b ()
setInSessionE k v
  = lift . withTop sessionManager $ setInSession (T.pack k) (T.pack v)

runStoryE :: S.UserStory a -> HandlerError App b a
runStoryE story = do
  x <- lift . runStory $ story
  case x of
    Left e  -> throwError . strMsg . S.userErrorMsg $ e
    Right y -> return y

-- Runs a UserStory in the registration context
registrationStory :: S.UserStory a -> Handler App b (Either S.UserError a)
registrationStory s = withTop serviceContext getServiceContext >>=
  \context -> liftIO $ (forgetUserState <$> S.runUserStory context Registration s)
  where
    forgetUserState = either Left (Right . fst)

-- | Runs a user story for authenticated user and saves the new user state
--   into the service context
runStory :: S.UserStory a -> Handler App b (Either S.UserError a)
runStory story = withTop serviceContext $ do
  result <- serviceContextAndUserData $ \context users authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      token  <- sessionToken
      let usrToken = userToken (unameFromAuth, token)
      ustate <- liftIO $ userData users usrToken
      case ustate of
        Nothing -> return . Left . strMsg $ "The user was not authenticated: " ++ show unameFromAuth
        Just state -> do
          eResult <- liftIO $ S.runUserStory context state story
          case eResult of
            Left e -> return . Left $ e
            Right (a,state') -> do
              liftIO $ modifyUserData users usrToken (const state')
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

logout :: Handler App b ()
logout = do
  um <- withTop auth $ currentUser
  case um of
    Nothing -> do
      logMessage ERROR "There is no user logged in to log out."
      withTop sessionManager $ resetSession

    Just authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      context <- withTop serviceContext $ getServiceContext
      let users = userContainer context
      token <- sessionToken
      liftIO $ users `userLogsOut` (userToken (unameFromAuth, token))
      withTop sessionManager $ resetSession
      withTop auth A.logout
