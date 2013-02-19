{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.PageHandlers (
    routes
  ) where

-- Bead imports

import Prelude hiding (id)
import qualified Prelude as P
import Bead.Domain.Types
import Bead.Domain.Entities as E
import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.UserActions
import Bead.View.Snap.Application
import Bead.View.Snap.Session
import Bead.View.Snap.HandlerUtils

import Bead.View.Snap.Login as L
import Bead.View.Snap.Registration
import Bead.View.Snap.Content hiding (BlazeTemplate, index, template)
import Bead.View.Snap.Content.All

-- Haskell imports

import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad (join)
import Control.Arrow ((&&&), (>>>))
import Control.Monad.Error (Error(..))
import qualified Control.Monad.CatchIO as CMC
import qualified Control.Exception as CE

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveDirectory)

-- * Route table

routes :: [(ByteString, Handler App App ())]
routes = join
  [ -- Add login handlers
    [ ("/",         index)
    , ("/logout",   L.logout)
    , ("/new_user", with auth $ registration)
    ]
    -- Add all pages with template names and handlers
  , L.map (routeOf &&& ((P.id &&& content) >>> handlePage)) P.allPages
    -- Add static handlers
  , [ ("",          serveDirectory "static") ]
  ]

-- * Handlers

index :: Handler App App ()
index =
  ifTop $ requireUser auth
            (with auth $ login Nothing)
            (redirect (routeOf P.Home))

{- Logged In user combinator. It tries to authenticate the user with three methods.
   The first method authenticate it using Snap auth, the second method authenticates
   it using the session encoded user information. The third method authenticates it
   using the service context. There is an extra criteria for the user, it's session
   must be the same as the actual version, otherwise the authentication fails
-}
userIsLoggedInFilter :: Handler App b () -> Handler App b () -> Handler App b ()
userIsLoggedInFilter inside outside = do
  -- Login authentication
  um <- withTop auth $ currentUser
  sv <- withTop sessionManager $ getSessionVersion
  case (um,sv == (Just sessionVersion)) of
    (Nothing,_) -> do
      logMessage ERROR "Unknown user from session"
      outside
    (_, False) -> do
      logMessage ERROR "Invalid session version found"
      outside
    -- The user must be authenticated by the auth manager and also has to be the right session version
    (Just authUser, True) -> do
      -- Session cookie authentication
      let unameFromAuth = usernameFromAuthUser authUser
      unameFromSession <- withTop sessionManager $ usernameFromSession
      pageFromSession  <- withTop sessionManager $ actPageFromSession
      case (Just unameFromAuth) == unameFromSession of
        False -> do
          logMessage ERROR $ join [
              "hLoggedIn: invalid username from session ", show unameFromAuth
            , ", ", show unameFromSession
            ]
          outside
        True -> do
          context <- withTop serviceContext $ getServiceContext
          let users = userContainer context
          -- Service context authentication
          isLoggedIn <- liftIO $ users `isUserLoggedIn` unameFromAuth
          case isLoggedIn of
            False -> do
              logMessage ERROR "hLoggedIn: user is not logged in persistence"
              outside
            True -> do
              -- TODO: Authorize the user for the action
              inside
              mUserData <- liftIO $ users `userData` unameFromAuth
              case mUserData of
                Nothing -> do
                  logMessage ERROR $ "No user data was found for the user " ++ show unameFromAuth
                  outside
                Just ud ->
                  CMC.catch
                    (withTop sessionManager . setActPageInSession . page $ ud)
                    someExceptionHandler
  where
    someExceptionHandler :: CE.SomeException -> Handler App b ()
    someExceptionHandler e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      errorPageHandler $ T.append "Exception occured; " (T.pack . show $ e)

-- | The 'redirectToActPage' redirects to the page if the user's state stored in the cookie
--   and the service state are the same, otherwise it's raises an exception
redirectToActPage :: Handler App b ()
redirectToActPage = do
  pageInSession <- withTop sessionManager $ actPageFromSession
  withUserState $ \uState -> 
    case pageInSession == Just (page uState) of
      False -> do
        logMessage ERROR $ "Actual page data stored in session and in the server differ"
        error $ "Actual page data stored in session and in the server differ"
      True ->  redirect . routeOf . page $ uState

{- When a user logs in the home page is shown for her. An universal handler
   is used. E.g "/home" -> handlePage P.Home.
   * If the user can navigate to the
   intented page from its state, it's state is going to change in his session
   and in the server side as well.
   * When a user submits information with a POST request, from the submitted information
   we calculate the appropiate user action and runs it
-}
handlePage :: (P.Page, Content) -> Handler App App ()
handlePage (P.Login, _) = loginSubmit
handlePage (p,c) = method GET handleRenderPage <|> method POST handleSubmitPage
  where
    handleRenderPage :: Handler App App ()
    handleRenderPage = userIsLoggedInFilter
      -- Logged in user GET data
      (maybe
         (do logMessage DEBUG $ "No GET handler found for " ++ show p
             L.logout)
         P.id
         (get c))
      -- Not logged in user tries to get some data
      (do withTop sessionManager $ resetSession
          L.logout)

    handleSubmitPage :: Handler App App ()
    handleSubmitPage = userIsLoggedInFilter
      -- Logged in user POSTs data
      (case post c of
         Nothing -> do
           logMessage DEBUG $ "No POST handler found for " ++ show p
           L.logout
         Just handlerUserAction -> do
           userAction <- handlerUserAction
           -- let userAction = Profile -- .. TODO: calculate the user action
           withUserState $ \ustate -> do
             runStory $ userStoryFor ustate userAction
             with sessionManager $ (commitSession >> touchSession)
             redirectToActPage)
      -- Not logged in user tires to post some data
      (do withTop sessionManager $ resetSession
          L.logout)
