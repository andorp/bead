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
import Bead.View.Snap.HandlerUtils as HU

import Bead.View.Snap.Login as L
import Bead.View.Snap.Registration
import Bead.View.Snap.Content hiding (BlazeTemplate, template)
import Bead.View.Snap.Content.All

-- Haskell imports

import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad (join)
import Control.Arrow ((&&&), (>>>))
import Control.Monad.Error (Error(..))
import qualified Control.Monad.CatchIO as CMC
import qualified Control.Exception as CE
import Control.Monad.Trans (lift)
import qualified Control.Monad.Error as CME

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
  e <- CME.runErrorT loggedInFilter
  case e of
    Right () -> return ()
    Left e' -> errorHappened e'

  where
    errorHappened e = do
      logMessage ERROR e
      outside

    someExceptionHandler :: CE.SomeException -> Handler App b ()
    someExceptionHandler e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      errorPageHandler $ T.append "Exception occured; " (T.pack . show $ e)

    loggedInFilter = do
      -- Authenticated user information
      serverSideUser <- lift . withTop auth           $ currentUser
      sessionVer     <- lift . withTop sessionManager $ getSessionVersion

      -- Guards: invalid session version or invalid user
      when (sessionVer /= (Just sessionVersion)) . CME.throwError $ "Invalid session version was found"
      when (isNothing serverSideUser)            . CME.throwError $ "Unknown user from session"

      -- Username and page from session
      let unameFromAuth = usernameFromAuthUser . fromJust $ serverSideUser
      usernameFromSession <- lift . withTop sessionManager $ usernameFromSession

      -- Guard: invalid user in session
      when (usernameFromSession /= (Just unameFromAuth)) . CME.throwError $ join [
              "hLoggedIn: invalid username from session ", show unameFromAuth
            , ", ", show usernameFromSession
            ]

      -- Guard: Is user logged in?
      context <- lift . withTop serviceContext $ getServiceContext
      let users = userContainer context
      isLoggedIn <- lift (liftIO $ users `isUserLoggedIn` unameFromAuth)
      unless (isLoggedIn) . CME.throwError $ "User is not logged in persistence"

      -- Guard: User's actul page differs from the one that is stored on the server
      pageFromSession <- lift . withTop sessionManager $ actPageFromSession
      mUserData       <- lift . liftIO $ userData users unameFromAuth
      when (isNothing mUserData) . CME.throwError $
        "No user data was found for the user " ++ show unameFromAuth
      when (Just (page (fromJust mUserData)) /= pageFromSession) . CME.throwError $ join [
          "Page stored in session and in server differs: ",
          "Server Side:", show (page (fromJust mUserData)), " <=> ",
          "Client Side:", show pageFromSession
        ]

      -- Correct user is logged in, run the handler and save the data
      lift inside
      mUserData <- lift (liftIO $ users `userData` unameFromAuth)

      when (isNothing mUserData) . CME.throwError $
        "No user data was found for the user " ++ show unameFromAuth

      lift $ CMC.catch
        (withTop sessionManager . setActPageInSession . page . fromJust $ mUserData)
        someExceptionHandler



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

runGETHandler
  :: (ContentHandlerError -> Handler App App ())
  -> HandlerError App App ()
  -> Handler App App ()
runGETHandler onError m = do
  x <- runErrorT m
  case x of
    Left e -> onError e
    Right y -> return y

runPOSTHandler
  :: (ContentHandlerError -> Handler App App ())
  -> P.Page
  -> HandlerError App App UserAction
  -> Handler App App ()
runPOSTHandler onError p m = do
  x <- runErrorT m
  case x of
    Left e -> onError e
    Right userAction -> do
      runStory $ do
        userStoryFor userAction
        S.changePage . P.parentPage $ p
      with sessionManager $ (commitSession >> touchSession)
      redirectToActPage


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
    notAllowedPage = withUserState $ \s -> do
      logMessage ERROR . join $ [
          "Page transition is not allowed "
        , show (page s), " -> ", show p
        ]
      L.logout

    changePage h =
      allowedPageByTransition p
        (do { runStory $ S.changePage p; h })
        notAllowedPage

    handleRenderPage :: Handler App App ()
    handleRenderPage = userIsLoggedInFilter

      -- If the GET handler is not found for the given page, the logout action is
      -- required as the user tries to do some invalid operation.
      (maybe
         -- GET handler is not found
         ((logMessage DEBUG $ "No GET handler found for " ++ show p) >> L.logout)
         -- GET handler is found
         changePage
         ((runGETHandler (error . show)) <$> (get c)) )

      -- Not logged in user tries to get some data
      (do withTop sessionManager $ resetSession
          L.logout)

    handleSubmitPage :: Handler App App ()
    handleSubmitPage = userIsLoggedInFilter

      -- If the POST handler is not found for the given page, logout action is
      -- required as the user tries to do some invalid operation
      (case post c of
         -- POST handler is not found
         Nothing -> do
           logMessage DEBUG $ "No POST handler found for " ++ show p
           L.logout
         -- POST handler is found
         Just handlerUserAction -> runPOSTHandler (error . show) p handlerUserAction
      )

      -- Not logged in user tires to post some data
      (do withTop sessionManager $ resetSession
          L.logout)

allowedPageByTransition :: P.Page -> Handler App App a -> Handler App App a -> Handler App App a
allowedPageByTransition p allowed restricted = withUserState $ \state ->
  case P.reachable (page state) p of
    False -> restricted
    True  -> allowed

