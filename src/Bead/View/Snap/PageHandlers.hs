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
import Bead.View.Snap.ErrorPage

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
import Snap.Snaplet.Fay
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveDirectory)

-- * Route table

routes :: [(ByteString, Handler App App ())]
routes = join
  [ -- Add login handlers
    [ ("/",         index)
    , ("/logout",   logoutAndResetRoute)
    , ("/new_user", with auth $ registration)
    , ("/fay", with fayContext fayServe)
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
userIsLoggedInFilter
  :: Handler App b HandlerResult
  -> Handler App b ()
  -> (String -> Handler App b ())
  -> Handler App b ()
userIsLoggedInFilter inside outside onError = do
  sessionVer <- withTop sessionManager $ getSessionVersion
  case sessionVer of
    -- Session timed out
    Nothing -> onError "Session timed out"
    -- Active session
    Just _ -> do
      e <- CME.runErrorT loggedInFilter
      case e of
        Right () -> return ()
        Left e' -> errorHappened . show $ e'

  where
    errorHappened e = do
      logMessage ERROR e
      outside

    someExceptionHandler :: (String -> Handler App b ()) -> CE.SomeException -> Handler App b ()
    someExceptionHandler onError e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      onError $ show e

    loggedInFilter = do
      -- Authenticated user information
      serverSideUser <- lift . withTop auth           $ currentUser
      sessionVer     <- lift . withTop sessionManager $ getSessionVersion

      -- Guards: invalid session version or invalid user
      when (sessionVer /= (Just sessionVersion)) . CME.throwError . strMsg $ "Invalid session version was found"
      when (isNothing serverSideUser)            . CME.throwError . strMsg $ "Unknown user from session"

      -- Username and page from session
      let unameFromAuth = usernameFromAuthUser . fromJust $ serverSideUser
      usernameFromSession <- lift . withTop sessionManager $ usernameFromSession

      -- Guard: invalid user in session
      when (usernameFromSession /= (Just unameFromAuth)) . CME.throwError . strMsg  $ join [
              "hLoggedIn: invalid username from session ", show unameFromAuth
            , ", ", show usernameFromSession
            ]

      -- Guard: Is user logged in?
      context <- lift . withTop serviceContext $ getServiceContext
      tkn     <- lift sessionToken
      let users = userContainer context
          usrToken = userToken (unameFromAuth, tkn)
      isLoggedIn <- lift (liftIO $ users `isUserLoggedIn` usrToken)
      unless (isLoggedIn) . CME.throwError . strMsg  $ "User is not logged in persistence"

      -- Guard: User's actul page differs from the one that is stored on the server
      pageFromSession <- lift . withTop sessionManager $ actPageFromSession
      mUserData       <- lift . liftIO $ userData users usrToken -- unameFromAuth
      when (isNothing mUserData) . CME.throwError . strMsg  $
        "No user data was found for the user " ++ show unameFromAuth
      when (Just (page (fromJust mUserData)) /= pageFromSession) . CME.throwError . strMsg  $ join [
          "Page stored in session and in server differs: ",
          "Server Side:", show (page (fromJust mUserData)), " <=> ",
          "Client Side:", show pageFromSession
        ]

      -- Correct user is logged in, run the handler and save the data
      result <- lift inside
      case result of
        HFailure -> lift $ HU.logout
        HSuccess -> do
          mUserData <- lift (liftIO $ users `userData` usrToken)

          when (isNothing mUserData) . CME.throwError . contentHandlerError $
            "No user data was found for the user " ++ show unameFromAuth

          lift $ (CMC.catch
                    (withTop sessionManager . setActPageInSession . page . fromJust $ mUserData)
                    (someExceptionHandler onError))

-- | The 'redirectToActPage' redirects to the page if the user's state stored in the cookie
--   and the service state are the same, otherwise it's raises an exception
redirectToActPage :: HandlerError App b ()
redirectToActPage = do
  pageInSession <- lift $ withTop sessionManager $ actPageFromSession
  withUserState $ \uState ->
    case pageInSession == Just (page uState) of
      False -> do
        lift $ logMessage ERROR $ "Actual page data stored in session and in the server differ"
        throwError . strMsg $ "Actual page data stored in session and in the server differ"
      True ->  redirect . routeOf . page $ uState

-- | Represents the result of a GET or POST handler
-- HandSuccess when no exception occured during the execution
-- HandFailure when some exception occured
data HandlerResult
  = HSuccess
  | HFailure
  deriving (Eq)

hsuccess :: Handler App a b -> Handler App a HandlerResult
hsuccess h = h >> (return HSuccess)

hfailure :: Handler App a b -> Handler App a HandlerResult
hfailure h = h >> (return HFailure)

evalHandlerError
  :: (ContentHandlerError -> Handler App b a)
  -> (c -> Handler App b a)
  -> HandlerError App b c
  -> Handler App b a
evalHandlerError onError onSuccess h = do
  x <- runErrorT h
  case x of
    Left e  -> onError e
    Right s -> onSuccess s

runGETHandler
  :: (ContentHandlerError -> Handler App App ())
  -> HandlerError App App ()
  -> Handler App App HandlerResult
runGETHandler onError
  = evalHandlerError
      (hfailure . onError)
      (\_ -> return HSuccess)

runPOSTHandler
  :: (ContentHandlerError -> Handler App App ())
  -> P.Page
  -> HandlerError App App UserAction
  -> Handler App App HandlerResult
runPOSTHandler onError p h
  = evalHandlerError
      (hfailure . onError)
      (\_ -> return HSuccess)
      (do userAction <- h
          lift $ runStory $ do
            userStoryFor userAction
            S.changePage . P.parentPage $ p
          lift $ with sessionManager $ (commitSession >> touchSession)
          redirectToActPage)

logoutAndResetRoute :: Handler App App ()
logoutAndResetRoute = do
  HU.logout
  redirect "/"

logoutAndErrorPage :: String -> Handler App App ()
logoutAndErrorPage msg = do
  HU.logout
  msgErrorPage msg

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
    failure, success :: (Monad m) => a -> m HandlerResult
    failure = const . return $ HFailure
    success = const . return $ HSuccess
    forgetResult h = h >> return ()

    notAllowedPage = withUserState $ \s -> do
      lift $ logMessage ERROR . join $ [
          "Page transition is not allowed "
        , show (page s), " -> ", show p
        ]
      lift $ logoutAndResetRoute

    changePage h =
      allowedPageByTransition p
        (do lift $ runStory $ S.changePage p
            h)
        notAllowedPage

    handleRenderPage :: Handler App App ()
    handleRenderPage = userIsLoggedInFilter

      -- If the GET handler is not found for the given page, the logout action is
      -- required as the user tries to do some invalid operation.
      (case get c of
         Nothing -> hsuccess $ do
           logMessage DEBUG $ "No GET handler found for " ++ show p
           logoutAndResetRoute
         Just getHandler -> runGETHandler errorPage $ changePage getHandler
      )

      -- Not logged in user tries to get some data
      logoutAndResetRoute

      -- Some internal error happened
      logoutAndErrorPage

    handleSubmitPage :: Handler App App ()
    handleSubmitPage = userIsLoggedInFilter

      -- If the POST handler is not found for the given page, logout action is
      -- required as the user tries to do some invalid operation
      (case post c of
         -- POST handler is not found
         Nothing -> hsuccess $ do
           logMessage DEBUG $ "No POST handler found for " ++ show p
           logoutAndResetRoute
         -- POST handler is found
         Just handlerUserAction -> runPOSTHandler errorPage p handlerUserAction
      )

      -- Not logged in user tires to post some data
      logoutAndResetRoute

      -- Some internal error happened
      logoutAndErrorPage

allowedPageByTransition
  :: P.Page -> HandlerError App App a -> HandlerError App App a -> HandlerError App App a
allowedPageByTransition p allowed restricted = withUserState $ \state ->
  let allow = and [
          P.reachable (page state) p
        , elem p (P.allowedPages (role state))
        ]
  in case allow of
    False -> restricted
    True  -> allowed
