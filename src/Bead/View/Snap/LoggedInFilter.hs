{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
module Bead.View.Snap.LoggedInFilter (
    userIsLoggedInFilter
  , HandlerResult(..)
  ) where

import qualified Control.Exception as CE
import qualified Control.Monad.Error as CME
import           Data.Maybe
import           Prelude hiding (id)
import qualified Prelude as P

import           Snap.Snaplet.Auth as A
import           Text.Printf (printf)

import           Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as SC hiding (serviceContext)
import           Bead.View.Snap.Application
import           Bead.View.Snap.Content hiding (BlazeTemplate, template)
import           Bead.View.Snap.ContentHandler as ContentHandler
import           Bead.View.Snap.Session

-- TODO: I18N
-- | Logged In user combinator. It tries to authenticate the user with three methods.
-- The first method authenticate it using Snap auth, the second method authenticates
-- it using the session encoded user information. The third method authenticates it
-- using the service context. There is an extra criteria for the user, it's session
-- must be the same as the actual version, otherwise the authentication fails.
-- If the authentication is done, the 'inside' method is computed and the
-- result is propagated in (Just x) otherwise the whole computation returns Nothing
userIsLoggedInFilter
  :: Handler App b (HandlerResult a)
  -> Handler App b ()
  -> (String -> Handler App b ())
  -> Handler App b (Maybe a)
userIsLoggedInFilter inside outside onError = do
  sessionVer <- withTop sessionManager $ getSessionVersion
  case sessionVer of
    -- Session timed out
    Nothing -> onError "Lejárt a munkamenet!" >> return Nothing
    -- Active session
    Just _ -> do
      e <- CME.runErrorT loggedInFilter
      case e of
        Right x -> return x
        Left e' -> errorHappened . show $ e' 

  where
    errorHappened e = do
      logMessage ERROR e
      outside
      return Nothing

    someExceptionHandler :: (String -> Handler App b ()) -> CE.SomeException -> Handler App b ()
    someExceptionHandler onError e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      onError $ show e

    loggedInFilter = do
      -- Authenticated user information
      serverSideUser <- lift . withTop auth           $ currentUser
      sessionVer     <- lift . withTop sessionManager $ getSessionVersion

      -- Guards: invalid session version or invalid user
      when (sessionVer /= (Just sessionVersion)) . CME.throwError . strMsg $ "Nem megfelelő a munkamenet verziója!"
      when (isNothing serverSideUser)            . CME.throwError . strMsg $ "Ismeretlen felhasználó!"

      -- Username and page from session
      let unameFromAuth = usernameFromAuthUser . fromJust $ serverSideUser
      usernameFromSession <- lift . withTop sessionManager $ usernameFromSession

      -- Guard: invalid user in session
      when (usernameFromSession /= (Just unameFromAuth)) . CME.throwError . strMsg $
        printf "Hibás felhasználó a munkamenetben: %s, %s." (show unameFromAuth) (show usernameFromSession)

      -- Guard: Is user logged in?
      context <- lift . withTop serviceContext $ getServiceContext
      tkn     <- lift sessionToken
      let users = userContainer context
          usrToken = userToken (unameFromAuth, tkn)
      isLoggedIn <- lift (liftIO $ users `isUserLoggedIn` usrToken)
      unless (isLoggedIn) . CME.throwError . strMsg  $ "A felhasználó a szerveren nincs bejelentkezve!"

      -- Correct user is logged in, run the handler and save the data
      result <- lift inside
      case result of
        HFailure -> do lift $ withTop debugLoggerContext $ debugMessage "Login filter."
                       lift $ ContentHandler.logout
                       return Nothing
        HSuccess x -> do
          mUserData <- lift (liftIO $ users `userData` usrToken)

          when (isNothing mUserData) . CME.throwError . contentHandlerError $
            printf "Nem található adat a felhasználóhoz: %s" (show unameFromAuth)

          return (Just x)

-- | Represents if the handler run successfully or encountered an error.
-- HandSuccess when no exception occured during the execution and a value is calculated
-- HandFailure when some exception occured
data HandlerResult a
  = HSuccess a
  | HFailure
  deriving (Eq)

