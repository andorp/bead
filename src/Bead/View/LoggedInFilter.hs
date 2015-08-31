{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
module Bead.View.LoggedInFilter (
    userIsLoggedInFilter
  , HandlerResult(..)
  , fayaxLoginFilter
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
import           Bead.Domain.Types (readMaybe)
import           Bead.View.BeadContext
import           Bead.View.Content hiding (BlazeTemplate, template)
import           Bead.View.ContentHandler as ContentHandler
import           Bead.View.Session

-- TODO: I18N
-- | Logged In user combinator. It tries to authenticate the user with three methods.
-- The first method authenticate it using Snap auth, the second method authenticates
-- it using the session encoded user information. The third method authenticates it
-- using the service context. There is an extra criteria for the user, it's session
-- must be the same as the actual version, otherwise the authentication fails.
-- If the authentication is done, the 'inside' method is computed and the
-- result is propagated in (Just x) otherwise the whole computation returns Nothing
userIsLoggedInFilter
  :: BeadHandler' b (HandlerResult a)
  -> BeadHandler' b ()
  -> (String -> BeadHandler' b ())
  -> BeadHandler' b (Maybe a)
userIsLoggedInFilter inside outside onError = do
  sessionVer <- getSessionVersion
  case sessionVer of
    -- Session timed out
    Nothing -> onError "Session timed out." >> return Nothing
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

    someExceptionHandler :: (String -> BeadHandler' b ()) -> CE.SomeException -> BeadHandler' b ()
    someExceptionHandler onError e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      onError $ show e

    loggedInFilter = do
      -- Authenticated user information
      serverSideUser <- lift currentUserTop
      sessionVer     <- lift getSessionVersion

      -- Guards: invalid session version or invalid user
      when (sessionVer /= (Just sessionVersion)) . CME.throwError . strMsg $ "Invalid session version"
      when (isNothing serverSideUser)            . CME.throwError . strMsg $ "Unknown user"

      -- Username and page from session
      let unameFromAuth = usernameFromAuthUser . fromJust $ serverSideUser
      usernameFromSession <- lift usernameFromSession

      -- Guard: invalid user in session
      when (usernameFromSession /= (Just unameFromAuth)) . CME.throwError . strMsg $
        printf "Invalid user in the session: %s, %s" (show unameFromAuth) (show usernameFromSession)

      -- Guard: Is user logged in?
      context <- lift getServiceContext
      tkn     <- lift sessionToken
      let users = userContainer context
          usrToken = userToken (unameFromAuth, tkn)
      isLoggedIn <- lift (liftIO $ users `isUserLoggedIn` usrToken)
      unless (isLoggedIn) . CME.throwError . strMsg  $ "This user is not logged into the server"

      -- Correct user is logged in, run the handler and save the data
      result <- lift inside
      case result of
        HFailure -> do lift $ debugMessage "Login filter."
                       lift $ ContentHandler.logout
                       return Nothing
        HSuccess x -> do
          mUserData <- lift (liftIO $ users `userData` usrToken)

          when (isNothing mUserData) . CME.throwError . contentHandlerError $
            printf "There can be no data found for the user: %s" (show unameFromAuth)

          return (Just x)

-- | Represents if the handler run successfully or encountered an error.
-- HandSuccess when no exception occured during the execution and a value is calculated
-- HandFailure when some exception occured
data HandlerResult a
  = HSuccess a
  | HFailure
  deriving (Eq)

-- * Fayax

-- Combines a given Fay ajax handler with the logged in filter,
-- which checks if the user is logged in. The result of the combination
-- is a filter which throws an exception if the user is not logged in, thus
-- it is not allowed to run ajax request without any authentication.
-- The exception is supposed to be catched by the snap at a certain point, when the
-- final message is generated with the "User is not logged in." message
fayaxLoginFilter
  :: (Data f1, Read f1, Show f2)
  => (f1 -> BeadHandler (HandlerResult f2))
  -> (f1 -> BeadHandler f2)
fayaxLoginFilter handler x = do
  result <- userIsLoggedInFilter (handler x) outside onError
  return $ fromMaybe resultError result
    where
      outside = return ()
      onError = const $ return ()
      resultError = error "User is not logged in."
