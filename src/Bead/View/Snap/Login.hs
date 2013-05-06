{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.Login (
    login
  , loginSubmit
  , Bead.View.Snap.Login.logout
  ) where

-- Bead imports

import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Application
import Bead.View.Snap.Dictionary (Language(..))
import Bead.View.Snap.Session
import Bead.View.Snap.HandlerUtils

import Bead.View.Snap.Content hiding (BlazeTemplate, template)
import Bead.View.Snap.Content.All

-- Haskell imports

import Data.String
import Data.ByteString.Char8 hiding (index)
import qualified Data.Text as T
import Control.Monad (join)

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session

-- import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

-- * Login and Logout handlers

login :: Maybe T.Text -> Handler App (AuthManager App) ()
login authError = blaze $ loginPage authError

-- TODO: Handle multiple login attempts correctly
-- One user should just log in at once.
loginSubmit :: Handler App b ()
loginSubmit = do
  withTop auth $ loginUser
    (fieldName loginUsername)
    (fieldName loginPassword)
    Nothing (\auth -> login (Just . T.pack . fromString . show $ auth)) $ do
      um <- currentUser
      case um of
        Nothing -> do
          logMessage ERROR $ "User is not logged during login submittion process"
          return ()
        Just authUser -> do
          context <- withTop serviceContext getServiceContext
          token   <- sessionToken
          let unameFromAuth = usernameFromAuthUser authUser
              passwFromAuth = passwordFromAuthUser authUser
          result <- liftIO $ S.runUserStory context UserNotLoggedIn (S.login unameFromAuth passwFromAuth token)
          case result of
            Right (val,userState) -> initSessionValues (page userState) unameFromAuth
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ show err
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              A.logout

  withTop sessionManager $ commitSession
  redirect "/"

  where
    err = Just . T.pack $ "Unknown user or password"

    initSessionValues :: P.Page -> Username -> Handler App b ()
    initSessionValues page username = do
      withTop sessionManager $ do
        setSessionVersion
        setLanguageInSession (Language "en")
        setUsernameInSession username
        setActPageInSession  page

      withTop serviceContext $ do
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page

logout :: Handler App b ()
logout = do
  um <- withTop auth $ currentUser
  case um of

    Nothing -> do
      logMessage ERROR "There is no user logged in to log out."
      withTop sessionManager $ resetSession
      redirect "/"

    Just authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      context <- withTop serviceContext $ getServiceContext
      let users = userContainer context
      -- Service context authentication
      token <- sessionToken
      liftIO $ users `userLogsOut` (userToken (unameFromAuth, token))
      withTop sessionManager $ resetSession
      withTop auth A.logout
      redirect "/"

-- * Blaze --

userForm :: AttributeValue -> AttributeValue -> Html
userForm act submitText = do
  H.form ! A.method "post" ! A.action act $ do
    H.table ! A.id "info" $ do
      H.tr $ do
        H.td "Login:"
        H.td $ H.input ! A.type_ "text"
                       ! A.name (fieldName loginUsername)
                       ! A.id   (fieldName loginUsername)
                       ! A.size "20"
      H.tr $ do
        H.td "Password:"
        H.td $ H.input ! A.type_ "password"
                       ! A.name (fieldName loginPassword)
                       ! A.id   (fieldName loginPassword)
                       ! A.size "20"
      H.tr $ do
        H.td $ return ()
        H.td $ H.input ! A.type_ "submit" ! A.id (fieldName loginSubmitBtn) ! A.value submitText

loginPage :: Maybe T.Text -> Html
loginPage err = withTitleAndHead content
  where
    content = do
      H.h1 $ "Login"
      maybe (return ()) (H.p . fromString. T.unpack) err
      userForm "/login" "Login"
      H.p $ do
        "Don't have a login yet? "
        H.a ! A.href "/new_user" $ "Create new user"
