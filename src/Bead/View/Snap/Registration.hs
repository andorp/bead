{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.Registration (
    registration
  , createAdminUser
  ) where

-- Bead imports

import Bead.Controller.Logging as L

import qualified Bead.Controller.UserStories as S
import qualified Bead.Controller.Pages as P (Page(Login))
import Bead.View.Snap.Application
import Bead.View.Snap.Session
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.DataBridge
import qualified Bead.Persistence.Persist as P (Persist(..), runPersist)

import Bead.View.Snap.Content hiding (
    BlazeTemplate, name, template, empty, method
  )

-- Haskell imports

import Data.Maybe (fromJust, isNothing)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Auth.Backends.JsonFile (mkJsonAuthMgr)

import Text.Blaze (textTag)
import Text.Blaze.Html5 (Html, (!)) -- hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)

createAdminUser :: P.Persist -> FilePath -> String -> String -> IO ()
createAdminUser persist usersdb name password = do
  mgr <- mkJsonAuthMgr usersdb
  pwd <- encryptPassword . ClearText . fromString $ password
  let authUser = defAuthUser {
      userLogin    = fromString name
    , userPassword = Just pwd
    }
  save mgr authUser
  let usr = User {
      u_role = Admin
    , u_username = Username name
    , u_email = Email ""
    , u_name = ""
    }
  createdUser <- lookupByLogin mgr (T.pack name)
  case createdUser of
    Nothing -> error "No user was created"
    Just u' -> case passwordFromAuthUser u' of
      Nothing  -> error "No password was given"
      Just pwd -> P.runPersist $ P.saveUser persist usr pwd
  return ()

-- * User registration handler

data RegError
  = RegError LogLevel String
  | RegErrorUserExist Username

instance Error RegError where
  noMsg      = RegError DEBUG ""
  strMsg msg = RegError DEBUG msg

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . B.pack . name $ param
  return (reqParam >>= decode param . B.unpack)

registration :: Handler App (AuthManager App) ()
registration = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = blaze $ newUser

    -- Registers a user as a Student, the administrator can grant
    -- better access to the user
    handleFormSubmit = do
      regResult <- runErrorT $ do
        -- Check if the user does not exist in the system
        uname <- readParameter regUsernamePrm
        when (isNothing uname) $ throwError (RegError ERROR "Username is not found in the request parameters")
        let username = fromJust uname
        context <- lift $ withTop serviceContext $ getServiceContext
        exist   <- liftIO $ S.runUserStory context Registration (S.doesUserExist username)
        case exist of
          Left _          -> throwError (RegError ERROR "User story failed")
          Right (True, _) -> throwError (RegErrorUserExist username)
          _               -> return ()

        -- Register the user in the Snap auth module
        lift $ registerUser
          (fieldName loginUsername)
          (fieldName loginPassword)
        -- Register the user in the service context module
        passwBS    <- readParameter regPasswordPrm -- (fieldName loginPassword)
        email      <- readParameter regEmailPrm    -- (fieldName regEmailAddress)
        fullname   <- getParam (fieldName regFullName)
        case (uname,email,fullname, passwBS) of
          (Just u,Just e, Just f, Just p) -> do
            -- Create a user info for the service context and
            -- read the created user info from the Snap auth service context
            -- and run the registration user story in the service context
            -- with a freshly created (and encrypted) user password
            -- in the user context
            let usr = User {
                  u_role = Student
                , u_username = u
                , u_email = e
                , u_name = unpack f
                }
            createdUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameMap T.pack u)
            when (isNothing createdUser) $ throwError (RegError ERROR "User was not created at the first stage")
            let user = fromJust createdUser
                mpwd = passwordFromAuthUser user
            when (isNothing mpwd) $ throwError (RegError ERROR "No password was given")
            let pwd = fromJust mpwd
            result <- liftIO $ S.runUserStory context Registration (S.createUser usr pwd)
            case result of
              Left err -> throwError (RegError ERROR (show err))
              _        -> throwError (RegError INFO "Everything went fine. The user is created.")

          _ -> throwError (RegError ERROR "Username, email, or family name was not provided by the form")
      case regResult of
        Left (RegErrorUserExist username) ->
          do withTop serviceContext $ logMessage INFO $ (usernameMap ("User already exist: "++) username)
             redirect "/"
        Left (RegError lvl msg) ->
          do withTop serviceContext $ logMessage lvl msg
             redirect "/"
        Right _ ->
          do redirect "/"

-- * Blaze

newUser :: Html
newUser = dynamicTitleAndHead "Registration" content
  where
    content = do
      H.h1 $ "Register a new user"
      registrationForm "/new_user"
      linkToPageWithText P.Login "Go back to the login page"

registrationForm :: String -> Html
registrationForm postAction = do
  postForm postAction ! (A.id . formId $ regForm) $ do
    table (fieldName registrationTable) (fieldName registrationTable) $ do
      tableLine "Username:"      $ textInput (fieldName loginUsername)     20 Nothing ! A.required ""
      tableLine "Password:"      $ passwordInput (fieldName loginPassword) 20 Nothing ! A.required ""
      tableLine "Email address:" $ textInput (fieldName regEmailAddress)   20 Nothing ! A.required ""
      tableLine "Full name:"     $ textInput (fieldName regFullName)       20 Nothing ! A.required ""
    submitButton (fieldName regSubmitBtn) "Register"
