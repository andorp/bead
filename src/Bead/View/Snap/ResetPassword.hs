{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.ResetPassword (
    resetPasswordPage
  , updateCurrentAuthPassword
  , checkCurrentAuthPassword
  , encryptPwd
  , loadAuthUser
  ) where

import Control.Monad.Trans.Error
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.Maybe

import Snap
import Snap.Snaplet.Auth as A
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Domain.Entities
import qualified Bead.Controller.UserStories as S
import Bead.Controller.Pages as P (Page(Login))
import Bead.View.Snap.Application
import Bead.View.Snap.Content hiding (name)
import Bead.View.Snap.DataBridge
import Bead.View.Snap.ErrorPage (errorPageWithTitle)
import Bead.View.Snap.EmailTemplate (ForgottenPassword(..))
import Bead.View.Snap.HandlerUtils (registrationStory, userState)
import Bead.View.Snap.Session (passwordFromAuthUser)
import Bead.View.Snap.Style

-- Generates a new random password for the given user
-- and saves it to the persistence layer and the authentication
-- and sends the email to the given user.
-- The handler calculates unit if everything went fine, otherwise a string
-- indicating the reason.
resetPassword :: Username -> ErrorT String (Handler App a) ()
resetPassword u = do
  checkUserInAuth
  checkUserInPersistence
  password <- randomPassword
  user <- loadAuthUser u
  encryptedPwd <- encryptPwd password
  updateUser user { userPassword = Just encryptedPwd }
  email password
  where
    checkUserInAuth = do
      exist    <- lift . withTop auth $ usernameExists (usernameStr u)
      unless exist . throwError $ "User does not exist: " ++ (usernameStr u)

    checkUserInPersistence =
      (lift $ registrationStory $ S.doesUserExist u) >>=
      either (throwError . show)
             (\e -> unless e $ throwError "User is not in persistence")

    randomPassword = lift . withTop randomPasswordContext $ getRandomPassword

    loadUserFromPersistence =
      (lift $ registrationStory $ S.loadUser u) >>=
      (either (throwError . show) return)

    email pwd = do
      address <- fmap u_email loadUserFromPersistence
      lift $ withTop sendEmailContext $
        sendEmail
          address
          "BE-AD Password reset"
          ForgottenPassword { restoreUrl = pwd }

usernameStr :: (IsString s) => Username -> s
usernameStr = usernameCata fromString

loadAuthUser :: (Error e) => Username -> ErrorT e (Handler App a) AuthUser
loadAuthUser u = do
  usr <- lift . withTop auth $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameStr u)
  when (isNothing usr) $ throwError $ strMsg "User is not in the authentication"
  return . fromJust $ usr

updateUser :: (Error e) => AuthUser -> ErrorT e (Handler App a) AuthUser
updateUser usr =
  (lift $ withTop auth $ withBackend $ \r -> liftIO $ save r usr) >>=
  either (throwError . strMsg . show) return

encryptPwd :: (Error e) => String -> ErrorT e (Handler App a) A.Password
encryptPwd = liftIO . encryptPassword . ClearText . fromString

-- Check if the current auth password is the same as the given one
-- If they are different an error is thrown.
checkCurrentAuthPassword :: (Error e) => String -> ErrorT e (Handler App a) ()
checkCurrentAuthPassword pwd = do
  name <- user <$> userState
  result <- lift $ withTop auth $
    loginByUsername (usernameCata fromString name) (ClearText $ fromString pwd) False
  when (isLeft result) . throwError $ strMsg "Invalid password"

-- Update the currently logged in user's password in the authentication module
updateCurrentAuthPassword :: (Error e) => String -> ErrorT e (Handler App a) ()
updateCurrentAuthPassword password = do
  name <- user <$> userState
  usr <- loadAuthUser name
  encPwd <- encryptPwd password
  updateUser (usr { userPassword = Just encPwd })
  return ()

resetPasswordPage :: Handler App App ()
resetPasswordPage = method GET resetPasswordGET <|> method POST resetPasswordPOST

{- Reset password GET handler
Renders the password reset request page. The page contains
two input fields for the user's name and the user's email
address. The user fills out the form, and clicks on "Reset password" button
and submit the requests.
-}
resetPasswordGET :: Handler App App ()
resetPasswordGET = renderForm
  where
    renderForm = blaze $ dynamicTitleAndHead "Reset Password" $ do
      H.h1 "Reset the password"
      postForm "/reset_pwd" $ do
        table (fieldName resetPasswordTable) (fieldName resetPasswordTable) # centerTable $ do
          tableLine "Username:"       $ textInput (name regUsernamePrm) 20 Nothing ! A.required ""
          tableLine "Email address: " $ textInput (name regEmailPrm)    20 Nothing ! A.required ""
        submitButton (fieldName pwdSubmitBtn) "Reset Password"
      linkToPageWithText P.Login "Go back to the login page"

{- Reset password POST handler
Reads out the parameters for the username and the email address, checks
if the user exist in the persistence layer with the given email address.
If the user is not exist or the given address differs, the error page is rendered.
-}
resetPasswordPOST :: Handler App App ()
resetPasswordPOST = renderErrorPage $ runErrorT $ do
  u <- readParameter regUsernamePrm
  e <- readParameter regEmailPrm
  case (u,e) of
    (Just username, Just email) -> do
      user <- loadUser username
      when (email /= (u_email user)) $ throwError $
        "Username or email address was not valid: " ++ show email ++ " " ++ show (u_email user)
      resetPassword username
      blaze $ dynamicTitleAndHead "Reset Password" $ do
        "Please check your emails"
        H.br
        linkToPageWithText P.Login "Go back to the login page"
    _ -> throwError "Username or email address was not given"
  where
    renderErrorPage :: Handler App App (Either String ()) -> Handler App App ()
    renderErrorPage m = m >>=
       (either (errorPageWithTitle "Reset Password") return)

    loadUser u =
      (lift $ registrationStory $ S.loadUser u) >>=
        (either (throwError . show) return)

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . fromString . name $ param
  return (reqParam >>= decode param . B.unpack)

-- * Helpers

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False