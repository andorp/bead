{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.ResetPassword (
    resetPasswordPage
  , setUserPassword
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

-- Generates a new random password for the given user. If the user does
-- not exist it thows an error
resetPassword :: (Error e) => Username -> ErrorT e (Handler App a) ()
resetPassword u = do
  user <- loadAuthUser u
  password <- randomPassword
  encryptedPwd <- encryptPwd password
  updateUser user { userPassword = Just encryptedPwd }
  emailPasswordToUser u password
  where
    randomPassword = lift . withTop randomPasswordContext $ getRandomPassword

-- Saves the users password it to the persistence layer and the authentication
-- and sends the email to the given user.
-- The handler returns a status message that should be displayed to the user.
setUserPassword :: (Error e) => Username -> String -> ErrorT e (Handler App a) String
setUserPassword u password = do
  let username = usernameCata id u
  authUser <- getAuthUser u
  case authUser of
    Nothing -> return $ concat ["The ", username, " does not exist."]
    Just user -> do
      encryptedPwd <- encryptPwd password
      updateUser user { userPassword = Just encryptedPwd }
      emailPasswordToUser u password
      return $ concat ["The password for ", username, " is set."]

emailPasswordToUser :: (Error e) => Username -> String -> ErrorT e (Handler App a) ()
emailPasswordToUser user pwd = do
  address <- fmap u_email loadUserFromPersistence
  lift $ withTop sendEmailContext $
    sendEmail
      address
      "BE-AD Password reset"
      ForgottenPassword { restoreUrl = pwd }
  where
    loadUserFromPersistence =
      (lift $ registrationStory $ S.loadUser user) >>=
      (either (throwError . strMsg . show) return)


-- Universal error message for every type of error
-- in such case the attacker could deduce minimal
-- amount of information
errorMsg :: (Error e) => e
errorMsg = strMsg "Invalid username and/or password"

checkUserInAuth :: (Error e) => Username -> ErrorT e (Handler App a) ()
checkUserInAuth u = do
  exist    <- lift . withTop auth $ usernameExists (usernameStr u)
  unless exist $ throwError errorMsg

checkUserInPersistence :: (Error e) => Username -> ErrorT e (Handler App a) ()
checkUserInPersistence u =
  (lift $ registrationStory $ S.doesUserExist u) >>=
  either (throwError . strMsg . show)
         (\e -> unless e $ throwError errorMsg)

usernameStr :: (IsString s) => Username -> s
usernameStr = usernameCata fromString

getAuthUser :: (Error e) => Username -> ErrorT e (Handler App a) (Maybe AuthUser)
getAuthUser u =
  lift . withTop auth $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameStr u)

loadAuthUser :: (Error e) => Username -> ErrorT e (Handler App a) AuthUser
loadAuthUser u = do
  usr <- getAuthUser u
  when (isNothing usr) $ throwError $ strMsg errorMsg
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
      linkToRoute "Go back to the login page"

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
      checkUserInAuth username
      checkUserInPersistence username
      user <- loadUser username
      when (email /= (u_email user)) $ throwError errorMsg
      resetPassword username
      lift pageContent
    _ -> throwError errorMsg
  where
    renderErrorPage :: Handler App App (Either String ()) -> Handler App App ()
    renderErrorPage m = m >>=
       (either (errorPageWithTitle "Reset Password") return)

    loadUser u =
      (lift $ registrationStory $ S.loadUser u) >>=
        (either (throwError . show) return)


pageContent :: (Handler App a) ()
pageContent = blaze $ dynamicTitleAndHead "Reset Password" $ do
  "Please check your emails"
  H.br
  linkToRoute "Go back to the login page"

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . fromString . name $ param
  return (reqParam >>= decode param . B.unpack)

-- * Helpers

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
