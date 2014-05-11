{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.ResetPassword (
    resetPasswordPage
  , setUserPassword
  , updateCurrentAuthPassword
  , checkCurrentAuthPassword
  , encryptPwd
  , loadAuthUser
  ) where

import           Control.Monad.Trans.Error
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.String

import           Snap
import           Snap.Snaplet.Auth as A
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5  as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Domain.Entities
import qualified Bead.Controller.UserStories as S
import           Bead.View.Snap.Application
import           Bead.View.Snap.Content hiding (name)
import           Bead.View.Snap.DataBridge
import           Bead.View.Snap.ErrorPage
import           Bead.View.Snap.EmailTemplate (ForgottenPassword(..))
import           Bead.View.Snap.HandlerUtils (registrationStory, renderPublicPage)

backToLogin :: Translation String
backToLogin = Msg_ResetPassword_GoBackToLogin "Back to login"

resetPasswordTitle :: Translation String
resetPasswordTitle = Msg_ResetPassword_ForgottenPassword "Forgotten password"

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
setUserPassword :: (Error e) => Username -> String -> ErrorT e (Handler App a) (Translation String)
setUserPassword u password = do
  let username = usernameCata id u
  authUser <- getAuthUser u
  case authUser of
    Nothing -> return $
      Msg_ResetPassword_UserDoesNotExist "No such user."
    Just user -> do
      encryptedPwd <- encryptPwd password
      updateUser user { userPassword = Just encryptedPwd }
      emailPasswordToUser u password
      return $
        Msg_ResetPassword_PasswordIsSet "The password has been set."

emailPasswordToUser :: (Error e) => Username -> String -> ErrorT e (Handler App a) ()
emailPasswordToUser user pwd = do
  msg <- lift i18nH
  address <- fmap u_email (loadUserFromPersistence msg)
  lift $ withTop sendEmailContext $ do
    sendEmail
      address
      (msg $ Msg_ResetPassword_EmailSubject "BE-AD: Forgotten password")
      (msg $ Msg_ResetPassword_EmailBody forgottenPasswordEmailTemplate)
      ForgottenPassword { fpUsername = usernameCata id user, fpNewPassword = pwd }
  where
    loadUserFromPersistence i18n =
      (lift $ registrationStory $ S.loadUser user) >>=
      (either (throwError . strMsg . S.translateUserError i18n) return)

-- Universal error message for every type of error
-- in such case the attacker could deduce minimal
-- amount of information
errorMsg = Msg_ResetPassword_GenericError "Invalid username or password."

checkUserInAuth :: (Error e) => Username -> ErrorT e (Handler App a) ()
checkUserInAuth u = do
  msg <- lift i18nH
  exist <- lift . withTop auth $ usernameExists (usernameStr u)
  unless exist $ throwError . strMsg $ msg errorMsg

checkUserInPersistence :: (Error e) => Username -> ErrorT e (Handler App a) ()
checkUserInPersistence u = do
  msg <- lift i18nH
  x <- lift $ registrationStory $ S.doesUserExist u
  either (throwError . strMsg . S.translateUserError msg)
         (\e -> unless e $ (throwError . strMsg . msg $ errorMsg)) x

usernameStr :: (IsString s) => Username -> s
usernameStr = usernameCata fromString

getAuthUser :: (Error e) => Username -> ErrorT e (Handler App a) (Maybe AuthUser)
getAuthUser u =
  lift . withTop auth $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameStr u)

loadAuthUser :: (Error e) => Username -> ErrorT e (Handler App a) AuthUser
loadAuthUser u = do
  msg <- lift i18nH
  usr <- getAuthUser u
  when (isNothing usr) $ throwError . strMsg $ msg errorMsg
  return . fromJust $ usr

updateUser :: (Error e) => AuthUser -> ErrorT e (Handler App a) AuthUser
updateUser usr =
  (lift $ withTop auth $ withBackend $ \r -> liftIO $ save r usr) >>=
  either (throwError . strMsg . show) return

encryptPwd :: (Error e) => String -> ErrorT e (Handler App a) A.Password
encryptPwd = liftIO . encryptPassword . ClearText . fromString

-- TODO: I18N
-- Check if the current auth password is the same as the given one
-- If they are different an error is thrown.
checkCurrentAuthPassword :: (Error e) => String -> ErrorT e (Handler App a) ()
checkCurrentAuthPassword pwd = do
  msg <- lift i18nH
  name <- user <$> userState
  result <- lift $ withTop auth $
    loginByUsername (usernameCata fromString name) (ClearText $ fromString pwd) False
  when (isLeft result) . throwError . strMsg . msg $
    Msg_ResetPassword_InvalidPassword "Invalid password."

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
    renderForm = renderPublicPage . dynamicTitleAndHead resetPasswordTitle $ do
      msg <- getI18N
      return $ do
        postForm "/reset_pwd" $ do
          table (fieldName resetPasswordTable) (fieldName resetPasswordTable) # centerTable $ do
            tableLine (msg $ Msg_ResetPassword_Username "Username:") $ textInput (name regUsernamePrm) 20 Nothing ! A.required ""
            tableLine (msg $ Msg_ResetPassword_Email "Email") $ textInput (name regEmailPrm) 20 Nothing ! A.required ""
          submitButton (fieldName pwdSubmitBtn) (msg $ Msg_ResetPassword_NewPwdButton "New password")
        linkToRoute (msg backToLogin)


{- Reset password POST handler
Reads out the parameters for the username and the email address, checks
if the user exist in the persistence layer with the given email address.
If the user is not exist or the given address differs, the error page is rendered.
-}
resetPasswordPOST :: Handler App App ()
resetPasswordPOST = renderErrorPage $ runErrorT $ do
  u <- readParameter regUsernamePrm
  e <- readParameter regEmailPrm
  msg <- lift i18nH
  case (u,e) of
    (Just username, Just email) -> do
      checkUserInAuth username
      checkUserInPersistence username
      user <- loadUser msg username
      when (email /= (u_email user)) $ throwError . strMsg . msg $ errorMsg
      resetPassword username
      lift pageContent
    _ -> throwError . strMsg . msg $ errorMsg
  where
    renderErrorPage :: Handler App App (Either String ()) -> Handler App App ()
    renderErrorPage m = m >>=
       (either (errorPage resetPasswordTitle) return)

    loadUser i18n u =
      (lift $ registrationStory $ S.loadUser u) >>=
        (either (throwError . strMsg . S.translateUserError i18n) return)

pageContent :: (Handler App a) ()
pageContent = renderPublicPage . dynamicTitleAndHead resetPasswordTitle $ do
  msg <- getI18N
  return $ do
    H.p . fromString . msg $ Msg_ResetPassword_EmailSent $ "The new password has been sent in email, it shall arrive soon."
    H.br
    linkToRoute (msg backToLogin)

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . fromString . name $ param
  return (reqParam >>= decode param . B.unpack)

-- * Helpers

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- * Email Template

forgottenPasswordEmailTemplate :: String
forgottenPasswordEmailTemplate = unlines
  [ "Dear {{fpUsername}},"
  , ""
  , "You have requested resetting your password, hence we have now generated"
  , "(and set) you a new password, which is as follows:"
  , ""
  , "    {{fpNewPassword}}"
  , ""
  , "Please, use this password to login and change it as soon as possible."
  , ""
  , "Cheers,"
  , "The Administrators"
  ]
