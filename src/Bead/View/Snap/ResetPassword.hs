module Bead.View.Snap.ResetPassword where

import Control.Monad.Trans.Error
import Data.String
import Data.Maybe

import Snap
import Snap.Snaplet.Auth

import Bead.Domain.Entities
import Bead.Controller.UserStories as S
import Bead.View.Snap.Application
import Bead.View.Snap.HandlerUtils (registrationStory)
import Bead.View.Snap.Session (passwordFromAuthUser)

-- Generates a new random password for the given user
-- and saves it to the persistence layer and the authentication
-- and sends the email to the given user.
-- The handler calculates unit if everything went fine, otherwise a string
-- indicating the reason.
resetPassword :: Username -> Handler App a (Either String ())
resetPassword u = runErrorT $ do
  checkUserInAuth
  checkUserInPersistence
  password <- randomPassword
  user <- loadUser
  updateUser user { userPassword = Just . ClearText . fromString $ password }
  resetPasswordInPersistence
  where
    username :: (IsString s) => Username -> s
    username = usernameFold fromString

    checkUserInAuth = do
      exist    <- lift . withTop auth $ usernameExists (username u)
      unless exist . throwError $ "User does not exist: " ++ (username u)

    checkUserInPersistence =
      (lift $ registrationStory $ doesUserExist u) >>=
      either (throwError . show)
             (\e -> unless e $ throwError "User is not in persistence")

    randomPassword = lift . withTop randomPasswordContext $ getRandomPassword

    loadUser = do
      usr <- lift . withTop auth $ withBackend $ \r -> liftIO $ lookupByLogin r (username u)
      when (isNothing usr) $ throwError "User is not in the authentication"
      return . fromJust $ usr

    updateUser usr =
      (lift $ withTop auth $ withBackend $ \r -> liftIO $ save r usr) >>=
      either (throwError . show) return

    resetPasswordInPersistence = do
      user <- loadUser
      pwd <- maybe
         (throwError "No password was saved for the user")
         return
         (passwordFromAuthUser user)
      (lift $ registrationStory $ S.resetPassword u pwd) >>=
        (either (throwError . show) return)
