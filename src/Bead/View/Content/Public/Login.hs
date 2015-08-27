{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Content.Public.Login (
    login
  ) where

import           Data.String (fromString)
import           Control.Monad

import           Snap.Snaplet.Auth

import qualified Bead.Controller.Pages as Pages
#ifndef SSO
import           Bead.View.Common
#endif
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RouteOf

import           Text.Blaze.Html5
#ifndef SSO
import           Text.Blaze.Html5.Attributes
#endif

#ifdef SSO
login :: Maybe AuthFailure -> IHtml
login (Just err) = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ do
      p $ fromString $
        join [msg $ msg_Login_Error "There was an error during login: ", show err]
    Bootstrap.rowCol4Offset4 $
      Bootstrap.buttonLink (unpack loginPath) (msg $ msg_Login_TryAgain "Try again")

login Nothing = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ do
      p $ fromString $
        msg $ msg_Login_On_SSO "This page should not be accessible in single sign-on mode.  Please report this issue to the administrators."
#else
login :: Maybe AuthFailure -> DictionaryInfos -> IHtml
login err langInfos = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ postForm (routeOf login) $ do
      Bootstrap.textInput     (fieldName loginUsername) (msg $ msg_Login_Username "Username:") ""
      Bootstrap.passwordInput (fieldName loginPassword) (msg $ msg_Login_Password "Password:")
      Bootstrap.submitButton  (fieldName loginSubmitBtn) (msg $ msg_Login_Submit "Login")
    maybe mempty (Bootstrap.rowCol4Offset4 . (p ! class_ "text-center bg-danger") . fromString . show) err
    Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $ do
      Bootstrap.buttonLink "/reg_request" (msg $ msg_Login_Registration "Registration")
      Bootstrap.buttonLink "/reset_pwd"   (msg $ msg_Login_Forgotten_Password "Forgotten password")
    languageMenu msg langInfos
  where
    login = Pages.login ()
#endif
