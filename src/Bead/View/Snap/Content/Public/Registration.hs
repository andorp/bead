{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Public.Registration (
    registrationFirstStep
  , registrationFirstStepEmailSent
  , registrationPasswordStep
  ) where

import           Data.String (fromString)

import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)
import           Text.Blaze.Html5 as H

import           Bead.View.Snap.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import           Bead.View.Snap.DataBridge as DataBridge

backToLogin msg =
  Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $
    Bootstrap.buttonLink "/" $ msg $ Msg_Registration_GoBackToLogin "Back to login"

registrationTitle msg =
  Bootstrap.rowCol4Offset4 $ h2 $
    fromString $ msg $ Msg_Registration_Title "Registration"

registrationFirstStep :: IHtml
registrationFirstStep = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_request" ! (A.id . formId $ regForm) $ do
      Bootstrap.textInput (DataBridge.name regUsernamePrm) (msg $ Msg_Registration_Username "Username:") ""
      Bootstrap.textInput (DataBridge.name regEmailPrm)    (msg $ Msg_Registration_Email "Email:") ""
      Bootstrap.textInput (DataBridge.name regFullNamePrm) (msg $ Msg_Registration_FullName "Full name:") ""
      Bootstrap.submitButton (fieldName regSubmitBtn) (msg $ Msg_Registration_SubmitButton "Registration")
    backToLogin msg

registrationFirstStepEmailSent :: IHtml
registrationFirstStepEmailSent = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ p $
      fromString $ msg $ Msg_RegistrationTokenSend_Title "The registration token has been sent in email, it shall arrive soon."
    backToLogin msg

-- The second step f the registration
-- registrationPasswordStep :: IHtml
registrationPasswordStep utcZoneInfo timeZones key language username token = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_final" ! (A.id . formId $ regFinalForm) $ do
      Bootstrap.passwordInput (DataBridge.name regPasswordPrm)      (msg $ Msg_RegistrationFinalize_Password "Password:")
      Bootstrap.passwordInput (DataBridge.name regPasswordAgainPrm) (msg $ Msg_RegistrationFinalize_PwdAgain "Password (again):")
      Bootstrap.selectionWithLabel
        (DataBridge.name regTimeZonePrm)
        (msg $ Msg_RegistrationFinalize_Timezone "Time zone:")
        (==utcZoneInfo)
        timeZones
      hiddenParam regUserRegKeyPrm key
      hiddenParam regTokenPrm      token
      hiddenParam regUsernamePrm   username
      hiddenParam regLanguagePrm   language
      Bootstrap.submitButton (fieldName regSubmitBtn) (msg $ Msg_RegistrationFinalize_SubmitButton "Register")
    backToLogin msg
    Bootstrap.turnSelectionsOn
  where
    hiddenParam parameter value = hiddenInput (DataBridge.name parameter) (DataBridge.encode parameter value)
