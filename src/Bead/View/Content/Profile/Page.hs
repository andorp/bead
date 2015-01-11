{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Profile.Page (
    profile
  , changePassword
  ) where

import           Control.Arrow ((&&&))
import           Data.String

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (currentUser)
import           Bead.Domain.Entities hiding (name)
import           Bead.View.Content hiding (name, option)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.DataBridge as B
import           Bead.View.Dictionary
import           Bead.View.Session (setLanguageInSession)

#ifdef LDAPEnabled
#else
import           Bead.View.ResetPassword
#endif

profile = ViewModifyHandler profilePage changeUserDetails

profilePage :: GETContentHandler
profilePage = do
  user <- userStory currentUser
  languages <- getDictionaryInfos
  ts <- lift foundTimeZones
  return $ profileContent ts user languages

changeUserDetails :: POSTContentHandler
changeUserDetails = do
  language <- getParameter userLanguagePrm
  setLanguage language
  ChangeUserDetails
    <$> getParameter regFullNamePrm
    <*> getParameter userTimeZonePrm
    <*> (return language)
  where
    setLanguage = lift . withTop sessionManager . setLanguageInSession

#ifdef LDAPEnabled
changePassword = ModifyHandler $ do
  return $ LogMessage "LDAP authentication can not change password from Profile page."
#else
changePassword = ModifyHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return . StatusMessage $ Msg_Profile_PasswordHasBeenChanged "The password has been changed."
#endif

profileContent :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
profileContent ts user ls = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ do
      -- User Details
      userDetailsCol $ postForm (routeOf profile) $ do
        profileFields msg
        Bootstrap.submitButton (fieldName changeProfileBtn) (msg $ Msg_Profile_SaveButton "Save")
      passwordSection msg
      Bootstrap.turnSelectionsOn
  where
    regFullNameField  = fromString $ B.name regFullNamePrm
    userLanguageField = fromString $ B.name userLanguagePrm
    userTimeZoneField = fromString $ B.name userTimeZonePrm
    fullName          = fromString $ u_name user

    userDetailsCol =
#ifdef LDAPEnabled
      Bootstrap.colMd12
#else
      Bootstrap.colMd6
#endif

    passwordSection msg = do
#ifdef LDAPEnabled
      return ()
#else
      let oldPasswordField = fromString $ B.name oldPasswordPrm
      let newPasswordField = fromString $ B.name newPasswordPrm
      let newPasswordAgain = fromString $ B.name newPasswordAgainPrm
      Bootstrap.colMd6 $ postForm (routeOf changePassword) `withId` (rFormId changePwdForm) $ do
        Bootstrap.passwordInput oldPasswordField (msg $ Msg_Profile_OldPassword "Old password: ")
        Bootstrap.passwordInput newPasswordField (msg $ Msg_Profile_NewPassword "New password: ")
        Bootstrap.passwordInput newPasswordAgain (msg $ Msg_Profile_NewPasswordAgain "New password (again): ")
        Bootstrap.submitButton (fieldName changePasswordBtn) (msg $ Msg_Profile_ChangePwdButton "Update")
#endif

    profileFields msg = do
#ifdef LDAPEnabled
      Bootstrap.labeledText (msg $ Msg_Profile_User "Username: ") (usernameCata fromString $ u_username user)
      Bootstrap.labeledText (msg $ Msg_Profile_Email "Email: ") (emailCata fromString $ u_email user)
      Bootstrap.labeledText (msg $ Msg_Profile_FullName "Full name: ") fullName
      hiddenInput regFullNameField (u_name user)
      Bootstrap.selection userLanguageField (== u_language user) languages
      Bootstrap.selection userTimeZoneField (== u_timezone user) timeZones
#else
      Bootstrap.labeledText (msg $ Msg_Profile_User "Username: ") (usernameCata fromString $ u_username user)
      Bootstrap.labeledText (msg $ Msg_Profile_Email "Email: ") (emailCata fromString $ u_email user)
      Bootstrap.textInputWithDefault regFullNameField (msg $ Msg_Profile_FullName "Full name: ") fullName
      Bootstrap.selection userLanguageField (== u_language user) languages
      Bootstrap.selection userTimeZoneField (== u_timezone user) timeZones
#endif

    timeZones = map (Prelude.id &&& timeZoneName Prelude.id) ts
    languages = map langValue ls
    profile = Pages.profile ()
    changePassword = Pages.changePassword ()

    langValue (lang,info)  = (lang, languageName info)
