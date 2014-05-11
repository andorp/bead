{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Profile (
    profile
  , changePassword
  ) where

import           Control.Arrow ((&&&))
import           Data.String

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (currentUser)
import           Bead.Domain.Entities
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.DataBridge as B
import           Bead.View.Snap.Dictionary
import           Bead.View.Snap.ResetPassword
import           Bead.View.Snap.Session (setLanguageInSession)


profile = ViewModifyHandler profilePage changeUserDetails

profilePage :: GETContentHandler
profilePage = withUserState $ \s -> do
  user <- userStory currentUser
  languages <- getDictionaryInfos
  renderDynamicPagelet $ withUserFrame s (profileContent user languages)

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

profileContent :: User -> DictionaryInfos -> IHtml
profileContent user languages = do
  msg <- getI18N
  return $ do
    postForm (routeOf profile) $ do
      table (fieldName profileTable) (fieldName profileTable) $ do
        tableLine (msg $ Msg_Profile_User "Username: ") (usernameCata (H.small . H.b . fromString) $ u_username user)
        tableLine (msg $ Msg_Profile_Email "Email: ") (emailCata (H.small . H.b . fromString) $ u_email user)
        tableLine (msg $ Msg_Profile_FullName "Full name: ") $ textInput (B.name regFullNamePrm) 20 (Just . u_name $ user) ! A.required ""
        tableLine (msg $ Msg_Profile_Timezone "Time zone: ") $ selectionWithDefault (B.name userTimeZonePrm) (u_timezone user) timeZones ! A.required ""

        tableLine (msg $ Msg_Profile_Language "Language: ") $
          selectionWithDefault (B.name userLanguagePrm) (u_language user) languages' ! A.required ""
      submitButton (fieldName changeProfileBtn) (msg $ Msg_Profile_SaveButton "Save")
    H.br
    postForm (routeOf changePassword) `withId` (rFormId changePwdForm) $ do
      table (fieldName changePasswordTable) (fieldName changePasswordTable) $ do
        tableLine (msg $ Msg_Profile_OldPassword "Old password: ") $ passwordInput (B.name oldPasswordPrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_Profile_NewPassword "New password: ") $ passwordInput (B.name newPasswordPrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_Profile_NewPasswordAgain "New password (again): ") $ passwordInput (B.name newPasswordAgainPrm) 20 Nothing ! A.required ""
      submitButton (fieldName changePasswordBtn) (msg $ Msg_Profile_ChangePwdButton "Update")
  where
    timeZones = map (id &&& show) [toEnum 0 .. ]
    languages' = map langValue languages
    profile = Pages.profile ()
    changePassword = Pages.changePassword ()

    langValue (lang,info)  = (lang, languageName info)

changePassword = ModifyHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return . StatusMessage $ Msg_Profile_PasswordHasBeenChanged "The password has been changed."
