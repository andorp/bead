{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Profile.Page (
    profile
  , changePassword
  ) where

import           Control.Arrow ((&&&))
import           Data.String

import           Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H hiding (map)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (currentUser)
import           Bead.Domain.Entities hiding (name)
import           Bead.View.Snap.Content hiding (name, option)
import qualified Bead.View.Snap.DataBridge as B
import           Bead.View.Snap.Dictionary
import           Bead.View.Snap.ResetPassword
import           Bead.View.Snap.Session (setLanguageInSession)


profile = ViewModifyHandler profilePage changeUserDetails

profilePage :: GETContentHandler
profilePage = withUserState $ \s -> do
  user <- userStory currentUser
  languages <- getDictionaryInfos
  ts <- lift foundTimeZones
  renderBootstrapPage . bootstrapUserFrame s $ profileContent ts user languages

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

changePassword = ModifyHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return . StatusMessage $ Msg_Profile_PasswordHasBeenChanged "The password has been changed."

profileContent :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
profileContent ts user ls = do
  msg <- getI18N
  return $ do
        H.div ! class_ "row" $ do
            -- User Details
            let regFullNameField = fromString $ B.name regFullNamePrm
                userLanguageField = fromString $ B.name userLanguagePrm
                userTimeZoneField = fromString $ B.name userTimeZonePrm
                fullName = fromString $ u_name user
            H.div ! class_ "col-md-6" $
              postForm (routeOf profile) $ do
                H.div ! class_ "form-group" $ do
                    H.label $ fromString $ msg $ Msg_Profile_User "Username: "
                    H.span ! class_ "form-control" $ usernameCata fromString $ u_username user
                H.div ! class_ "form-group" $ do
                    H.label $ fromString $ msg $ Msg_Profile_Email "Email: "
                    H.span ! class_ "form-control" $ emailCata fromString $ u_email user
                H.div ! class_ "form-group" $ do
                    H.label ! for regFullNameField $ fromString $ msg $ Msg_Profile_FullName "Full name: "
                    input ! class_ "form-control" -- ! placeholder fullName
                          ! type_ "text" ! A.id regFullNameField ! A.name regFullNameField
                          ! value fullName
                -- Languages
                H.div ! class_ "form-group" $
                  selectionWithDefAndAttr
                    userLanguageField
                    [class_ "combobox form-control", A.style "display:none", A.required ""]
                    (== u_language user)
                        languages

                -- Timezones
                H.div ! class_ "form-group" $
                  selectionWithDefAndAttr
                    userTimeZoneField
                    [class_ "combobox form-control", A.style "display:none", A.required ""]
                    (== u_timezone user)
                    timeZones

                button ! type_ "submit"
                       ! (name . fromString $ fieldName changeProfileBtn)
                       ! class_ "btn btn-block btn-default"
                       $ fromString $ msg $ Msg_Profile_SaveButton "Save"

            -- Password Section
            let oldPasswordField = fromString $ B.name oldPasswordPrm
                newPasswordField = fromString $ B.name newPasswordPrm
                newPasswordAgain = fromString $ B.name newPasswordAgainPrm
            H.div ! class_ "col-md-6" $
              postForm (routeOf changePassword) `withId` (rFormId changePwdForm) $ do
                H.div ! class_ "form-group" $ do
                    H.label ! for oldPasswordField $ fromString $ msg $ Msg_Profile_OldPassword "Old password: "
                    input ! class_ "form-control" ! A.id oldPasswordField ! A.name oldPasswordField
                          ! placeholder "Password" ! type_ "password"
                H.div ! class_ "form-group" $ do
                    H.label ! for newPasswordField $ fromString $ msg $ Msg_Profile_NewPassword "New password: "
                    input ! class_ "form-control" ! A.id newPasswordField ! A.name newPasswordField
                          ! placeholder "Password" ! type_ "password"
                H.div ! class_ "form-group" $ do
                    H.label ! for newPasswordAgain $ fromString $ msg $ Msg_Profile_NewPasswordAgain "New password (again): "
                    input ! class_ "form-control" ! A.id newPasswordAgain ! A.name newPasswordAgain
                          ! placeholder "Password" ! type_ "password"
                button ! type_ "submit" ! name (fieldName changePasswordBtn) ! class_ "btn btn-block btn-default" $ 
                  fromString $ msg $ Msg_Profile_ChangePwdButton "Update"

        H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr
        script ! type_ "text/javascript" $ "//\n $(document).ready(function(){\n $('.combobox').combobox()\n });\n //"
  where
    timeZones = map (Prelude.id &&& timeZoneName Prelude.id) ts
    languages = map langValue ls
    profile = Pages.profile ()
    changePassword = Pages.changePassword ()

    langValue (lang,info)  = (lang, languageName info)
