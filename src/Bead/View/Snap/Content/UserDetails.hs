{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserDetails (
    userDetails
  ) where

import           Control.Arrow ((&&&))
import           Data.String (fromString)

import qualified Text.Blaze.Html5 as H

import           Bead.Controller.UserStories (loadUser, doesUserExist)
import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Types (Str(..))
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.DataBridge as B
import           Bead.View.Snap.Dictionary

userDetails = ViewModifyHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  exist    <- userStory $ doesUserExist username
  case exist of
    True -> do
      user      <- userStory $ loadUser username
      languages <- getDictionaryInfos
      renderPagelet $ withUserFrame s (userDetailForm user languages)

    False -> renderPagelet $ withUserFrame s (userDoesNotExist username)

userDataChange :: POSTContentHandler
userDataChange = do
  user <- User
    <$> getValue -- role
    <*> getValue -- username
    <*> getParameter userEmailPrm
    <*> getParameter (stringParameter (fieldName userFamilyNameField) "Teljes név")
    <*> getParameter userTimeZonePrm
    <*> getParameter userLanguagePrm
  return $ UpdateUser user

userDetailForm :: User -> DictionaryInfos -> IHtml
userDetailForm u languages = do
  msg <- getI18N
  return $ postForm (routeOf userDetails) $ do
    table "user-detail-table" "user-detail-table" $ do
      tableLine (msg $ Msg_Input_User_Role "Role")  $ required $ i18n msg $ inputPagelet (Just $ u_role u)
      tableLine (msg $ Msg_Input_User_Email "Email") $ required $ textInput (fieldName userEmailField) 20 (Just . str $ u_email u)
      tableLine (msg $ Msg_Input_User_FullName "Full name") $ required $ textInput (fieldName userFamilyNameField) 20 (Just $ u_name u)
      tableLine (msg $ Msg_Input_User_TimeZone "Time zone") $ required $
        selectionWithDefault (B.name userTimeZonePrm) (u_timezone u) userTimeZones
      tableLine (msg $ Msg_Input_User_Language "Language") $ required $
        selectionWithDefault' (B.name userLanguagePrm) ((u_language u)==) (map langVal languages)
      hiddenTableLine . hiddenInput (fieldName usernameField) . str . u_username $ u
    submitButton (fieldName saveChangesBtn) (msg $ Msg_UserDetails_SaveButton "Update")
  where
    userTimeZones :: [(TimeZone, String)]
    userTimeZones = map (id &&& show) $ [toEnum 0 .. ]

    userDetails = Pages.userDetails ()

    langVal (lang,info) = (lang, languageName info)

userDoesNotExist :: Username -> IHtml
userDoesNotExist username = do
  msg <- getI18N
  return $ H.p $ do
    (fromString $ msg $ Msg_UserDetails_NonExistingUser "No such user:")
    fromString . str $ username

