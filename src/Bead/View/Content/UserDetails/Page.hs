{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.UserDetails.Page (
    userDetails
  ) where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import           Data.Maybe
import           Data.String (fromString)

import           Text.Blaze.Html5 hiding (map)

import           Bead.Controller.UserStories (loadUser, doesUserExist)
import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.DataBridge as B
import           Bead.View.Dictionary

userDetails = ViewModifyHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = do
  username <- getParameter usernamePrm
  exist    <- userStory $ doesUserExist username
  page <- case exist of
    True -> do
      user      <- userStory $ loadUser username
      languages <- getDictionaryInfos
      ts <- lift foundTimeZones
      return (userDetailForm ts user languages)

    False -> return (userDoesNotExist username)
  return page

userDataChange :: POSTContentHandler
userDataChange = do
  user <- User
    <$> getRole
    <*> getUsername
    <*> getParameter userEmailPrm
    <*> getParameter (stringParameter (fieldName userFamilyNameField) "Teljes név")
    <*> getParameter userTimeZonePrm
    <*> getParameter userLanguagePrm
  return $ UpdateUser user

userDetailForm :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
userDetailForm timeZones user dictionaries = do
  msg <- getI18N
  return $ do
    postForm (routeOf userDetails) $ do
      Bootstrap.labeledText "" (usernameCata fromString $ u_username user)
      userDetailsFields msg
      Bootstrap.submitButton (fieldName saveChangesBtn) (msg $ Msg_UserDetails_SaveButton "Update")
    Bootstrap.turnSelectionsOn
  where
    userDetailsFields msg = do
#ifdef LDAPEnabled
      Bootstrap.selection (fieldName userRoleField) (== u_role user) (roles msg)
      emailCata (Bootstrap.labeledText (msg $ Msg_Input_User_Email "Email") . fromString) (u_email user)
      hiddenInput (fieldName userEmailField) (emailCata id $ u_email user)
      Bootstrap.labeledText (msg $ Msg_Input_User_FullName "Full name") $ fromString (u_name user)
      hiddenInput (fieldName userFamilyNameField) (u_name user)
      timeZoneName (Bootstrap.labeledText (msg $ Msg_Input_User_TimeZone "Timezone") . fromString) (u_timezone user)
      hiddenInput (B.name userTimeZonePrm) (Bootstrap.encode "timezone" $ u_timezone user)
      Bootstrap.labeledText
        (msg $ Msg_Input_User_Language "Language")
        (fromString $ fromMaybe "Language is not found" $ Map.lookup (u_language user) languageMap)
      hiddenInput (B.name userLanguagePrm) (Bootstrap.encode "language" $ u_language user)
      hiddenInput (fieldName usernameField) . usernameCata id $ u_username user
#else
      Bootstrap.selection (fieldName userRoleField) (== u_role user) (roles msg)
      Bootstrap.textInputWithDefault (fieldName userEmailField) (msg $ Msg_Input_User_Email "Email") (emailCata id $ u_email user)
      Bootstrap.textInputWithDefault (fieldName userFamilyNameField) (msg $ Msg_Input_User_FullName "Full name") (u_name user)
      Bootstrap.selection (B.name userTimeZonePrm) (== u_timezone user) userTimeZones
      Bootstrap.selection (B.name userLanguagePrm) (== u_language user) languages
      hiddenInput (fieldName usernameField) . usernameCata id $ u_username user
#endif
    userTimeZones = map (id &&& timeZoneName id) timeZones
    userDetails = Pages.userDetails ()
    langVal (lang,info) = (lang, languageName info)
    languageMap = Map.fromList languages
    languages = map langVal dictionaries

    roleLabel = roleCata
      (Msg_InputHandlers_Role_Student "Student")
      (Msg_InputHandlers_Role_GroupAdmin "Teacher")
      (Msg_InputHandlers_Role_CourseAdmin "Course Admin")
      (Msg_InputHandlers_Role_Admin "Administrator")

    allRoles = [toEnum 0 ..]
    roles msg = map (id &&& (msg . roleLabel)) allRoles

userDoesNotExist :: Username -> IHtml
userDoesNotExist username = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $ do
      (fromString $ msg $ Msg_UserDetails_NonExistingUser "No such user:")
      usernameCata fromString username

getRole = getParameter rolePrm

getUsername = getParameter usernamePrm

