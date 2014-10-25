{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserDetails.Page (
    userDetails
  ) where

import           Control.Arrow ((&&&))
import           Data.String (fromString)

import           Text.Blaze.Html5 hiding (map)

import           Bead.Controller.UserStories (loadUser, doesUserExist)
import qualified Bead.Controller.Pages as Pages
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import qualified Bead.View.Snap.DataBridge as B
import           Bead.View.Snap.Dictionary

userDetails = ViewModifyHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  exist    <- userStory $ doesUserExist username
  let render p = renderBootstrapPage $ bootStrapUserFrame s p
  case exist of
    True -> do
      user      <- userStory $ loadUser username
      languages <- getDictionaryInfos
      ts <- lift foundTimeZones
      render (userDetailForm ts user languages)

    False -> render (userDoesNotExist username)

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

userDetailForm :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
userDetailForm timeZones user dictionaries = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ hr
    Bootstrap.rowColMd12 $ Bootstrap.pageHeader $ h2 $
      fromString $ msg $ Msg_LinkText_UserDetails "User Details"
    postForm (routeOf userDetails) $ do
      --tableLine (msg $ Msg_Input_User_Role "Role")  $ required $ i18n msg $ inputPagelet (Just $ u_role u)
      Bootstrap.selection (fieldName userRoleField) (== u_role user) (roles msg)
      Bootstrap.textInputWithDefault (fieldName userEmailField) (msg $ Msg_Input_User_Email "Email") (emailCata id $ u_email user)
      Bootstrap.textInputWithDefault (fieldName userFamilyNameField) (msg $ Msg_Input_User_FullName "Full name") (u_name user)
      Bootstrap.selection (B.name userTimeZonePrm) (== u_timezone user) userTimeZones
      Bootstrap.selection (B.name userLanguagePrm) (== u_language user) languages
      hiddenInput (fieldName usernameField) . usernameCata id $ u_username user
      Bootstrap.submitButton (fieldName saveChangesBtn) (msg $ Msg_UserDetails_SaveButton "Update")
    Bootstrap.turnSelectionsOn
  where
    userTimeZones = map (id &&& timeZoneName id) timeZones
    userDetails = Pages.userDetails ()
    langVal (lang,info) = (lang, languageName info)
    languages = map langVal dictionaries

    roleLabel = roleCata
      (Msg_InputHandlers_Role_Student "Student")
      (Msg_InputHandlers_Role_GroupAdmin "Teacher")
      (Msg_InputHandlers_Role_CourseAdmin "Course Admin")
      (Msg_InputHandlers_Role_Admin "Administrator")

    allRoles = [toEnum 0 ..]
    roles msg = map (id &&& (msg . roleLabel)) allRoles

{-
    msg <- getI18N
    return . maybe
      (selection (fieldName userRoleField))
      (selectionWithDefault (fieldName userRoleField)) q $ (roles msg)
    where
      allRoles = [toEnum 0 ..]
      roles msg = map (id &&& (msg . roleLabel)) allRoles

      roleLabel = roleCata
        (Msg_InputHandlers_Role_Student "Student")
        (Msg_InputHandlers_Role_GroupAdmin "Teacher")
        (Msg_InputHandlers_Role_CourseAdmin "Course Admin")
        (Msg_InputHandlers_Role_Admin "Administrator")

-}

{-
userDetailForm :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
userDetailForm ts u languages = do
  msg <- getI18N
  return $ postForm (routeOf userDetails) $ do
    table "user-detail-table" "user-detail-table" $ do
      tableLine (msg $ Msg_Input_User_Role "Role")  $ required $ i18n msg $ inputPagelet (Just $ u_role u)
      tableLine (msg $ Msg_Input_User_Email "Email") $ required $ textInput (fieldName userEmailField) 20 (Just . emailCata id $ u_email u)
      tableLine (msg $ Msg_Input_User_FullName "Full name") $ required $ textInput (fieldName userFamilyNameField) 20 (Just $ u_name u)
      tableLine (msg $ Msg_Input_User_TimeZone "Time zone") $ required $
        selectionWithDefault (B.name userTimeZonePrm) (u_timezone u) userTimeZones
      tableLine (msg $ Msg_Input_User_Language "Language") $ required $
        selectionWithDefault' (B.name userLanguagePrm) ((u_language u)==) (map langVal languages)
      hiddenTableLine . hiddenInput (fieldName usernameField) . usernameCata id $ u_username u
    submitButton (fieldName saveChangesBtn) (msg $ Msg_UserDetails_SaveButton "Update")
  where
    userTimeZones = map (id &&& timeZoneName id) ts

    userDetails = Pages.userDetails ()

    langVal (lang,info) = (lang, languageName info)

userDoesNotExist :: Username -> IHtml
userDoesNotExist username = do
  msg <- getI18N
  return $ H.p $ do
    (fromString $ msg $ Msg_UserDetails_NonExistingUser "No such user:")
    usernameCata fromString username
-}

userDoesNotExist :: Username -> IHtml
userDoesNotExist username = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ hr
    Bootstrap.rowColMd12 $ Bootstrap.pageHeader $ h2 $
      fromString $ msg $ Msg_LinkText_UserDetails "User Details"
    Bootstrap.rowColMd12 $ p $ do
      (fromString $ msg $ Msg_UserDetails_NonExistingUser "No such user:")
      usernameCata fromString username
