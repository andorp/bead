{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Profile (
    profile, changePassword
  ) where

import Control.Monad (liftM)
import Data.String

import Snap.Snaplet.Auth hiding (currentUser)
import Text.Blaze.Html5 (Html,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Controller.Pages (Page(..))
import Bead.Controller.ServiceContext
import Bead.Controller.UserStories (currentUser)
import Bead.Domain.Entities
import Bead.View.Snap.Content
import qualified Bead.View.Snap.DataBridge as B
import Bead.View.Snap.HandlerUtils (userState)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.ResetPassword
import Bead.View.Snap.Style
import Bead.View.Snap.Session (convertPassword)

profile :: Content
profile = getPostContentHandler profilePage changeUserDetails

profilePage :: GETContentHandler
profilePage = withUserState $ \s -> do
  user <- userStory currentUser
  renderDynamicPagelet $ withUserFrame s (profileContent user)

changeUserDetails :: POSTContentHandler
changeUserDetails = ChangeUserDetails
  <$> getParameter regFullNamePrm
  <*> getParameter userTimeZonePrm

profileContent :: User -> Pagelet
profileContent user = onlyHtml $ mkI18NHtml $ \i -> do
  postForm (routeOf Profile) $ do
    table (fieldName profileTable) (fieldName profileTable) $ do
      tableLine (i "Felhasználó: ") (usernameCata (H.small . H.b . fromString) $ u_username user)
      tableLine (i "Email cím: ") (emailCata (H.small . H.b . fromString) $ u_email user)
      tableLine (i "Teljes név: ") $ textInput (B.name regFullNamePrm) 20 (Just . u_name $ user) ! A.required ""
      tableLine (i "Időzóna: ") $ defEnumSelection (B.name userTimeZonePrm) (u_timezone user) ! A.required ""
    submitButton (fieldName changeProfileBtn) (i "Mentés")
  H.br
  postForm (routeOf ChangePassword) `withId` (rFormId changePwdForm) $ do
    table (fieldName changePasswordTable) (fieldName changePasswordTable) $ do
      tableLine (i "Régi jelszó: ") $ passwordInput (B.name oldPasswordPrm) 20 Nothing ! A.required ""
      tableLine (i "Új jelszó: ") $ passwordInput (B.name newPasswordPrm) 20 Nothing ! A.required ""
      tableLine (i "Új jelszó (ismét): ") $ passwordInput (B.name newPasswordAgainPrm) 20 Nothing ! A.required ""
    submitButton (fieldName changePasswordBtn) (i "Csere")

changePassword :: Content
changePassword = postContentHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return . StatusMessage $ "A jelszó megváltozott!"
