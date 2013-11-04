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
  user <- runStoryE currentUser
  renderDynamicPagelet $ withUserFrame s (profileContent user)

changeUserDetails :: POSTContentHandler
changeUserDetails = ChangeUserDetails
  <$> getParameter regEmailPrm
  <*> getParameter regFullNamePrm
  <*> getParameter userTimeZonePrm

profileContent :: User -> Pagelet
profileContent user = onlyHtml $ mkI18NHtml $ \i -> do
  postForm (routeOf Profile) $ do
    table (fieldName profileTable) (fieldName profileTable) # centerTable $ do
      tableLine (i "Email address: ") $ textInput (B.name regEmailPrm) 20 (emailCata Just $ u_email user) ! A.required ""
      tableLine (i "Full name: ") $ textInput (B.name regFullNamePrm) 20 (Just . u_name $ user) ! A.required ""
      tableLine (i "Time zone: ") $ defEnumSelection (B.name userTimeZonePrm) (u_timezone user) ! A.required ""
    submitButton (fieldName changeProfileBtn) (i "Save")
  postForm (routeOf ChangePassword) `withId` (rFormId changePwdForm) $ do
    table (fieldName changePasswordTable) (fieldName changePasswordTable) # centerTable $ do
      tableLine (i "Old Password: ") $ passwordInput (B.name oldPasswordPrm) 20 Nothing ! A.required ""
      tableLine (i "New Password: ") $ passwordInput (B.name newPasswordPrm) 20 Nothing ! A.required ""
      tableLine (i "New Password Again: ") $ passwordInput (B.name newPasswordAgainPrm) 20 Nothing ! A.required ""
    submitButton (fieldName changePasswordBtn) (i "Change")

changePassword :: Content
changePassword = postContentHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return . StatusMessage $ "Password has been changed."
