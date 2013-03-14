{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserDetails (
    userDetails
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Email(..), roles)
import Bead.Controller.UserStories (loadUser)
import Bead.Controller.Pages as P (Page(UserDetails))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)

userDetails :: Content
userDetails = getContentHandler userDetailPage

userDetailPage :: GETContentHandler
userDetailPage = withUserStateE $ \s -> do
  username <- getParamE (fieldName usernameField) Username "Username is not found"
  user     <- runStoryE . loadUser $ username
  blaze $ withUserFrame s (userDetailForm user) Nothing

userDataChange :: POSTContentHandler
userDataChange = undefined

userDetailForm :: User -> Html
userDetailForm u = do
  postForm (routeOf P.UserDetails) $ do
    table "user-detail-table" $ do
      tableLine "User's role"  $ selection (fieldName userRoleField) $ mapM_ roleOptions roles
      tableLine "User's email" $ textInput (fieldName userEmailField) 20 (defaultValue . str . u_email $ u)
      tableLine "User's familyname" $ textInput (fieldName userFamilyNameField) 20 (defaultValue . u_name $ u)
    submitButton "Save changes"
  where
    roleOptions r = option (show r) (show r) (u_role u == r)
