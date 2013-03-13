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
  H.form ! A.method "post" ! A.action (routeOf P.UserDetails) $ do
    H.table ! A.id "user-detail-table" $ do
      H.tr $ do
        H.td $ "User's role"
        H.td $ selection (fieldName userRoleField) (u_role u) roles
      H.tr $ do
        H.td $ "User's email"
        H.td $ H.input ! A.type_ "text" ! A.name (fieldName userEmailField) ! A.value (fromString . str . u_email $ u) ! A.size "20"
      H.tr $ do
        H.td $ "User's familyname"
        H.td $ H.input ! A.type_ "text" ! A.name (fieldName userFamilyNameField) ! A.value (fromString . u_name $ u) ! A.size "20"
      H.tr $ do
        H.td $ H.input ! A.type_ "submit" ! A.value "Save changes"
