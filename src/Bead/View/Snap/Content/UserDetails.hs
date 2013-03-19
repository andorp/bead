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
userDetails = getPostContentHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = withUserStateE $ \s -> do
  username <- getParamE (fieldName usernameField) Username "Username is not found"
  user     <- runStoryE . loadUser $ username
  blaze $ withUserFrame s (userDetailForm user) Nothing

userDataChange :: POSTContentHandler
userDataChange = do
  user <- getValue
  return $ UpdateUser user

userDetailForm :: User -> Html
userDetailForm u = do
  postForm (routeOf P.UserDetails) $ do
    inputPagelet . defaultValue $ u
    submitButton "Save changes"
