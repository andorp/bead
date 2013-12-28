{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UserDetails (
    userDetails
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Email(..), roles)
import Bead.Controller.UserStories (loadUser, doesUserExist)
import Bead.Controller.Pages as P (Page(UserDetails))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)

userDetails :: Content
userDetails = getPostContentHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = withUserState $ \s -> do
  username <- getParameter usernamePrm
  exist    <- userStory $ doesUserExist username
  case exist of
    True -> do
      user     <- userStory $ loadUser username
      renderPagelet $ withUserFrame s (userDetailForm user)

    False -> renderPagelet $ withUserFrame s (userDoesNotExist username)


userDataChange :: POSTContentHandler
userDataChange = do
  user <- getValue
  return $ UpdateUser user

userDetailForm :: User -> Pagelet
userDetailForm u = onlyHtml $ mkI18NHtml $ \i18n -> do
  postForm (routeOf P.UserDetails) $ do
    inputPagelet . defaultValue $ u
    submitButton (fieldName saveChangesBtn) (i18n "Mentés")

userDoesNotExist :: Username -> Pagelet
userDoesNotExist username = onlyHtml $ mkI18NHtml $ \i -> do
  H.p $ do
    translate i "Nem létező felhasználó:"
    fromString . str $ username

