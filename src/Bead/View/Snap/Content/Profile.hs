{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Profile (
    profile
  ) where

import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

profile :: Content
profile = getContentHandler profilePage

profilePage :: GETContentHandler
profilePage = withUserState $ \s -> do
  renderPagelet $ withUserFrame s (profileContent)

profileContent :: Pagelet
profileContent = onlyHtml $ mkI18NHtml $ \i -> do
  H.p $ (translate i "Full name")
  H.p $ (translate i "Password section")
