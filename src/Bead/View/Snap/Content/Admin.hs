{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Admin (
    admin
  ) where

import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

admin :: Content
admin = Content {
    get   = Just (blaze adminPage)
  , post  = Nothing
  }

adminPage :: Html
adminPage = base a Nothing
  where
    a = do
      H.p "Admin page"
      mapM_ (\(link,text) -> (H.p $ H.a ! A.href link $ text)) $ [
          ("/logout", "Logout")
        , ("/home", "Home")
        , ("/create-exercise", "Create Exercise")
        ]
