{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Logging
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.HandlerUtils

home :: Content
home = Content {
    get   = Just homePage
  , post  = Nothing
  }

homePage :: Handler App App ()
homePage = withUserState $ \s ->
  blaze $ withUserFrame s "This is the home page, with a rendered menu" Nothing

