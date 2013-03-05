{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Control.Monad (liftM)

import Bead.Controller.Pages as P (Page(Exercise))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (selectExercises)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.HandlerUtils

home :: Content
home = Content {
    get   = Just homePage
  , post  = Nothing
  }

homePage :: GETContentHandler
homePage = withUserStateE $ \s -> do
  keys <- runStoryE . selectExercises $ every
  let es = exerciseKeys (routeOf P.Exercise) (map fst keys)
  lift $ blaze $ withUserFrame s es Nothing

  where
    every _ _ = True
