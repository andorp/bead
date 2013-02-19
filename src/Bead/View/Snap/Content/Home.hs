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

homePage :: Handler App App ()
homePage = withUserState $ \s -> do
  eKeys <- runStory (selectExercises every)
  case eKeys of
    Left err -> error "Error happened: selecting exercises"
    Right keys -> do
      let es = exerciseKeys (routeOf P.Exercise) (map fst keys)
      blaze $ withUserFrame s es Nothing

  where
    every _ _ = True

