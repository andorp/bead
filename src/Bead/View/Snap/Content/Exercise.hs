{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Exercise (
    exercise
  ) where

import Bead.View.Snap.Content hiding (exercise, exerciseForm)
import qualified Bead.View.Snap.Content as C (exercise)
import Bead.Controller.UserStories (loadExercise)
import Bead.Domain.Relationships (ExerciseKey(..))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Char8
import Data.String


exercise :: Content
exercise = getContentHandler showExercise

showExercise :: GETContentHandler
showExercise = withUserStateE $ \s -> do
  key <- getParamE (fieldName exerciseKey) ExerciseKey "Exercise key is not found"
  ex  <- runStoryE . loadExercise $ key
  blaze $ withUserFrame s (exerciseForm ex) Nothing

exerciseForm :: Exercise -> Html
exerciseForm e = do
  H.p "Exercise: "
  fromString . C.exercise $ e
