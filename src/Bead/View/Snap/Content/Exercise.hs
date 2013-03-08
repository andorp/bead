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
exercise = Content {
    get  = Just showExercise
  , post = Nothing
  }

showExercise :: GETContentHandler
showExercise = withUserStateE $ \s -> do
  key <- getParamE . fieldName $ exerciseKey
  ex  <- runStoryE . loadExercise . ExerciseKey . unpack $ key
  lift $ blaze $ withUserFrame s (exerciseForm ex) Nothing
  return ()

exerciseForm :: Exercise -> Html
exerciseForm e = do
  H.p "Exercise: "
  fromString . C.exercise $ e

submitSolution :: POSTContentHandler
submitSolution = do
  return undefined
