{-# LANGAUGE OverloadedStrings #-}
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
showExercise = withUserState $ \s -> do
  mKey <- getParam (fieldName exerciseKey)
  case mKey of
    Nothing -> error "Bead.View.Snap.Content.Exercise.showExercise"
    Just key -> do
      ex <- runStory . loadExercise . ExerciseKey . unpack $ key
      case ex of
        Left err -> error "Error happened: loading exercise"
        Right ex' -> do
          blaze $ withUserFrame s (exerciseForm ex') Nothing

exerciseForm :: Exercise -> Html
exerciseForm e = do
  H.p $ fromString "Exercise: "
  fromString . C.exercise $ e

submitSolution :: POSTContentHandler
submitSolution = do
  return undefined
