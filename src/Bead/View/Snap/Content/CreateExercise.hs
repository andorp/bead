{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateExercise (
    createExercise
  ) where

import Bead.View.Snap.Content
import qualified Bead.Controller.Pages as P (Page(CreateExercise))

import Text.Blaze.Html5 (Html,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

createExercise :: Content
createExercise = getPostContentHandler exercisePage submitExercise

-- | POST request handler, tries to get the CreateExercise user action
--   from the received request
submitExercise :: POSTContentHandler
submitExercise = do
  exerciseText <- getParam (fieldName exerciseForm)
  case exerciseText of
    Nothing -> return . LogMessage $ "No exercise form was found in the submitted form"
    Just t  -> return . CreateExercise . Exercise . unpack $ t

exercisePage :: GETContentHandler
exercisePage = withUserStateAndFrame . const $ do
      H.form ! A.method "post" ! A.action (routeOf P.CreateExercise) $ do
      H.p "Create a new exercise"
      H.textarea ! A.name "exercise" ! A.cols "20" ! A.rows "5" $ empty
      H.input ! A.type_ "submit"
