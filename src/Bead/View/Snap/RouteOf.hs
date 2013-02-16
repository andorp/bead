{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.RouteOf (
    routeOf
  ) where

import Data.String
import Bead.Controller.Pages

routeOf :: (IsString s) => Page -> s
routeOf = r where
  r Login      = fromString "/login"
  r Home       = fromString "/home"
  r Profile    = fromString "/profile"
  r Course     = fromString "/course"
  r Group      = fromString "/group"
  r OpenExam   = fromString "/open-exam"
  r ClosedExam = fromString "/closed-exam"
  r Error      = fromString "/error"
  r SubmitExam = fromString "/submit-exam"
  r Evaulation = fromString "/evaulation"
  r Training   = fromString "/training"
  r Admin      = fromString "/admin"
  r CreateExercise = fromString "/create-exercise"
  r p = error $ "There is no route defined for the page: " ++ (show p)
