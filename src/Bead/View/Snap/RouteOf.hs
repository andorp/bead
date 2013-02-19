{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.RouteOf (
    routeOf
  , unitTests
  ) where

import Data.String
import Bead.Controller.Pages

import Bead.Invariants (UnitTests(..))

routeOf :: (IsString s) => Page -> s
routeOf = r where
  r Login      = fromString "/login"
  r Logout     = fromString "/logout"
  r Home       = fromString "/home"
  r Profile    = fromString "/profile"
  r Course     = fromString "/course"
  r Group      = fromString "/group"
  r Exercise   = fromString "/exercise"
  r ClosedExam = fromString "/closed-exam"
  r Error      = fromString "/error"
  r SubmitExam = fromString "/submit-exam"
  r Evaulation = fromString "/evaulation"
  r Training   = fromString "/training"
  r Admin      = fromString "/admin"
  r CreateExercise = fromString "/create-exercise"
  r p = error $ "There is no route defined for the page: " ++ (show p)

unitTests = UnitTests $ map (\p -> (routeOf' p, length (routeOf' p) > 0)) [Login .. ]
  where
    routeOf' :: Page -> String
    routeOf' = routeOf
