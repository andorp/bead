{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , routeOf
  , routeWithParams
  , invariants
  ) where

import Data.String
import Data.List (intersperse)
import Control.Monad (join)
import Bead.Controller.Pages hiding (invariants)

import Bead.Invariants (Invariants(..))

newtype ReqParam = ReqParam (String,String)

queryStringParam :: ReqParam -> String
queryStringParam (ReqParam (k,v)) = join [k, "=", v]

class RequestParam r where
  requestParam :: r -> ReqParam

routeOf :: (IsString s) => Page -> s
routeOf = r where
  r Login      = fromString "/login"
  r Logout     = fromString "/logout"
  r Home       = fromString "/home"
  r Profile    = fromString "/profile"
  r Course     = fromString "/course"
  r Group      = fromString "/group"
  r Groups     = fromString "/groups"
  r Exercise   = fromString "/exercise"
  r ClosedExam = fromString "/closed-exam"
  r Error      = fromString "/error"
  r SubmitExam = fromString "/submit-exam"
  r Evaulation = fromString "/evaulation"
  r Training   = fromString "/training"
  r Admin      = fromString "/admin"
  r Users      = fromString "/users"
  r UserDetails    = fromString "/user-detail"
  r CreateExercise = fromString "/create-exercise"
  r Courses        = fromString "/courses"
  r CreateCourse   = fromString "/create-course"
  r CreateGroup    = fromString "/create-group"
  r p = error $ "There is no route defined for the page: " ++ (show p)

routeWithParams :: (IsString s) => Page -> [ReqParam] -> s
routeWithParams p rs = fromString . join $
  [routeOf p, "?"] ++ (intersperse "&" (map queryStringParam rs))

invariants = Invariants [
    ("RouteOf strings must not be empty", \p -> length (routeOf' p) > 0)
  ] where
    routeOf' :: Page -> String
    routeOf' = routeOf
