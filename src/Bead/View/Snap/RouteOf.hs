{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , routeOf
  , routeWithParams
  , requestRoute
#ifdef TEST
  , invariants
  , unitTests
#endif
  ) where

import Data.String
import Data.List (intersperse, nub)
import Control.Monad (join)
#ifdef TEST
import Bead.Controller.Pages hiding (invariants, unitTests)
import Bead.Invariants (Invariants(..), UnitTests(..))
#else
import Bead.Controller.Pages
#endif

newtype ReqParam = ReqParam (String,String)

queryStringParam :: ReqParam -> String
queryStringParam (ReqParam (k,v)) = join [k, "=", v]

class ReqParamValue p where
  paramValue :: (IsString s) => p -> s

class (ReqParamValue r) => RequestParam r where
  requestParam :: r -> ReqParam

routeOf :: (IsString s) => Page -> s
routeOf = r where
  r Login      = fromString "/login"
  r Logout     = fromString "/logout"
  r Home       = fromString "/home"
  r Error      = fromString "/error"
  r Profile    = fromString "/profile"
  r CourseAdmin = fromString "/course-admin"
  r ModifyEvaulation = fromString "/modify-evaulation"
  r EvaulationTable = fromString "/evaulation-table"
  r Evaulation      = fromString "/evaulation"
  r Submission      = fromString "/submission"
  r SubmissionList  = fromString "/submission-list"
  r UserSubmissions = fromString "/user-submissions"
  r SubmissionDetails = fromString "/submission-details"
  r Administration   = fromString "/administration"
  r GroupRegistration = fromString "/group-registration"
  r CreateCourse = fromString "/create-course"
  r UserDetails = fromString "/user-details"
  r AssignCourseAdmin = fromString "/assign-course-admin"
  r CreateGroup = fromString "/create-group"
  r AssignProfessor = fromString "/assign-professor"
  r NewGroupAssignment  = fromString "/new-group-assignment"
  r NewCourseAssignment  = fromString "/new-course-assignment"
  r ModifyAssignment = fromString "/modify-assignment"
  r ChangePassword = fromString "/change-password"
  r SetUserPassword = fromString "/set-user-password"

routeWithParams :: (IsString s) => Page -> [ReqParam] -> s
routeWithParams p rs = fromString . join $
  [routeOf p, "?"] ++ (intersperse "&" (map queryStringParam rs))

-- Creates a request route from the given route and the given request parameters
requestRoute :: (IsString s) => String -> [ReqParam] -> s
requestRoute route rs = fromString . join $
  [route, "?"] ++ (intersperse "&" (map queryStringParam rs))

#ifdef TEST

-- * Invariants

unitTests = UnitTests [
    ("Routes must be differents", let rs = map routeOf allPages in (length rs == length (nub rs)) )
  ]

invariants = Invariants [
    ("RouteOf strings must not be empty", \p -> length (routeOf' p) > 0)
  ] where
    routeOf' :: Page -> String
    routeOf' = routeOf

#endif
