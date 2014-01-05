{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , routeOf
  , routeWithParams
  , requestRoute
  , RoutePath
  , loginPath
  , logoutPath
  , homePath
  , errorPath
  , profilePath
  , courseAdminPath
  , modifyEvaluationPath
  , evaluationTablePath
  , evaluationPath
  , submissionPath
  , submissionListPath
  , userSubmissionsPath
  , submissionDetailsPath
  , administrationPath
  , groupRegistrationPath
  , createCoursePath
  , userDetailsPath
  , assignCourseAdminPath
  , createGroupPath
  , assignGroupAdminPath
  , newGroupAssignmentPath
  , newCourseAssignmentPath
  , modifyAssignmentPath
  , changePasswordPath
  , setUserPasswordPath
  , commentFromEvaluationPath
  , commentFromModifyEvaluationPath
  , pageRoutePath
#ifdef TEST
  , invariants
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
import Bead.View.Snap.RequestParams

-- Route Path represents the route in the HTTP request
type RoutePath = String

loginPath :: RoutePath
loginPath = "/login"

logoutPath :: RoutePath
logoutPath = "/logout"

homePath :: RoutePath
homePath = "/home"

errorPath :: RoutePath
errorPath = "/error"

profilePath :: RoutePath
profilePath = "/profile"

courseAdminPath :: RoutePath
courseAdminPath = "/course-admin"

modifyEvaluationPath :: RoutePath
modifyEvaluationPath = "/modify-evaluation"

evaluationTablePath :: RoutePath
evaluationTablePath = "/evaluation-table"

evaluationPath :: RoutePath
evaluationPath = "/evaluation"

submissionPath :: RoutePath
submissionPath = "/submission"

submissionListPath :: RoutePath
submissionListPath = "/submission-list"

userSubmissionsPath :: RoutePath
userSubmissionsPath = "/user-submissions"

submissionDetailsPath :: RoutePath
submissionDetailsPath = "/submission-details"

administrationPath :: RoutePath
administrationPath = "/administration"

groupRegistrationPath :: RoutePath
groupRegistrationPath = "/group-registration"

createCoursePath :: RoutePath
createCoursePath = "/create-course"

userDetailsPath :: RoutePath
userDetailsPath = "/user-details"

assignCourseAdminPath :: RoutePath
assignCourseAdminPath = "/assign-course-admin"

createGroupPath :: RoutePath
createGroupPath = "/create-group"

assignGroupAdminPath :: RoutePath
assignGroupAdminPath = "/assign-group-admin"

newGroupAssignmentPath :: RoutePath
newGroupAssignmentPath = "/new-group-assignment"

newCourseAssignmentPath :: RoutePath
newCourseAssignmentPath = "/new-course-assignment"

modifyAssignmentPath :: RoutePath
modifyAssignmentPath = "/modify-assignment"

changePasswordPath :: RoutePath
changePasswordPath = "/change-password"

setUserPasswordPath :: RoutePath
setUserPasswordPath = "/set-user-password"

commentFromEvaluationPath :: RoutePath
commentFromEvaluationPath = "/comment-from-evaluation"

commentFromModifyEvaluationPath :: RoutePath
commentFromModifyEvaluationPath = "/comment-from-modify-evaluation"

-- Returns a base path for the given page
pageRoutePath :: Page -> RoutePath
pageRoutePath = r where
  r Login      = fromString loginPath
  r Logout     = fromString logoutPath
  r Home       = fromString homePath
  r Error      = fromString errorPath
  r Profile    = fromString profilePath
  r CourseAdmin = fromString courseAdminPath
  r (ModifyEvaluation _ _) = fromString modifyEvaluationPath
  r EvaluationTable = fromString evaluationTablePath
  r (Evaluation _)  = fromString evaluationPath
  r Submission      = fromString submissionPath
  r SubmissionList  = fromString submissionListPath
  r UserSubmissions = fromString userSubmissionsPath
  r SubmissionDetails = fromString submissionDetailsPath
  r Administration   = fromString administrationPath
  r GroupRegistration = fromString groupRegistrationPath
  r CreateCourse = fromString createCoursePath
  r UserDetails = fromString userDetailsPath
  r AssignCourseAdmin = fromString assignCourseAdminPath
  r CreateGroup = fromString createGroupPath
  r AssignGroupAdmin = fromString assignGroupAdminPath
  r NewGroupAssignment  = fromString newGroupAssignmentPath
  r NewCourseAssignment  = fromString newCourseAssignmentPath
  r ModifyAssignment = fromString modifyAssignmentPath
  r ChangePassword = fromString changePasswordPath
  r SetUserPassword = fromString setUserPasswordPath
  r (CommentFromEvaluation _) = fromString commentFromEvaluationPath
  r (CommentFromModifyEvaluation _ _) = fromString commentFromModifyEvaluationPath

-- Calculates a request parameter list from the given page value
pageRequestParams :: Page -> [ReqParam]
pageRequestParams = r where
  r (ModifyEvaluation sk ek)   = [requestParam sk, requestParam ek]
  r (Evaluation sk)            = [requestParam sk]
  r (CommentFromEvaluation ek) = [requestParam ek]
  r (CommentFromModifyEvaluation ek sk) = [requestParam ek, requestParam sk]
  r _ = []

-- Calculates the full path from a page value, including the base path and the
-- request parameters
routeOf :: (IsString s) => Page -> s
routeOf p = queryString (pageRoutePath p) (pageRequestParams p)

-- Produces a query string for a GET request from the given base name, and the
-- given parameters
queryString :: (IsString s) => String -> [ReqParam] -> s
queryString base []     = fromString base
queryString base params = fromString . join $ [base, "?"] ++ (intersperse "&" (map queryStringParam params))

routeWithParams :: (IsString s) => Page -> [ReqParam] -> s
routeWithParams p rs = fromString . join $
  [routeOf p, "?"] ++ (intersperse "&" (map queryStringParam rs))

-- Creates a request route from the given route and the given request parameters
requestRoute :: (IsString s) => String -> [ReqParam] -> s
requestRoute route rs = fromString . join $
  [route, "?"] ++ (intersperse "&" (map queryStringParam rs))

#ifdef TEST

-- * Invariants

invariants = Invariants [
    ("RouteOf strings must not be empty", \p -> length (routeOf' p) > 0)
  ] where
    routeOf' :: Page -> String
    routeOf' = routeOf

#endif
