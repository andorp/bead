{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , routeOf
  , routeWithParams
  , requestRoute
  , queryString -- Creates a well-formed query string from base path and parameters
  , RoutePath
  , loginPath
  , changeLanguagePath
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
  , newTestScriptPath
  , modifyTestScriptPath
  , uploadFilePath
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
  , deleteUsersFromCoursePath
  , deleteUsersFromGroupPath
  , unsubscribeFromCoursePath
  , pageRoutePath
  , pageRequestParams
#ifdef TEST
  , routeOfInvariants
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

changeLanguagePath :: RoutePath
changeLanguagePath = "/change-language"

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

newTestScriptPath :: RoutePath
newTestScriptPath = "/new-test-script"

modifyTestScriptPath :: RoutePath
modifyTestScriptPath = "/modify-test-script"

uploadFilePath :: RoutePath
uploadFilePath = "/upload-file"

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

deleteUsersFromCoursePath :: RoutePath
deleteUsersFromCoursePath = "/delete-users-from-course"

deleteUsersFromGroupPath :: RoutePath
deleteUsersFromGroupPath = "/delete-users-from-group"

unsubscribeFromCoursePath :: RoutePath
unsubscribeFromCoursePath = "/unsubscribe-from-course"

-- Returns a base path for the given page
pageRoutePath :: Page -> RoutePath
pageRoutePath = fromString . r where
  r = pageCata
    loginPath
    logoutPath
    homePath
    profilePath
    errorPath
    administrationPath
    courseAdminPath
    evaluationTablePath
    (const evaluationPath)
    (const $ const modifyEvaluationPath)
    (const newGroupAssignmentPath)
    (const newCourseAssignmentPath)
    modifyAssignmentPath
    submissionPath
    submissionListPath
    (const $ const submissionDetailsPath)
    groupRegistrationPath
    userDetailsPath
    userSubmissionsPath
    newTestScriptPath
    (const modifyTestScriptPath)
    uploadFilePath
    createCoursePath
    createGroupPath
    assignCourseAdminPath
    assignGroupAdminPath
    changePasswordPath
    setUserPasswordPath
    (const commentFromEvaluationPath)
    (const $ const commentFromModifyEvaluationPath)
    (const deleteUsersFromCoursePath)
    (const deleteUsersFromGroupPath)
    (const unsubscribeFromCoursePath)

-- Calculates a request parameter list from the given page value
pageRequestParams :: Page -> [ReqParam]
pageRequestParams = r where
  r (ModifyEvaluation sk ek)   = [requestParam sk, requestParam ek]
  r (Evaluation sk)            = [requestParam sk]
  r (SubmissionDetails ak sk)  = [requestParam ak, requestParam sk]
  r (CommentFromEvaluation ek) = [requestParam ek]
  r (CommentFromModifyEvaluation ek sk) = [requestParam ek, requestParam sk]
  r (DeleteUsersFromCourse ck) = [requestParam ck]
  r (DeleteUsersFromGroup gk) = [requestParam gk]
  r (UnsubscribeFromCourse ck) = [requestParam ck]
  r (ModifyTestScript tsk) = [requestParam tsk]
  r (NewCourseAssignment ck) = [requestParam ck]
  r (NewGroupAssignment gk) = [requestParam gk]
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

routeOfInvariants = Invariants [
    ("RouteOf strings must not be empty", \p -> length (routeOf' p) > 0)
  ] where
    routeOf' :: Page -> String
    routeOf' = routeOf

#endif
