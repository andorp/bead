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
  , courseOverviewPath
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
  , viewAssignmentPath
  , newGroupAssignmentPreviewPath
  , newCourseAssignmentPreviewPath
  , modifyAssignmentPreviewPath
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

courseOverviewPath :: RoutePath
courseOverviewPath = "/course-overview"

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

viewAssignmentPath :: RoutePath
viewAssignmentPath = "/view-assignment"

newGroupAssignmentPreviewPath :: RoutePath
newGroupAssignmentPreviewPath = "/new-group-assignment-preview"

newCourseAssignmentPreviewPath :: RoutePath
newCourseAssignmentPreviewPath = "/new-course-assignment-preview"

modifyAssignmentPreviewPath :: RoutePath
modifyAssignmentPreviewPath = "/modify-assignment-preview"

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
pageRoutePath :: Page a -> Page RoutePath
pageRoutePath = fmap fromString . r where
  r = constantsP
    loginPath
    logoutPath
    homePath
    profilePath
    administrationPath
    courseAdminPath
    courseOverviewPath
    evaluationTablePath
    evaluationPath
    modifyEvaluationPath
    newGroupAssignmentPath
    newCourseAssignmentPath
    modifyAssignmentPath
    viewAssignmentPath
    newGroupAssignmentPreviewPath
    newCourseAssignmentPreviewPath
    modifyAssignmentPreviewPath
    submissionPath
    submissionListPath
    submissionDetailsPath
    groupRegistrationPath
    userDetailsPath
    userSubmissionsPath
    newTestScriptPath
    modifyTestScriptPath
    uploadFilePath
    createCoursePath
    createGroupPath
    assignCourseAdminPath
    assignGroupAdminPath
    changePasswordPath
    setUserPasswordPath
    commentFromEvaluationPath
    commentFromModifyEvaluationPath
    deleteUsersFromCoursePath
    deleteUsersFromGroupPath
    unsubscribeFromCoursePath

-- Calculates a request parameter list from the given page value
pageRequestParams :: Page a -> Page [ReqParam]
pageRequestParams = liftsP
  (c []) -- login
  (c []) -- logout
  (c []) -- home
  (c []) -- profile
  (c []) -- administration
  (c []) -- courseAdmin
  (\ck _ -> [requestParam ck]) -- courseOverview
  (c []) -- evaluationTable
  (\ek _ -> [requestParam ek]) -- evaluation
  (\sk ek _ -> [requestParam sk, requestParam ek]) -- modifyEvaluation
  (\gk _ -> [requestParam gk]) -- newGroupAssignment
  (\ck _ -> [requestParam ck]) -- newCourseAssignment
  (\ak _ -> [requestParam ak]) -- modifyAssignment
  (\ak _ -> [requestParam ak]) -- viewAssignment
  (\gk _ -> [requestParam gk]) -- newGroupAssignmentPreview
  (\ck _ -> [requestParam ck]) -- newCourseAssignmentPreview
  (\ak _ -> [requestParam ak]) -- modifyAssignmentPreview
  (c []) -- submission
  (c []) -- submissionList
  (\ak sk _ -> [requestParam ak, requestParam sk]) -- submissionDetails
  (c []) -- groupRegistration
  (c []) -- userDetails
  (c []) -- userSubmissions
  (c []) -- newTestScript
  (\tsk _ -> [requestParam tsk]) -- modifyTestScript
  (c []) -- uploadFile
  (c []) -- createCourse
  (c []) -- createGroup
  (c []) -- assignCourseAdmin
  (c []) -- assignGroupAdmin
  (c []) -- changePassword
  (c []) -- setUserPassword
  (\sk _ -> [requestParam sk]) -- commentFromEvaluation
  (\sk ek _ -> [requestParam sk, requestParam ek]) -- commentFromModifyEvaluation
  (\ck _ -> [requestParam ck]) -- deleteUsersFromCourse
  (\gk _ -> [requestParam gk]) -- deleteUsersFromGroup
  (\gk _ -> [requestParam gk]) -- unsubscribeFromCourse
    where
      c = const

-- Calculates the full path from a page value, including the base path and the
-- request parameters
routeOf :: (IsString s) => Page a -> s
routeOf p = queryString (pageValue (pageRoutePath p)) (pageValue (pageRequestParams p))

-- Produces a query string for a GET request from the given base name, and the
-- given parameters
queryString :: (IsString s) => String -> [ReqParam] -> s
queryString base []     = fromString base
queryString base params = fromString . join $ [base, "?"] ++ (intersperse "&" (map queryStringParam params))

routeWithParams :: (IsString s) => Page a -> [ReqParam] -> s
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
    routeOf' :: Page () -> String
    routeOf' = routeOf

#endif
