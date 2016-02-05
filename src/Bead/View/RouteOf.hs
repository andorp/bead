{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , routeOf
  , routeWithParams
  , requestRoute
  , queryString -- Creates a well-formed query string from base path and parameters
  , RoutePath
  , indexPath
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
  , viewUserScorePath
  , newUserScorePath
  , modifyUserScorePath
  , userSubmissionsPath
  , newTestScriptPath
  , modifyTestScriptPath
  , uploadFilePath
  , markdownPath
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
#ifndef SSO
  , setUserPasswordPath
#endif
  , deleteUsersFromCoursePath
  , deleteUsersFromGroupPath
  , unsubscribeFromCoursePath
  , pageRoutePath
  , getSubmissionPath
  , getCourseCsvPath
  , getGroupCsvPath
  , newGroupAssessmentPath
  , newCourseAssessmentPath
  , fillNewGroupAssessmentPreviewPath
  , fillNewCourseAssessmentPreviewPath
  , modifyAssessmentPath
  , viewAssessmentPath
  , staticPath
  , pageRequestParams
#ifdef TEST
  , routeOfTest
#endif
  ) where

import           Control.Monad (join)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.List (intersperse)
import           Data.String

import           Bead.Controller.Pages
import           Bead.View.RequestParams

#ifdef TEST
import           Test.Tasty.TestSet
import           Test.QuickCheck.Arbitrary
#endif


-- Route Path represents the route in the HTTP request
type RoutePath = ByteString

indexPath :: RoutePath
indexPath = "/"

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

viewUserScorePath :: RoutePath
viewUserScorePath = "/view-user-score"

newUserScorePath :: RoutePath
newUserScorePath = "/new-user-score"

modifyUserScorePath :: RoutePath
modifyUserScorePath = "/modify-user-score"

newTestScriptPath :: RoutePath
newTestScriptPath = "/new-test-script"

modifyTestScriptPath :: RoutePath
modifyTestScriptPath = "/modify-test-script"

uploadFilePath :: RoutePath
uploadFilePath = "/upload-file"

markdownPath :: RoutePath
markdownPath = "/markdown"

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

#ifndef SSO
setUserPasswordPath :: RoutePath
setUserPasswordPath = "/set-user-password"
#endif

deleteUsersFromCoursePath :: RoutePath
deleteUsersFromCoursePath = "/delete-users-from-course"

deleteUsersFromGroupPath :: RoutePath
deleteUsersFromGroupPath = "/delete-users-from-group"

unsubscribeFromCoursePath :: RoutePath
unsubscribeFromCoursePath = "/unsubscribe-from-course"

getSubmissionPath :: RoutePath
getSubmissionPath = "/get-submission"

getCourseCsvPath :: RoutePath
getCourseCsvPath = "/get-course-csv"

getGroupCsvPath :: RoutePath
getGroupCsvPath = "/get-group-csv"

newGroupAssessmentPath :: RoutePath
newGroupAssessmentPath = "/new-group-assessment"

newCourseAssessmentPath :: RoutePath
newCourseAssessmentPath = "/new-course-assessment"

fillNewGroupAssessmentPreviewPath :: RoutePath
fillNewGroupAssessmentPreviewPath = "/fill-new-group-assessment-preview"

fillNewCourseAssessmentPreviewPath :: RoutePath
fillNewCourseAssessmentPreviewPath = "/fill-new-course-assessment-preview"

modifyAssessmentPath :: RoutePath
modifyAssessmentPath = "/modify-assessment"

viewAssessmentPath :: RoutePath
viewAssessmentPath = "/view-assessment"

staticPath :: RoutePath
staticPath = ""

type PageRoutePath = Page RoutePath RoutePath RoutePath RoutePath RoutePath

-- Returns a base path for the given page
pageRoutePath :: Page a b c d e -> PageRoutePath
pageRoutePath = pfmap id id id id id . r where
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
    viewUserScorePath
    newUserScorePath
    modifyUserScorePath
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
#ifndef SSO
    setUserPasswordPath
#endif
    deleteUsersFromCoursePath
    deleteUsersFromGroupPath
    unsubscribeFromCoursePath
    getSubmissionPath
    getCourseCsvPath
    getGroupCsvPath
    newGroupAssessmentPath
    newCourseAssessmentPath
    fillNewGroupAssessmentPreviewPath
    fillNewCourseAssessmentPreviewPath
    modifyAssessmentPath
    viewAssessmentPath

type PageReqParams = Page [ReqParam] [ReqParam] [ReqParam] [ReqParam] [ReqParam]

-- Calculates a request parameter list from the given page value
pageRequestParams :: Page a b c d e -> PageReqParams
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
  (\sk _ -> [requestParam sk]) -- viewUserScore
  (\assk u _ -> [requestParam assk, requestParam u]) -- newUserScore
  (\sk _ -> [requestParam sk]) -- modifyUserScore
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
#ifndef SSO
  (c []) -- setUserPassword
#endif
  (\ck _ -> [requestParam ck]) -- deleteUsersFromCourse
  (\gk _ -> [requestParam gk]) -- deleteUsersFromGroup
  (\gk _ -> [requestParam gk]) -- unsubscribeFromCourse
  (\sk _ -> [requestParam sk]) -- getSubmission
  (\ck _ -> [requestParam ck]) -- getCourseCsv
  (\gk _ -> [requestParam gk]) -- getGroupCsv
  (\gk _ -> [requestParam gk]) -- newGroupAssessment
  (\ck _ -> [requestParam ck]) -- newCourseAssessment
  (\gk _ -> [requestParam gk]) -- fillNewGroupAssessmentPreview
  (\ck _ -> [requestParam ck]) -- fillNewCourseAssessmentPreview
  (\ak _ -> [requestParam ak]) -- modifyAssessment
  (\ak _ -> [requestParam ak]) -- viewAssessment  
    where
      c = const

-- Calculates the full path from a page value, including the base path and the
-- request parameters
routeOf :: (IsString s) => Page a b c d e -> s
routeOf p = queryString (pageValue (pageRoutePath p)) (pageValue (pageRequestParams p))

-- Produces a query string for a GET request from the given base name, and the
-- given parameters
queryString :: (IsString s) => ByteString -> [ReqParam] -> s
queryString base []     = fromString $ Char8.unpack base
queryString base params = fromString . join $ [Char8.unpack base, "?"] ++ (intersperse "&" (map queryStringParam params))

routeWithParams :: (IsString s) => Page a b c d e -> [ReqParam] -> s
routeWithParams p rs = fromString . join $
  [routeOf p, "?"] ++ (intersperse "&" (map queryStringParam rs))

-- Creates a request route from the given route and the given request parameters
requestRoute :: (IsString s) => String -> [ReqParam] -> s
requestRoute route rs = fromString . join $
  [route, "?"] ++ (intersperse "&" (map queryStringParam rs))

#ifdef TEST

routeOfTest =
  assertProperty
    "Non-empty RouteOr path values"
    (\p -> Char8.length (routeOf' p) > 0)
    pageGen
    "RouteOf strings must not be empty"
  where
    routeOf' :: PageDesc -> ByteString
    routeOf' = routeOf

#endif
