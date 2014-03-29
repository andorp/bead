{-# LANGUAGE CPP #-}
module Bead.View.Snap.Content.All (
    Route
  , routeCata
  , content
#ifdef TEST
  , invariants
#endif
  ) where

import qualified Data.Map as Map

import qualified Bead.Controller.Pages as Pages hiding (invariants)
import Bead.View.Snap.RouteOf hiding (invariants)
import Bead.View.Snap.Content (Content(..), emptyContent)
import Bead.View.Snap.Content.Home (
    home,
    deleteUsersFromCourse,
    deleteUsersFromGroup)
import Bead.View.Snap.Content.Profile (profile, changePassword)
import Bead.View.Snap.Content.CourseAdmin (courseAdmin, createGroup, assignGroupAdmin)
import Bead.View.Snap.Content.CourseOverview (courseOverview)
import Bead.View.Snap.Content.Administration (administration, assignCourseAdmin)
import Bead.View.Snap.Content.EvaluationTable (evaluationTable)
import Bead.View.Snap.Content.Evaluation (
    evaluation,
    modifyEvaluation,
    commentFromEvaluation,
    commentFromModifyEvaluation)
import Bead.View.Snap.Content.NewAssignment
import Bead.View.Snap.Content.Submission (submission)
import Bead.View.Snap.Content.SubmissionList (submissionList)
import Bead.View.Snap.Content.SubmissionDetails (submissionDetails)
import Bead.View.Snap.Content.GroupRegistration (groupRegistration, unsubscribeFromCourse)
import Bead.View.Snap.Content.CreateCourse (createCourse)
import Bead.View.Snap.Content.UserDetails (userDetails)
import Bead.View.Snap.Content.UserSubmissions (userSubmissions)
import Bead.View.Snap.Content.SetUserPassword (setUserPassword)
import Bead.View.Snap.Content.NewTestScript (newTestScript, modifyTestScript)
import Bead.View.Snap.Content.UploadFile (uploadFile)

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

-- Route consits of a route string and a content handler
-- routes are load up at the start of the server
newtype Route = Route (String, Content)

routeCata f (Route (route, contentHandler)) = f route contentHandler

-- Creates a Route value from the given path and the
-- content handler
route path content = Route (path, content)

-- A Route list that defines routes for for every Page values
content :: [Route]
content = [
  route loginPath emptyContent,
  route logoutPath emptyContent,
  route homePath home,
  route profilePath profile,
  route courseAdminPath courseAdmin,
  route courseOverviewPath courseOverview,
  route administrationPath administration,
  route evaluationTablePath evaluationTable,
  route evaluationPath evaluation,
  route submissionPath submission,
  route submissionListPath submissionList,
  route userSubmissionsPath userSubmissions,
  route newTestScriptPath newTestScript,
  route modifyTestScriptPath modifyTestScript,
  route uploadFilePath uploadFile,
  route modifyEvaluationPath modifyEvaluation,
  route submissionDetailsPath submissionDetails,
  route groupRegistrationPath groupRegistration,
  route createCoursePath createCourse,
  route userDetailsPath userDetails,
  route assignCourseAdminPath assignCourseAdmin,
  route createGroupPath createGroup,
  route assignGroupAdminPath assignGroupAdmin,
  route newGroupAssignmentPath newGroupAssignment,
  route newCourseAssignmentPath newCourseAssignment,
  route modifyAssignmentPath modifyAssignment,
  route viewAssignmentPath viewAssignment,
  route newGroupAssignmentPreviewPath newGroupAssignmentPreview,
  route newCourseAssignmentPreviewPath newCourseAssignmentPreview,
  route modifyAssignmentPreviewPath modifyAssignmentPreview,
  route changePasswordPath changePassword,
  route setUserPasswordPath setUserPassword,
  route commentFromEvaluationPath commentFromEvaluation,
  route commentFromModifyEvaluationPath commentFromModifyEvaluation,
  route deleteUsersFromCoursePath deleteUsersFromCourse,
  route deleteUsersFromGroupPath deleteUsersFromGroup,
  route unsubscribeFromCoursePath unsubscribeFromCourse]

contentMap :: Pages.Page a -> Content
contentMap p = maybe (error "Content is not defined") id
             . Map.lookup (Pages.pageValue $ pageRoutePath p)
             . Map.fromList $ map routeToPair content
  where
    routeToPair = routeCata $ \path content -> (path,content)

#ifdef TEST

invariants :: Invariants Pages.PageDesc
invariants = Invariants [
    ("Content handler must be defined ", \p -> getOrPost $ contentMap p)
  ]
  where
    getOrPost c =
      case (get c, post c) of
        (Just _, Just _)  -> True
        (Just _, Nothing) -> True
        (Nothing, Just _) -> True
        (Nothing, Nothing) -> True

#endif
