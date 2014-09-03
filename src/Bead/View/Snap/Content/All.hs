{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Bead.View.Snap.Content.All (
    pageContent
#ifdef TEST
  , invariants
#endif
  ) where


import qualified Bead.Controller.Pages as Pages hiding (invariants)
import Bead.View.Snap.Content
import Bead.View.Snap.Content.Home.Page
import Bead.View.Snap.Content.Profile.Page
import Bead.View.Snap.Content.CourseAdmin
import Bead.View.Snap.Content.CourseOverview.Page
import Bead.View.Snap.Content.Administration
import Bead.View.Snap.Content.EvaluationTable
import Bead.View.Snap.Content.Evaluation
import Bead.View.Snap.Content.Assignment.Page
import Bead.View.Snap.Content.Submission
import Bead.View.Snap.Content.SubmissionList
import Bead.View.Snap.Content.SubmissionDetails
import Bead.View.Snap.Content.GroupRegistration
import Bead.View.Snap.Content.UserDetails
import Bead.View.Snap.Content.UserSubmissions
import Bead.View.Snap.Content.SetUserPassword
import Bead.View.Snap.Content.NewTestScript
import Bead.View.Snap.Content.UploadFile

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

pageContent :: Pages.Page a b c d -> PageHandler
pageContent = Pages.constantsP
  nullViewHandler -- login
  nullViewHandler -- logout
  home
  profile
  administration
  courseAdmin
  courseOverview
  evaluationTable
  evaluation
  modifyEvaluation
  newGroupAssignment
  newCourseAssignment
  modifyAssignment
  viewAssignment
  newGroupAssignmentPreview
  newCourseAssignmentPreview
  modifyAssignmentPreview
  submission
  submissionList
  submissionDetails
  groupRegistration
  userDetails
  userSubmissions
  newTestScript
  modifyTestScript
  uploadFile
  createCourse
  createGroup
  assignCourseAdmin
  assignGroupAdmin
  changePassword
  setUserPassword
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  where
    nullViewHandler = ViewHandler (return ())


#ifdef TEST

invariants :: Invariants Pages.PageDesc
invariants = Invariants [
    ("Content handler must be defined ", Pages.pageKindCata view userView viewModify modify . pageContent)
  ] where
      view !_x = True
      userView !_x = True
      viewModify !_x = True
      modify !_x = True

#endif
