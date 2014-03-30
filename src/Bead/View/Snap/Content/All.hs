{-# LANGUAGE CPP #-}
module Bead.View.Snap.Content.All (
    pageContent
#ifdef TEST
  , invariants
#endif
  ) where


import qualified Bead.Controller.Pages as Pages hiding (invariants)
import Bead.View.Snap.Content (Content(..), emptyContent)
import Bead.View.Snap.Content.Home
import Bead.View.Snap.Content.Profile
import Bead.View.Snap.Content.CourseAdmin
import Bead.View.Snap.Content.CourseOverview
import Bead.View.Snap.Content.Administration
import Bead.View.Snap.Content.EvaluationTable
import Bead.View.Snap.Content.Evaluation
import Bead.View.Snap.Content.NewAssignment
import Bead.View.Snap.Content.Submission
import Bead.View.Snap.Content.SubmissionList
import Bead.View.Snap.Content.SubmissionDetails
import Bead.View.Snap.Content.GroupRegistration
import Bead.View.Snap.Content.CreateCourse
import Bead.View.Snap.Content.UserDetails
import Bead.View.Snap.Content.UserSubmissions
import Bead.View.Snap.Content.SetUserPassword
import Bead.View.Snap.Content.NewTestScript
import Bead.View.Snap.Content.UploadFile

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

pageContent :: Pages.Page a -> Pages.Page Content
pageContent = Pages.constantsP
  emptyContent -- login
  emptyContent -- logout
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
  commentFromEvaluation
  commentFromModifyEvaluation
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse

#ifdef TEST

invariants :: Invariants Pages.PageDesc
invariants = Invariants [
    ("Content handler must be defined ", \p -> getOrPost . Pages.pageValue $ pageContent p)
  ]
  where
    getOrPost c =
      case (get c, post c) of
        (Just _, Just _)  -> True
        (Just _, Nothing) -> True
        (Nothing, Just _) -> True
        (Nothing, Nothing) -> True

#endif
