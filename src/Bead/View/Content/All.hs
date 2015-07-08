{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Bead.View.Content.All (
    pageContent
#ifdef TEST
  , pageContentTest
#endif
  ) where


import qualified Bead.Controller.Pages as Pages hiding (invariants)
import Bead.View.Content
import Bead.View.Content.Home.Page
import Bead.View.Content.Profile.Page
import Bead.View.Content.CourseAdmin.Page
import Bead.View.Content.CourseOverview.Page
import Bead.View.Content.Administration.Page
import Bead.View.Content.EvaluationTable.Page
import Bead.View.Content.Evaluation.Page
import Bead.View.Content.Assignment.Page
import Bead.View.Content.Submission.Page
import Bead.View.Content.SubmissionList.Page
import Bead.View.Content.SubmissionDetails.Page
import Bead.View.Content.GroupRegistration.Page
import Bead.View.Content.UserDetails.Page
import Bead.View.Content.UserSubmissions.Page
#ifndef LDAPEnabled
import Bead.View.Content.SetUserPassword.Page
#endif
import Bead.View.Content.NewTestScript.Page
import Bead.View.Content.UploadFile.Page
import Bead.View.Content.GetSubmission

#ifdef TEST
import Test.Tasty.TestSet
#endif

pageContent :: Pages.Page a b c d e -> PageHandler
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
#ifndef LDAPEnabled
  setUserPassword
#endif
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  getSubmission
  where
    -- Returns an empty handler that computes an empty I18N Html monadic value
    nullViewHandler = ViewHandler (return (return (return ())))


#ifdef TEST

pageContentTest =
  assertProperty
    "Content handler is a total function"
    (Pages.pageKindCata view userView viewModify modify data_ . pageContent)
    Pages.pageGen
    "Content handler must be defined"
  where
      view !_x = True
      userView !_x = True
      viewModify !_x = True
      modify !_x = True
      data_ !_x = True

#endif
