{-# LANGUAGE CPP #-}
module Bead.View.Snap.Content.All (
    content
#ifdef TEST
  , invariants
#endif
  ) where

import Bead.Controller.Pages (Page)
import Bead.View.Snap.Content (Content(..), emptyContent)
import Bead.View.Snap.Content.Home (home)
import Bead.View.Snap.Content.Profile (profile, changePassword)
import Bead.View.Snap.Content.CourseAdmin (courseAdmin, createGroup, assignProfessor)
import Bead.View.Snap.Content.Administration (administration, assignCourseAdmin)
import Bead.View.Snap.Content.EvaulationTable (evaulationTable)
import Bead.View.Snap.Content.Evaulation (evaulation, modifyEvaulation)
import Bead.View.Snap.Content.NewAssignment (newCourseAssignment, newGroupAssignment, modifyAssignment)
import Bead.View.Snap.Content.Submission (submission)
import Bead.View.Snap.Content.SubmissionList (submissionList)
import Bead.View.Snap.Content.SubmissionDetails (submissionDetails)
import Bead.View.Snap.Content.GroupRegistration (groupRegistration)
import Bead.View.Snap.Content.CreateCourse (createCourse)
import Bead.View.Snap.Content.UserDetails (userDetails)
import Bead.View.Snap.Content.UserSubmissions (userSubmissions)
import Bead.View.Snap.Content.SetUserPassword (setUserPassword)

import qualified Bead.Controller.Pages as P

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

content :: Page -> Content
content P.Login  = emptyContent
content P.Logout = emptyContent
content P.Home   = home
content P.Error  = emptyContent
content P.Profile = profile
content P.CourseAdmin = courseAdmin
content P.Administration = administration
content P.EvaulationTable = evaulationTable
content P.Evaulation      = evaulation
content P.Submission      = submission
content P.SubmissionList  = submissionList
content P.UserSubmissions = userSubmissions
content P.ModifyEvaulation = modifyEvaulation
content P.SubmissionDetails = submissionDetails
content P.GroupRegistration = groupRegistration
content P.CreateCourse = createCourse
content P.UserDetails = userDetails
content P.AssignCourseAdmin = assignCourseAdmin
content P.CreateGroup = createGroup
content P.AssignProfessor = assignProfessor
content P.NewGroupAssignment  = newGroupAssignment
content P.NewCourseAssignment = newCourseAssignment
content P.ModifyAssignment    = modifyAssignment
content P.ChangePassword = changePassword
content P.SetUserPassword = setUserPassword

#ifdef TEST

invariants = Invariants [
    ("Content handler must be defined ", \p -> getOrPost p)
  ]
  where
    getOrPost p =
      let c = content p
      in case (get c, post c) of
           (Just _, Just _)  -> True
           (Just _, Nothing) -> True
           (Nothing, Just _) -> True
           (Nothing, Nothing) -> True
#endif
