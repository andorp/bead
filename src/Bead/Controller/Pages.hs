{-# LANGUAGE CPP #-}
module Bead.Controller.Pages (
    Page(..)
  , allPages
  , pageTransition
  , parentPage
  , allowedPages
  , contentPages
  , menuPages
  , reachable
#ifdef TEST
  , invariants
  , unitTests
#endif
  ) where

import qualified Bead.Domain.Entities      as E
import qualified Bead.Domain.Relationships as R

import Data.List ((\\),nub)
import Control.Monad (join)

import Data.Set (Set(..))
import qualified Data.Set as Set hiding ((\\))
#ifdef TEST
import Bead.Invariants (Invariants(..), UnitTests(..))
#endif

-- * Page types and necessary data

data Page
  = Login
  | Logout
  | Home
  | Profile
  | Error
  | Administration
  | CourseAdmin
  | EvaluationTable
  | Evaluation
  | ModifyEvaluation
  | NewGroupAssignment
  | NewCourseAssignment
  | ModifyAssignment
  | Submission
  | SubmissionList
  | SubmissionDetails
  | GroupRegistration
  | UserDetails
  | UserSubmissions

  -- Only Post handlers
  | CreateCourse
  | CreateGroup
  | AssignCourseAdmin
  | AssignGroupAdmin
  | ChangePassword
  | SetUserPassword
  -- etc ...
  deriving (Eq, Ord, Enum, Show)

allPages :: [Page]
allPages = [Login .. ]

contentPages :: [Page]
contentPages = allPages \\ [Error]

pageTransition Logout = [Login, Logout]
pageTransition Login = [Login, Home]
pageTransition s = nub $ p s ++ [s, Error, Logout] where
  p Error            = []
  p Home = [ Logout, CourseAdmin, EvaluationTable, NewGroupAssignment, NewCourseAssignment
           , Submission, SubmissionList, GroupRegistration, Administration, Profile
           , UserSubmissions, SubmissionDetails, ModifyAssignment
           , SetUserPassword --TODO
           ]
  p CourseAdmin      = [Home, CreateGroup, AssignGroupAdmin]
  p EvaluationTable  = [Home, Evaluation, ModifyEvaluation]
  p Evaluation       = [Home, EvaluationTable]
  p Submission       = [Home, SubmissionList]
  p SubmissionList   = [Home, SubmissionDetails]
  p SubmissionDetails = [Home, SubmissionList]
  p Administration   = [Home, CreateCourse, UserDetails, AssignCourseAdmin]
  p Profile          = [Home, ChangePassword]
  p UserSubmissions  = [Home, ModifyEvaluation, Evaluation]
  p ModifyEvaluation = [Home, EvaluationTable]
  p UserDetails      = [Administration]
  p GroupRegistration = [Home]
  p NewGroupAssignment = [Home, NewGroupAssignment]
  p NewCourseAssignment = [Home, NewCourseAssignment]
  p ModifyAssignment    = [Home]

  p CreateCourse      = [Administration]
  p AssignCourseAdmin = [Administration]
  p CreateGroup       = [CourseAdmin]
  p AssignGroupAdmin  = [CourseAdmin]
  p ChangePassword    = [Profile]
  p SetUserPassword   = [Home]

reachable :: Page -> Page -> Bool
reachable p q = elem q $ pageTransition p

regularPages = [
    Home
  , Profile
  , ChangePassword
  , Error
  , Submission
  , SubmissionList
  , SubmissionDetails
  , GroupRegistration
  ]

groupAdminPages = [
    EvaluationTable
  , Evaluation
  , ModifyEvaluation
  , NewGroupAssignment
  , ModifyAssignment
  , UserSubmissions
  , SetUserPassword
  ]

courseAdminPages = [
    CourseAdmin
  , CreateGroup
  , AssignGroupAdmin
  , EvaluationTable
  , Evaluation
  , ModifyEvaluation
  , NewCourseAssignment
  , NewGroupAssignment
  , ModifyAssignment
  , UserSubmissions
  , SetUserPassword
  ]

adminPages = [
    Administration
  , CreateCourse
  , UserDetails
  , AssignCourseAdmin
  ]

nonMenuPages = [
    Login
  , Logout
  , Error
  , CreateCourse
  , Submission
  , SubmissionList
  , SubmissionDetails
  , UserDetails
  , Evaluation
  , ModifyEvaluation
  , AssignCourseAdmin
  , CreateGroup
  , AssignGroupAdmin
  , NewGroupAssignment
  , NewCourseAssignment
  , ModifyAssignment
  , UserSubmissions
  , ChangePassword
  , SetUserPassword
  ]

allowedPages :: E.Role -> [Page]
allowedPages E.Student     =                     regularPages
allowedPages E.GroupAdmin  = groupAdminPages  ++ regularPages
allowedPages E.CourseAdmin = courseAdminPages ++ regularPages
allowedPages E.Admin       = adminPages       ++ regularPages

menuPages :: E.Role -> Page -> [Page]
menuPages r p = filter allowedPage $ pageTransition p
  where
    allowedPage p' = and [
        elem p' $ allowedPages r
      , not $ elem p' nonMenuPages
      , p' /= p
      ]

parentPage :: Page -> Page
parentPage Login          = Login
parentPage Error          = Error
parentPage Logout         = Logout
parentPage Profile        = Profile
parentPage Home           = Home
parentPage CourseAdmin    = Home
parentPage EvaluationTable = Home
parentPage Evaluation      = EvaluationTable
parentPage ModifyEvaluation = EvaluationTable
parentPage Submission      = Home
parentPage SubmissionList  = Home
parentPage UserSubmissions = Home
parentPage SubmissionDetails = Home
parentPage Administration  = Home
parentPage GroupRegistration = Home
parentPage UserDetails  = Administration
parentPage CreateCourse = Administration
parentPage AssignCourseAdmin = Administration
parentPage CreateGroup     = CourseAdmin
parentPage AssignGroupAdmin = CourseAdmin
parentPage NewGroupAssignment  = NewGroupAssignment
parentPage NewCourseAssignment = NewCourseAssignment
parentPage ModifyAssignment    = Home
parentPage ChangePassword      = Profile
parentPage SetUserPassword     = Home

#ifdef TEST

-- * Invariants

invariants = Invariants [
    -- For each page the following property is hold:
    -- Every parent page of a page has a transition to the given page
    ("Each parent page of a page has a transition to the given page", 
      \p -> elem p $ pageTransition $ parentPage p)
  ]

unitTests = UnitTests [
    ("Regular, Admin and NonMenu pages should cover all pages",
     Set.fromList (join [regularPages, groupAdminPages, courseAdminPages, adminPages, nonMenuPages]) ==
     Set.fromList allPages)
  ]
#endif
