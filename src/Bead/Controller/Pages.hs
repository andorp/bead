{-# LANGUAGE CPP #-}
module Bead.Controller.Pages (
    Page(..)
  , pageCata -- Template function for the page data structure
  , pageTransition
  , parentPage
  , allowedPage
  , contentPages
  , menuPages
  , reachable
  , isLogin
  , isLogout
  , isHome
  , isProfile
  , isError
  , isAdministration
  , isCourseAdmin
  , isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewGroupAssignment
  , isNewCourseAssignment
  , isModifyAssignment
  , isSubmission
  , isSubmissionList
  , isSubmissionDetails
  , isGroupRegistration
  , isUserDetails
  , isUserSubmissions
  , isCreateCourse
  , isCreateGroup
  , isAssignCourseAdmin
  , isAssignGroupAdmin
  , isChangePassword
  , isSetUserPassword
  , isCommentFromEvaluation
  , isCommentFromModifyEvaluation
#ifdef TEST
  , invariants
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
  | Evaluation R.SubmissionKey
  | ModifyEvaluation R.SubmissionKey R.EvaluationKey
  | NewGroupAssignment
  | NewCourseAssignment
  | ModifyAssignment
  | Submission
  | SubmissionList
  | SubmissionDetails R.AssignmentKey R.SubmissionKey
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
  | CommentFromEvaluation R.SubmissionKey
  | CommentFromModifyEvaluation R.SubmissionKey R.EvaluationKey
  | DeleteUsersFromCourse R.CourseKey -- NOTE: Users will be defined in parameters
  | DeleteUsersFromGroup R.GroupKey -- NOTE: Users will be defined in parameters
  -- etc ...
  deriving (Eq, Ord, Show)

-- Template method for the page data structure
pageCata
  login
  logout
  home
  profile
  error
  administration
  courseAdmin
  evaluationTable
  evaluation
  modifyEvaluation
  newGroupAssignment
  newCourseAssignment
  modifyAssignment
  submission
  submissionList
  submissionDetails
  groupRegistration
  userDetails
  userSubmissions
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
  p = case p of
    Login -> login
    Logout -> logout
    Home -> home
    Profile -> profile
    Error -> error
    Administration -> administration
    CourseAdmin -> courseAdmin
    EvaluationTable -> evaluationTable
    Evaluation sk -> evaluation sk
    ModifyEvaluation sk ek -> modifyEvaluation sk ek
    NewGroupAssignment -> newGroupAssignment
    NewCourseAssignment -> newCourseAssignment
    ModifyAssignment -> modifyAssignment
    Submission -> submission
    SubmissionList -> submissionList
    SubmissionDetails ak sk -> submissionDetails ak sk
    GroupRegistration -> groupRegistration
    UserDetails -> userDetails
    UserSubmissions -> userSubmissions
    CreateCourse -> createCourse
    CreateGroup -> createGroup
    AssignCourseAdmin -> assignCourseAdmin
    AssignGroupAdmin -> assignGroupAdmin
    ChangePassword -> changePassword
    SetUserPassword -> setUserPassword
    CommentFromEvaluation sk -> commentFromEvaluation sk
    CommentFromModifyEvaluation sk ek -> commentFromModifyEvaluation sk ek
    DeleteUsersFromCourse ck -> deleteUsersFromCourse ck
    DeleteUsersFromGroup gk -> deleteUsersFromGroup gk


isLogin Login = True
isLogin _     = False

isLogout Logout = True
isLogout _      = False

isHome Home = True
isHome _    = False

isProfile Profile = True
isProfile _       = False

isError Error = True
isError _     = False

isAdministration Administration = True
isAdministration _              = False

isCourseAdmin CourseAdmin = True
isCourseAdmin _           = False

isEvaluationTable EvaluationTable = True
isEvaluationTable _               = False

isEvaluation (Evaluation _) = True
isEvaluation _              = False

isModifyEvaluation (ModifyEvaluation _ _) = True
isModifyEvaluation _                      = False

isNewGroupAssignment NewGroupAssignment = True
isNewGroupAssignment _                  = False

isNewCourseAssignment NewCourseAssignment = True
isNewCourseAssignment _                   = False

isModifyAssignment ModifyAssignment = True
isModifyAssignment _                = False

isSubmission Submission = True
isSubmission _          = False

isSubmissionList SubmissionList = True
isSubmissionList _              = False

isSubmissionDetails (SubmissionDetails _ _) = True
isSubmissionDetails _                       = False

isGroupRegistration GroupRegistration = True
isGroupRegistration _                 = False

isUserDetails UserDetails = True
isUserDetails _           = False

isUserSubmissions UserSubmissions = True
isUserSubmissions _               = False

isCreateCourse CreateCourse = True
isCreateCourse _            = False

isCreateGroup CreateGroup = True
isCreateGroup _           = False

isAssignCourseAdmin AssignCourseAdmin = True
isAssignCourseAdmin _                 = False

isAssignGroupAdmin AssignGroupAdmin = True
isAssignGroupAdmin _                = False

isChangePassword ChangePassword = True
isChangePassword _              = False

isSetUserPassword SetUserPassword = True
isSetUserPassword _               = False

isCommentFromEvaluation (CommentFromEvaluation _) = True
isCommentFromEvaluation _                         = False

isCommentFromModifyEvaluation (CommentFromModifyEvaluation _ _) = True
isCommentFromModifyEvaluation _                                 = False

isDeleteUsersFromCourse (DeleteUsersFromCourse _) = True
isDeleteUsersFromCourse _                         = False

isDeleteUsersFromGroup (DeleteUsersFromGroup _) = True
isDeleteUsersFromGroup _                        = False

-- Returns True if the page need to be rendered
contentPages :: Page -> Bool
contentPages Error = False
contentPages _     = True

-- Returns True if the page transition is allowed
-- from the first page to the second page
pageTransition :: Page -> Page -> Bool
pageTransition Logout = isPage [isLogin, isLogout]
pageTransition Login  = isPage [isLogin, isHome]
pageTransition s = isPage (p s) <||> isPage [isError, isLogout] where
  p Error = []
  p Home = [ isLogout, isCourseAdmin, isEvaluationTable, isNewGroupAssignment, isNewCourseAssignment
           , isSubmission, isSubmissionList, isGroupRegistration, isAdministration, isProfile
           , isUserSubmissions, isSubmissionDetails, isModifyAssignment
           , isSetUserPassword, isHome, isDeleteUsersFromCourse, isDeleteUsersFromGroup
           ]
  p CourseAdmin = [isHome, isProfile, isCreateGroup, isAssignGroupAdmin, isCourseAdmin]
  p EvaluationTable  = [isHome, isProfile, isEvaluation, isModifyEvaluation, isEvaluationTable]
  p (Evaluation _)   = [isHome, isProfile, isEvaluationTable, isCommentFromEvaluation, isEvaluation]
  p Submission       = [isHome, isProfile, isSubmissionList, isSubmission]
  p SubmissionList   = [isHome, isProfile, isSubmissionDetails, isSubmissionList]
  p (SubmissionDetails _ _) = [isHome, isProfile, isSubmissionList, isSubmissionDetails]
  p Administration   = [isHome, isProfile, isCreateCourse, isUserDetails, isAssignCourseAdmin, isAdministration]
  p Profile          = [isHome, isChangePassword, isProfile]
  p UserSubmissions  = [isHome, isProfile, isModifyEvaluation, isEvaluation, isUserSubmissions]
  p (ModifyEvaluation _ _) = [isHome, isProfile, isEvaluationTable, isCommentFromModifyEvaluation, isModifyEvaluation]
  p UserDetails      = [isAdministration, isUserDetails]
  p GroupRegistration = [isHome, isProfile, isGroupRegistration]
  p NewGroupAssignment = [isHome, isProfile, isNewGroupAssignment, isNewGroupAssignment]
  p NewCourseAssignment = [isHome, isProfile, isNewCourseAssignment, isNewCourseAssignment]
  p ModifyAssignment    = [isHome, isProfile, isModifyAssignment]

  p CreateCourse      = [isAdministration, isCreateCourse]
  p AssignCourseAdmin = [isAdministration, isAssignCourseAdmin]
  p CreateGroup       = [isCourseAdmin, isCreateGroup]
  p AssignGroupAdmin  = [isCourseAdmin, isAssignGroupAdmin]
  p ChangePassword    = [isProfile, isChangePassword]
  p SetUserPassword   = [isHome, isProfile, isSetUserPassword]
  p (CommentFromEvaluation _) = [isCommentFromEvaluation, isEvaluation]
  p (CommentFromModifyEvaluation _ _) = [isCommentFromModifyEvaluation, isModifyEvaluation]
  p (DeleteUsersFromCourse _) = [isHome]
  p (DeleteUsersFromGroup _) = [isHome]

-- Returns the if the given page satisfies one of the given predicates in the page predicate
-- list
isPage :: [Page -> Bool] -> Page -> Bool
isPage fs p = or $ map ($ p) fs

-- Shortcut binary or for the given predicates
(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <||> g = \x -> case f x of
                   True -> True
                   False -> g x

-- Returns True is the q is reachable form the p
reachable :: Page -> Page -> Bool
reachable p q = pageTransition p q

regularPages = [
    isHome
  , isProfile
  , isChangePassword
  , isError
  , isSubmission
  , isSubmissionList
  , isSubmissionDetails
  , isGroupRegistration
  ]

groupAdminPages = [
    isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewGroupAssignment
  , isModifyAssignment
  , isUserSubmissions
  , isSetUserPassword
  ]

courseAdminPages = [
    isCourseAdmin
  , isCreateGroup
  , isAssignGroupAdmin
  , isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewCourseAssignment
  , isNewGroupAssignment
  , isModifyAssignment
  , isUserSubmissions
  , isSetUserPassword
  ]

adminPages = [
    isAdministration
  , isCreateCourse
  , isUserDetails
  , isAssignCourseAdmin
  ]

-- Pages that can not be displayed only, modifies the
-- persistented data somehow
dataModificationPages = [
    isCommentFromEvaluation
  , isCommentFromModifyEvaluation
  , isEvaluation
  , isModifyEvaluation
  , isDeleteUsersFromCourse
  , isDeleteUsersFromGroup
  ]

-- Pages that not part of the site content
nonActivePages = [
    isLogin
  , isLogout
  ]

menuPageList = [
    Home
  , Profile
  , Administration
  , CourseAdmin
  , EvaluationTable
  , GroupRegistration
  , UserSubmissions
  ]

-- Returns a page predicate function depending on the role, which page transition is allowed,
-- from a given page
allowedPage :: E.Role -> (Page -> Bool)
allowedPage = E.roleCata student groupAdmin courseAdmin admin
  where
    student     = isPage regularPages
    groupAdmin  = isPage (groupAdminPages ++ regularPages)
    courseAdmin = isPage (courseAdminPages ++ regularPages)
    admin       = isPage (adminPages ++ regularPages)

-- Produces a Page list that must be rendered in the page menu for the given role
menuPages :: E.Role -> Page -> [Page]
menuPages r p = filter allowedPage' menuPageList
  where
    allowedPage' p' = and [
        allowedPage r p'
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
parentPage (Evaluation _) = EvaluationTable
parentPage (ModifyEvaluation _ _) = EvaluationTable
parentPage Submission      = Home
parentPage SubmissionList  = Home
parentPage UserSubmissions = Home
parentPage (SubmissionDetails ak sk) = SubmissionDetails ak sk
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
parentPage (CommentFromEvaluation sk) = Evaluation sk
parentPage (CommentFromModifyEvaluation sk ek) = ModifyEvaluation sk ek
parentPage (DeleteUsersFromCourse _) = Home
parentPage (DeleteUsersFromGroup _) = Home

#ifdef TEST

-- * Invariants

invariants = Invariants [
    -- For each page the following property is hold:
    -- Every parent page of a page has a transition to the given page
    ("Each parent page of a page has a transition to the given page",
      \p -> pageTransition (parentPage p) p)
  , ("Regular, Admin and NonMenu pages should cover all pages",
      isPage (join [ regularPages, groupAdminPages, courseAdminPages
                   , adminPages, dataModificationPages, menuPagePred
                   , nonActivePages ]))
  ] where
      menuPagePred = [flip elem menuPageList]

#endif
