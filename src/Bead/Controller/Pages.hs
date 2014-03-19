{-# LANGUAGE CPP #-}
module Bead.Controller.Pages (
    Page(..)
  , pageCata -- Template function for the page data structure
  , parentPage
  , allowedPage
  , contentPages
  , menuPages
  , isLogin
  , isLogout
  , isHome
  , isProfile
  , isError
  , isAdministration
  , isCourseAdmin
  , isCourseOverview
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
  , isTemporaryViewPage
#ifdef TEST
  , invariants
#endif
  ) where

import qualified Bead.Domain.Entities      as E
import qualified Bead.Domain.Relationships as R

import Control.Monad (join)

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

data Page
  = Login
  | Logout
  | Home
  | Profile
  | Error
  | Administration
  | CourseAdmin
  | CourseOverview R.CourseKey
  | EvaluationTable
  | Evaluation R.SubmissionKey
  | ModifyEvaluation R.SubmissionKey R.EvaluationKey
  | NewGroupAssignment R.GroupKey
  | NewCourseAssignment R.CourseKey
  | ModifyAssignment R.AssignmentKey
  | ViewAssignment R.AssignmentKey
  | NewGroupAssignmentPreview R.GroupKey
  | NewCourseAssignmentPreview R.CourseKey
  | ModifyAssignmentPreview R.AssignmentKey
  | Submission
  | SubmissionList
  | SubmissionDetails R.AssignmentKey R.SubmissionKey
  | GroupRegistration
  | UserDetails
  | UserSubmissions
  | NewTestScript
  | ModifyTestScript R.TestScriptKey
  | UploadFile

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
  | UnsubscribeFromCourse R.GroupKey -- NOTE: There is no course without an active group
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
  p = case p of
    Login -> login
    Logout -> logout
    Home -> home
    Profile -> profile
    Error -> error
    Administration -> administration
    CourseAdmin -> courseAdmin
    CourseOverview ck -> courseOverview ck
    EvaluationTable -> evaluationTable
    Evaluation sk -> evaluation sk
    ModifyEvaluation sk ek -> modifyEvaluation sk ek
    NewGroupAssignment gk -> newGroupAssignment gk
    NewCourseAssignment ck -> newCourseAssignment ck
    ModifyAssignment ak -> modifyAssignment ak
    ViewAssignment ak -> viewAssignment ak
    NewGroupAssignmentPreview gk -> newGroupAssignmentPreview gk
    NewCourseAssignmentPreview ck -> newCourseAssignmentPreview ck
    ModifyAssignmentPreview ak -> modifyAssignmentPreview ak
    Submission -> submission
    SubmissionList -> submissionList
    SubmissionDetails ak sk -> submissionDetails ak sk
    GroupRegistration -> groupRegistration
    UserDetails -> userDetails
    UserSubmissions -> userSubmissions
    NewTestScript -> newTestScript
    ModifyTestScript tsk -> modifyTestScript tsk
    UploadFile -> uploadFile
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
    UnsubscribeFromCourse gk -> unsubscribeFromCourse gk


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

isCourseOverview (CourseOverview _) = True
isCourseOverview _                  = False

isEvaluationTable EvaluationTable = True
isEvaluationTable _               = False

isEvaluation (Evaluation _) = True
isEvaluation _              = False

isModifyEvaluation (ModifyEvaluation _ _) = True
isModifyEvaluation _                      = False

isNewGroupAssignment (NewGroupAssignment _) = True
isNewGroupAssignment _                      = False

isNewCourseAssignment (NewCourseAssignment _) = True
isNewCourseAssignment _                       = False

isModifyAssignment (ModifyAssignment _ ) = True
isModifyAssignment _                     = False

isViewAssignment (ViewAssignment _) = True
isViewAssignment _                  = False

isNewGroupAssignmentPreview (NewGroupAssignmentPreview _) = True
isNewGroupAssignmentPreview _ = False

isNewCourseAssignmentPreview (NewCourseAssignmentPreview _) = True
isNewCourseAssignmentPreview _ = False

isModifyAssignmentPreview (ModifyAssignmentPreview _) = True
isModifyAssignmentPreview _ = False

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

isNewTestScript NewTestScript = True
isNewTestScript _             = False

isModifyTestScript (ModifyTestScript _) = True
isModifyTestScript _                    = False

isUploadFile UploadFile = True
isUploadFile _          = False

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

isUnsubscribeFromCourse (UnsubscribeFromCourse _) = True
isUnsubscribeFromCourse _                         = False

-- Returns True if the page need to be rendered
contentPages :: Page -> Bool
contentPages Error = False
contentPages _     = True

-- Returns the if the given page satisfies one of the given predicates in the page predicate
-- list
isPage :: [Page -> Bool] -> Page -> Bool
isPage fs p = or $ map ($ p) fs

-- Shortcut binary or for the given predicates
(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <||> g = \x -> case f x of
                   True -> True
                   False -> g x

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
  , isViewAssignment
  , isNewGroupAssignmentPreview
  , isModifyAssignmentPreview
  , isUserSubmissions
  , isSetUserPassword
  , isUploadFile
  ]

courseAdminPages = [
    isCourseAdmin
  , isCourseOverview
  , isCreateGroup
  , isAssignGroupAdmin
  , isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewCourseAssignment
  , isNewGroupAssignment
  , isModifyAssignment
  , isNewCourseAssignmentPreview
  , isNewGroupAssignmentPreview
  , isModifyAssignmentPreview
  , isViewAssignment
  , isUserSubmissions
  , isSetUserPassword
  , isNewTestScript
  , isModifyTestScript
  , isUploadFile
  ]

adminPages = [
    isAdministration
  , isCreateCourse
  , isUserDetails
  , isAssignCourseAdmin
  , isUploadFile
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
  , isUnsubscribeFromCourse
  ]

-- Temporary data is rendered on the UI depending
-- on the user input. Temporary data pages, does
-- not redirect to a view page after submission
tempViewPages = [
    isNewGroupAssignmentPreview
  , isNewCourseAssignmentPreview
  , isModifyAssignmentPreview
  ]

-- Returns True if the given page is a temporary view page
-- otherwise False
isTemporaryViewPage :: Page -> Bool
isTemporaryViewPage p = or (map ($ p) tempViewPages)

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
  , NewTestScript
  , UploadFile
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
parentPage = pageCata
  Login   --login
  Logout  -- logout
  Home    -- home
  Profile -- profile
  Error   -- error
  Home    -- administration
  Home    -- courseAdmin
  (const Home) -- courseOverview
  Home    -- evaluationTable
  (const EvaluationTable)  -- evaluation
  (const2 EvaluationTable) -- modifyEvaluation
  (const Home) -- newGroupAssignment
  (const Home) -- newCourseAssignment
  (const Home) -- modifyAssignment
  (const Home) -- viewAssignment
  (const Home) -- newGroupAssignmentPreview
  (const Home) -- newCourseAssignmentPreview
  (const Home) -- modifyAssignmentPreview
  Home -- submission
  Home -- submissionList
  SubmissionDetails -- submissionDetails
  Home -- groupRegistration
  Administration -- userDetails
  Home -- userSubmissions
  Home -- newTestScript
  (const Home) -- modifyTestScript
  UploadFile  -- uploadFile
  Administration -- createCourse
  CourseAdmin    -- createGroup
  Administration -- sassignCourseAdmin
  CourseAdmin -- assignGroupAdmin
  Profile -- changePassword
  Home -- setUserPassword
  Evaluation -- commentFromEvaluation
  ModifyEvaluation -- commentFromModifyEvaluation
  (const Home) -- deleteUsersFromCourse
  (const Home) -- deleteUsersFromGroup
  (const Home) -- unsubscribeFromCourse
  where
    const2 = const . const

#ifdef TEST

-- * Invariants

invariants = Invariants [
    ("Regular, Admin and NonMenu pages should cover all pages",
      isPage (join [ regularPages, groupAdminPages, courseAdminPages
                   , adminPages, dataModificationPages, menuPagePred
                   , nonActivePages ]))
  ] where
      menuPagePred = [flip elem menuPageList]

#endif
