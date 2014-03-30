{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Bead.Controller.Pages where

import qualified Bead.Domain.Entities      as E
import           Bead.Domain.Relationships as R

import Control.Monad (join)

#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

-- View pages are rendered using the data stored in the
-- persistence layer. Mainly for information propagation
-- for the user.
data ViewPage a
  = Login a
  | Logout a
  | Home a
  | CourseOverview CourseKey a
  | EvaluationTable a
  | ViewAssignment AssignmentKey a
  | Submission a
  | SubmissionList a
  | SubmissionDetails AssignmentKey SubmissionKey a
  | UserSubmissions a
  deriving (Eq, Ord, Show, Functor)

viewPageCata
  login
  logout
  home
  courseOverview
  evaluationTable
  viewAssignment
  submission
  submissionList
  submissionDetails
  userSubmissions
  p = case p of
    Login a -> login a
    Logout a -> logout a
    Home a -> home a
    CourseOverview ck a -> courseOverview ck a
    EvaluationTable a -> evaluationTable a
    ViewAssignment ak a -> viewAssignment ak a
    Submission a -> submission a
    SubmissionList a -> submissionList a
    SubmissionDetails ak sk a -> submissionDetails ak sk a
    UserSubmissions a -> userSubmissions a

viewPageValue :: ViewPage a -> a
viewPageValue = viewPageCata
  id -- login
  id -- logout
  id -- home
  cid -- courseOverview
  id -- evaluationTable
  cid -- viewAssignment
  id -- submission
  id -- submissionList
  c2id -- submissionDetails
  id -- userSubmissions
  where
    cid = const id
    c2id = const . cid

-- User View pages are rendered using the data stored in the
-- persistence and some temporary data given by the user. Mainly
-- is for the information propagation to the user in a stated way.
data UserViewPage a
  = NewGroupAssignmentPreview GroupKey a
  | NewCourseAssignmentPreview CourseKey a
  | ModifyAssignmentPreview AssignmentKey a
  deriving (Eq, Ord, Show, Functor)

userViewPageCata
  newGroupAssignmentPreview
  newCourseAssignmentPreview
  modifyAssignmentPreview
  p = case p of
    NewGroupAssignmentPreview gk a -> newGroupAssignmentPreview gk a
    NewCourseAssignmentPreview ck a -> newCourseAssignmentPreview ck a
    ModifyAssignmentPreview ak a -> modifyAssignmentPreview ak a

userViewPageValue :: UserViewPage a -> a
userViewPageValue = userViewPageCata
  cid -- newGroupAssignmentPreview
  cid -- newCourseAssignmentPreview
  cid -- modifyAssignmentPreview
  where
    cid = const id

-- View and Modify pages which rendered using data stored in the
-- persistence and after some user input it modifies the information
-- stored in the persistence.
data ViewModifyPage a
  = Profile a
  | Administration a
  | CourseAdmin a
  | Evaluation SubmissionKey a
  | ModifyEvaluation SubmissionKey EvaluationKey a
  | NewGroupAssignment GroupKey a
  | NewCourseAssignment CourseKey a
  | ModifyAssignment AssignmentKey a
  | GroupRegistration a
  | UserDetails a
  | NewTestScript a
  | ModifyTestScript TestScriptKey a
  | UploadFile a
  deriving (Eq, Ord, Show, Functor)

viewModifyPageCata
  profile
  administration
  courseAdmin
  evaluation
  modifyEvaluation
  newGroupAssignment
  newCourseAssignment
  modifyAssignment
  groupRegistration
  userDetails
  newTestScript
  modifyTestScript
  uploadFile
  p = case p of
    Profile a -> profile a
    Administration a -> administration a
    CourseAdmin a -> courseAdmin a
    Evaluation sk a -> evaluation sk a
    ModifyEvaluation sk ek a -> modifyEvaluation sk ek a
    NewGroupAssignment gk a -> newGroupAssignment gk a
    NewCourseAssignment ck a -> newCourseAssignment ck a
    ModifyAssignment ak a -> modifyAssignment ak a
    GroupRegistration a -> groupRegistration a
    UserDetails a -> userDetails a
    NewTestScript a -> newTestScript a
    ModifyTestScript tk a -> modifyTestScript tk a
    UploadFile a -> uploadFile a

viewModifyPageValue :: ViewModifyPage a -> a
viewModifyPageValue = viewModifyPageCata
  id -- profile
  id -- administration
  id -- courseAdmin
  cid -- evaluation
  c2id -- modifyEvaluation
  cid -- newGroupAssignment
  cid -- newCourseAssignment
  cid -- modifyAssignment
  id -- groupRegistration
  id -- userDetails
  id -- newTestScript
  cid -- modifyTestScript
  id -- uploadFile
  where
    cid = const id
    c2id = const . cid

-- Modify page is a following and lously coupled page of a
-- view page, uses the information gathered from the user input
-- and changes information in the persistence layer
data ModifyPage a
  = CreateCourse a
  | CreateGroup a
  | AssignCourseAdmin a
  | AssignGroupAdmin a
  | ChangePassword a
  | SetUserPassword a
  | CommentFromEvaluation R.SubmissionKey a
  | CommentFromModifyEvaluation R.SubmissionKey R.EvaluationKey a
  | DeleteUsersFromCourse R.CourseKey a
  | DeleteUsersFromGroup R.GroupKey a
  | UnsubscribeFromCourse R.GroupKey a
  deriving (Eq, Ord, Show, Functor)

modifyPageCata
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
    CreateCourse a -> createCourse a
    CreateGroup a -> createGroup a
    AssignCourseAdmin a -> assignCourseAdmin a
    AssignGroupAdmin a -> assignGroupAdmin a
    ChangePassword a -> changePassword a
    SetUserPassword a -> setUserPassword a
    CommentFromEvaluation sk a -> commentFromEvaluation sk a
    CommentFromModifyEvaluation sk ek a -> commentFromModifyEvaluation sk ek a
    DeleteUsersFromCourse ck a -> deleteUsersFromCourse ck a
    DeleteUsersFromGroup gk a -> deleteUsersFromGroup gk a
    UnsubscribeFromCourse gk a -> unsubscribeFromCourse gk a

modifyPageValue :: ModifyPage a -> a
modifyPageValue = modifyPageCata
  id -- createCourse
  id -- createGroup
  id -- assignCourseAdmin
  id -- assignGroupAdmin
  id -- changePassword
  id -- setUserPassword
  cid -- commentFromEvaluation
  c2id -- commentFromModifyEvaluation
  cid -- deleteUsersFromCourse
  cid -- deleteUsersFromGroup
  cid -- unsubscribeFromCourse
  where
    cid = const id
    c2id = const . cid

data Page a
  = View (ViewPage a)
  | UserView (UserViewPage a)
  | ViewModify (ViewModifyPage a)
  | Modify (ModifyPage a)
  deriving (Eq, Ord, Show, Functor)

pageCata'
  view
  userView
  viewModify
  modify
  q = case q of
    View p -> view p
    UserView p -> userView p
    ViewModify p -> viewModify p
    Modify p -> modify p

pageValue :: Page a -> a
pageValue = pageCata' viewPageValue userViewPageValue viewModifyPageValue modifyPageValue

type PageDesc = Page ()

login                   = View . Login
logout                  = View . Logout
home                    = View . Home
courseOverview ck       = View . CourseOverview ck
evaluationTable         = View . EvaluationTable
viewAssignment ak       = View . ViewAssignment ak
submission              = View . Submission
submissionList          = View . SubmissionList
submissionDetails ak sk = View . SubmissionDetails ak sk
userSubmissions         = View . UserSubmissions

newGroupAssignmentPreview gk  = UserView . NewGroupAssignmentPreview gk
newCourseAssignmentPreview ck = UserView . NewCourseAssignmentPreview ck
modifyAssignmentPreview ak    = UserView . ModifyAssignmentPreview ak

profile                = ViewModify . Profile
administration         = ViewModify . Administration
courseAdmin            = ViewModify . CourseAdmin
evaluation sk          = ViewModify . Evaluation sk
modifyEvaluation sk ek = ViewModify . ModifyEvaluation sk ek
newGroupAssignment gk  = ViewModify . NewGroupAssignment gk
newCourseAssignment ck = ViewModify . NewCourseAssignment ck
modifyAssignment ak    = ViewModify . ModifyAssignment ak
groupRegistration      = ViewModify . GroupRegistration
userDetails            = ViewModify . UserDetails
newTestScript          = ViewModify . NewTestScript
modifyTestScript tk    = ViewModify . ModifyTestScript tk
uploadFile             = ViewModify . UploadFile

createCourse      = Modify . CreateCourse
createGroup       = Modify . CreateGroup
assignCourseAdmin = Modify . AssignCourseAdmin
assignGroupAdmin  = Modify . AssignGroupAdmin
changePassword    = Modify . ChangePassword
setUserPassword   = Modify . SetUserPassword
commentFromEvaluation sk = Modify . CommentFromEvaluation sk
commentFromModifyEvaluation sk ek = Modify . CommentFromModifyEvaluation sk ek
deleteUsersFromCourse ck          = Modify . DeleteUsersFromCourse ck
deleteUsersFromGroup gk           = Modify . DeleteUsersFromGroup gk
unsubscribeFromCourse gk          = Modify . UnsubscribeFromCourse gk

-- Template method for the page data structure
pageCata
  login
  logout
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
  p = case p of
    (View (Login a)) -> login a
    (View (Logout a)) -> logout a
    (View (Home a)) -> home a
    (ViewModify (Profile a)) -> profile a
    (ViewModify (Administration a)) -> administration a
    (ViewModify (CourseAdmin a)) -> courseAdmin a
    (View (CourseOverview ck a)) -> courseOverview ck a
    (View (EvaluationTable a)) -> evaluationTable a
    (ViewModify (Evaluation sk a)) -> evaluation sk a
    (ViewModify (ModifyEvaluation sk ek a)) -> modifyEvaluation sk ek a
    (ViewModify (NewGroupAssignment gk a)) -> newGroupAssignment gk a
    (ViewModify (NewCourseAssignment ck a)) -> newCourseAssignment ck a
    (ViewModify (ModifyAssignment ak a)) -> modifyAssignment ak a
    (View (ViewAssignment ak a)) -> viewAssignment ak a
    (UserView (NewGroupAssignmentPreview gk a)) -> newGroupAssignmentPreview gk a
    (UserView (NewCourseAssignmentPreview ck a)) -> newCourseAssignmentPreview ck a
    (UserView (ModifyAssignmentPreview ak a)) -> modifyAssignmentPreview ak a
    (View (Submission a)) -> submission a
    (View (SubmissionList a)) -> submissionList a
    (View (SubmissionDetails ak sk a)) -> submissionDetails ak sk a
    (ViewModify (GroupRegistration a)) -> groupRegistration a
    (ViewModify (UserDetails a)) -> userDetails a
    (View (UserSubmissions a)) -> userSubmissions a
    (ViewModify (NewTestScript a)) -> newTestScript a
    (ViewModify (ModifyTestScript tsk a)) -> modifyTestScript tsk a
    (ViewModify (UploadFile a)) -> uploadFile a
    (Modify (CreateCourse a)) -> createCourse a
    (Modify (CreateGroup a)) -> createGroup a
    (Modify (AssignCourseAdmin a)) -> assignCourseAdmin a
    (Modify (AssignGroupAdmin a)) -> assignGroupAdmin a
    (Modify (ChangePassword a)) -> changePassword a
    (Modify (SetUserPassword a)) -> setUserPassword a
    (Modify (CommentFromEvaluation sk a)) -> commentFromEvaluation sk a
    (Modify (CommentFromModifyEvaluation sk ek a)) -> commentFromModifyEvaluation sk ek a
    (Modify (DeleteUsersFromCourse ck a)) -> deleteUsersFromCourse ck a
    (Modify (DeleteUsersFromGroup gk a)) -> deleteUsersFromGroup gk a
    (Modify (UnsubscribeFromCourse gk a)) -> unsubscribeFromCourse gk a

-- Constants that attached each of the page constructor
constantsP
  login_
  logout_
  home_
  profile_
  administration_
  courseAdmin_
  courseOverview_
  evaluationTable_
  evaluation_
  modifyEvaluation_
  newGroupAssignment_
  newCourseAssignment_
  modifyAssignment_
  viewAssignment_
  newGroupAssignmentPreview_
  newCourseAssignmentPreview_
  modifyAssignmentPreview_
  submission_
  submissionList_
  submissionDetails_
  groupRegistration_
  userDetails_
  userSubmissions_
  newTestScript_
  modifyTestScript_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  changePassword_
  setUserPassword_
  commentFromEvaluation_
  commentFromModifyEvaluation_
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  = pageCata
      (c $ login login_)
      (c $ logout logout_)
      (c $ home home_)
      (c $ profile profile_)
      (c $ administration administration_)
      (c $ courseAdmin courseAdmin_)
      (\ck _ -> courseOverview ck courseOverview_)
      (c $ evaluationTable evaluationTable_)
      (\ek _ -> evaluation ek evaluation_)
      (\sk ek _ -> modifyEvaluation sk ek modifyEvaluation_)
      (\gk _ -> newGroupAssignment gk newGroupAssignment_)
      (\ck _ -> newCourseAssignment ck newCourseAssignment_)
      (\ak _ -> modifyAssignment ak modifyAssignment_)
      (\ak _ -> viewAssignment ak viewAssignment_)
      (\gk _ -> newGroupAssignmentPreview gk newGroupAssignmentPreview_)
      (\ck _ -> newCourseAssignmentPreview ck newCourseAssignmentPreview_)
      (\ak _ -> modifyAssignmentPreview ak modifyAssignmentPreview_)
      (c $ submission submission_)
      (c $ submissionList submissionList_)
      (\ak sk _ -> submissionDetails ak sk submissionDetails_)
      (c $ groupRegistration groupRegistration_)
      (c $ userDetails userDetails_)
      (c $ userSubmissions userSubmissions_)
      (c $ newTestScript newTestScript_)
      (\tsk _ -> modifyTestScript tsk modifyTestScript_)
      (c $ uploadFile uploadFile_)
      (c $ createCourse createCourse_)
      (c $ createGroup createGroup_)
      (c $ assignCourseAdmin assignCourseAdmin_)
      (c $ assignGroupAdmin assignGroupAdmin_)
      (c $ changePassword changePassword_)
      (c $ setUserPassword setUserPassword_)
      (\sk _ -> commentFromEvaluation sk commentFromEvaluation_)
      (\sk ek _ -> commentFromModifyEvaluation sk ek commentFromModifyEvaluation_)
      (\ck _ -> deleteUsersFromCourse ck deleteUsersFromCourse_)
      (\gk _ -> deleteUsersFromGroup gk deleteUsersFromGroup_)
      (\gk _ -> unsubscribeFromCourse gk unsubscribeFromCourse_)
  where
    c = const

liftsP
  login_
  logout_
  home_
  profile_
  administration_
  courseAdmin_
  courseOverview_
  evaluationTable_
  evaluation_
  modifyEvaluation_
  newGroupAssignment_
  newCourseAssignment_
  modifyAssignment_
  viewAssignment_
  newGroupAssignmentPreview_
  newCourseAssignmentPreview_
  modifyAssignmentPreview_
  submission_
  submissionList_
  submissionDetails_
  groupRegistration_
  userDetails_
  userSubmissions_
  newTestScript_
  modifyTestScript_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  changePassword_
  setUserPassword_
  commentFromEvaluation_
  commentFromModifyEvaluation_
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  = pageCata
      (login . login_)
      (logout . logout_)
      (home . home_)
      (profile . profile_)
      (administration . administration_)
      (courseAdmin . courseAdmin_)
      (\ck a -> courseOverview ck (courseOverview_ ck a))
      (evaluationTable . evaluationTable_)
      (\ek a -> evaluation ek (evaluation_ ek a))
      (\sk ek a -> modifyEvaluation sk ek (modifyEvaluation_ sk ek a))
      (\gk a -> newGroupAssignment gk (newGroupAssignment_ gk a))
      (\ck a -> newCourseAssignment ck (newCourseAssignment_ ck a))
      (\ak a -> modifyAssignment ak (modifyAssignment_ ak a))
      (\ak a -> viewAssignment ak (viewAssignment_ ak a))
      (\gk a -> newGroupAssignmentPreview gk (newGroupAssignmentPreview_ gk a))
      (\ck a -> newCourseAssignmentPreview ck (newCourseAssignmentPreview_ ck a))
      (\ak a -> modifyAssignmentPreview ak (modifyAssignmentPreview_ ak a))
      (submission . submission_)
      (submissionList . submissionList_)
      (\ak sk a -> submissionDetails ak sk (submissionDetails_ ak sk a))
      (groupRegistration . groupRegistration_)
      (userDetails . userDetails_)
      (userSubmissions . userSubmissions_)
      (newTestScript . newTestScript_)
      (\tsk a -> modifyTestScript tsk (modifyTestScript_ tsk a))
      (uploadFile . uploadFile_)
      (createCourse . createCourse_)
      (createGroup . createGroup_)
      (assignCourseAdmin . assignCourseAdmin_)
      (assignGroupAdmin . assignGroupAdmin_)
      (changePassword . changePassword_)
      (setUserPassword . setUserPassword_)
      (\sk a -> commentFromEvaluation sk (commentFromEvaluation_ sk a))
      (\sk ek a -> commentFromModifyEvaluation sk ek (commentFromModifyEvaluation_ sk ek a))
      (\ck a -> deleteUsersFromCourse ck (deleteUsersFromCourse_ ck a))
      (\gk a -> deleteUsersFromGroup gk (deleteUsersFromGroup_ gk a))
      (\gk a -> unsubscribeFromCourse gk (unsubscribeFromCourse_ gk a))

isLogin (View (Login _)) = True
isLogin _ = False

isLogout (View (Logout _)) = True
isLogout _ = False

isHome (View (Home _)) = True
isHome _ = False

isProfile (ViewModify (Profile _)) = True
isProfile _ = False

isAdministration (ViewModify (Administration _)) = True
isAdministration _ = False

isCourseAdmin (ViewModify (CourseAdmin _)) = True
isCourseAdmin _ = False

isCourseOverview (View (CourseOverview _ _)) = True
isCourseOverview _ = False

isEvaluationTable (View (EvaluationTable _)) = True
isEvaluationTable _ = False

isEvaluation (ViewModify (Evaluation _ _)) = True
isEvaluation _ = False

isModifyEvaluation (ViewModify (ModifyEvaluation _ _ _)) = True
isModifyEvaluation _ = False

isNewGroupAssignment (ViewModify (NewGroupAssignment _ _)) = True
isNewGroupAssignment _ = False

isNewCourseAssignment (ViewModify (NewCourseAssignment _ _)) = True
isNewCourseAssignment _ = False

isModifyAssignment (ViewModify (ModifyAssignment _ _)) = True
isModifyAssignment _ = False

isViewAssignment (View (ViewAssignment _ _)) = True
isViewAssignment _ = False

isNewGroupAssignmentPreview (UserView (NewGroupAssignmentPreview _ _)) = True
isNewGroupAssignmentPreview _ = False

isNewCourseAssignmentPreview (UserView (NewCourseAssignmentPreview _ _)) = True
isNewCourseAssignmentPreview _ = False

isModifyAssignmentPreview (UserView (ModifyAssignmentPreview _ _)) = True
isModifyAssignmentPreview _ = False

isSubmission (View (Submission _)) = True
isSubmission _ = False

isSubmissionList (View (SubmissionList _)) = True
isSubmissionList _ = False

isSubmissionDetails (View (SubmissionDetails _ _ _)) = True
isSubmissionDetails _ = False

isGroupRegistration (ViewModify (GroupRegistration _)) = True
isGroupRegistration _ = False

isUserDetails (ViewModify (UserDetails _)) = True
isUserDetails _ = False

isUserSubmissions (View (UserSubmissions _)) = True
isUserSubmissions _ = False

isNewTestScript (ViewModify (NewTestScript _)) = True
isNewTestScript _ = False

isModifyTestScript (ViewModify (ModifyTestScript _ _)) = True
isModifyTestScript _ = False

isUploadFile (ViewModify (UploadFile _)) = True
isUploadFile _ = False

isCreateCourse (Modify (CreateCourse _)) = True
isCreateCourse _ = False

isCreateGroup (Modify (CreateGroup _)) = True
isCreateGroup _ = False

isAssignCourseAdmin (Modify (AssignCourseAdmin _))= True
isAssignCourseAdmin _ = False

isAssignGroupAdmin (Modify (AssignGroupAdmin _)) = True
isAssignGroupAdmin _ = False

isChangePassword (Modify (ChangePassword _)) = True
isChangePassword _ = False

isSetUserPassword (Modify (SetUserPassword _))= True
isSetUserPassword _ = False

isCommentFromEvaluation (Modify (CommentFromEvaluation _ _)) = True
isCommentFromEvaluation _ = False

isCommentFromModifyEvaluation (Modify (CommentFromModifyEvaluation _ _ _)) = True
isCommentFromModifyEvaluation _ = False

isDeleteUsersFromCourse (Modify (DeleteUsersFromCourse _ _)) = True
isDeleteUsersFromCourse _ = False

isDeleteUsersFromGroup (Modify (DeleteUsersFromGroup _ _)) = True
isDeleteUsersFromGroup _ = False

isUnsubscribeFromCourse (Modify (UnsubscribeFromCourse _ _)) = True
isUnsubscribeFromCourse _ = False

-- Returns the if the given page satisfies one of the given predicates in the page predicate
-- list
isPage :: [Page a -> Bool] -> Page a -> Bool
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
isTemporaryViewPage :: Page a -> Bool
isTemporaryViewPage p = or (map ($ p) tempViewPages)

-- Pages that not part of the site content

nonActivePages = [
    isLogin
  , isLogout
  ]

menuPageList = map ($ ()) [
    home
  , profile
  , administration
  , courseAdmin
  , evaluationTable
  , groupRegistration
  , userSubmissions
  , newTestScript
  , uploadFile
  ]

-- Returns a page predicate function depending on the role, which page transition is allowed,
-- from a given page
allowedPage :: E.Role -> (Page a -> Bool)
allowedPage = E.roleCata student groupAdmin courseAdmin admin
  where
    student     = isPage regularPages
    groupAdmin  = isPage (groupAdminPages ++ regularPages)
    courseAdmin = isPage (courseAdminPages ++ regularPages)
    admin       = isPage (adminPages ++ regularPages)

-- Produces a Page list that must be rendered in the page menu for the given role
menuPages :: E.Role -> PageDesc -> [PageDesc]
menuPages r p = filter allowedPage' menuPageList
  where
    allowedPage' p' = and [
        allowedPage r p'
      , p' /= p
      ]

-- Returns a Page descriptior for the given Modify or ViewModify
-- parent page, where the page needs to be redirected after the
-- modification of the data.
parentPage :: PageDesc -> Maybe PageDesc
parentPage = pageCata'
  (const Nothing) -- view
  (const Nothing) -- userView
  viewModifyParent
  modifyParent
  where
    c2 = const . const
    viewModifyParent = Just . viewModifyPageCata
      profile -- profile
      home    -- administration
      home    -- courseAdmin
      (const evaluationTable) -- evaluation
      (c2 evaluationTable) -- modifyEvaluation
      (const home) -- newGroupAssignment
      (const home) -- newCourseAssignment
      (const home) -- modifyAssignment
      home -- groupRegistration
      administration -- userDetails
      home           -- newTestScript
      (const home)   -- modifyTestScript
      uploadFile     -- uploadFile

    modifyParent = Just . modifyPageCata
      administration -- createCourse
      courseAdmin    -- createGroup
      administration -- assignCourseAdmin
      courseAdmin    -- assignGroupAdmin
      profile        -- changePassword
      home           -- setUserPassword
      evaluation     -- commentFromEvaluation
      modifyEvaluation -- commentFromModifyEvaluation
      (const home)     -- deleteUsersFromCourse
      (const home)     -- deleteUsersFromGroup
      (const home)     -- unsubscribeFromCourse

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

