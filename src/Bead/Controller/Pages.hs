{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Bead.Controller.Pages where

import           Control.Monad (join)

import qualified Bead.Domain.Entities      as E
import           Bead.Domain.Relationships as R

#ifdef TEST
import           Control.Applicative
import           Test.Tasty.TestSet
import           Test.QuickCheck.Gen
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
  | SubmissionList a
  | UserSubmissions a
  | Administration a
  | CourseAdmin a
  deriving (Eq, Ord, Show, Functor)

viewPageCata
  login
  logout
  home
  courseOverview
  evaluationTable
  viewAssignment
  submissionList
  userSubmissions
  administration
  courseAdmin
  p = case p of
    Login a -> login a
    Logout a -> logout a
    Home a -> home a
    CourseOverview ck a -> courseOverview ck a
    EvaluationTable a -> evaluationTable a
    ViewAssignment ak a -> viewAssignment ak a
    SubmissionList a -> submissionList a
    UserSubmissions a -> userSubmissions a
    Administration a -> administration a
    CourseAdmin a -> courseAdmin a

viewPageValue :: ViewPage a -> a
viewPageValue = viewPageCata
  id -- login
  id -- logout
  id -- home
  cid -- courseOverview
  id -- evaluationTable
  cid -- viewAssignment
  id -- submissionList
  id -- userSubmissions
  id -- administration
  id -- courseAdmin
  where
    cid = const id

-- Pages that extract information from the persistence
-- and all the data will be rendered in the response
data DataPage a
  = GetSubmission SubmissionKey a
  deriving (Eq, Ord, Show, Functor)

dataPageCata
  getSubmission
  p = case p of
    GetSubmission sk a -> getSubmission sk a

dataPageValue :: DataPage a -> a
dataPageValue = dataPageCata
  (const id) -- getSubmission

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
#ifndef LDAPEnabled
  | SetUserPassword a
#endif
  | SubmissionDetails AssignmentKey SubmissionKey a
  | Submission a
  deriving (Eq, Ord, Show, Functor)

viewModifyPageCata
  profile
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
#ifndef LDAPEnabled
  setUserPassword
#endif
  submissionDetails
  submission
  p = case p of
    Profile a -> profile a
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
#ifndef LDAPEnabled
    SetUserPassword a -> setUserPassword a
#endif
    SubmissionDetails ak sk a -> submissionDetails ak sk a
    Submission a -> submission a

viewModifyPageValue :: ViewModifyPage a -> a
viewModifyPageValue = viewModifyPageCata
  id -- profile
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
#ifndef LDAPEnabled
  id -- setUserPassword
#endif
  c2id -- submissionDetails
  id -- submission
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
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  p = case p of
    CreateCourse a -> createCourse a
    CreateGroup a -> createGroup a
    AssignCourseAdmin a -> assignCourseAdmin a
    AssignGroupAdmin a -> assignGroupAdmin a
    ChangePassword a -> changePassword a
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
  cid -- deleteUsersFromCourse
  cid -- deleteUsersFromGroup
  cid -- unsubscribeFromCourse
  where
    cid = const id
    c2id = const . cid

-- The kind of the possible page types
data Page a b c d e
  = View       (ViewPage a)
  | UserView   (UserViewPage b)
  | ViewModify (ViewModifyPage c)
  | Modify     (ModifyPage d)
  | Data       (DataPage e)
  deriving (Eq, Ord, Show)

liftPK
  view
  userView
  viewModify
  modify
  data_
  v = case v of
    View v       -> View       $ view v
    UserView u   -> UserView   $ userView u
    ViewModify v -> ViewModify $ viewModify v
    Modify m     -> Modify     $ modify m
    Data d       -> Data       $ data_ d

pfmap f0 f1 f2 f3 f4 = liftPK (fmap f0) (fmap f1) (fmap f2) (fmap f3) (fmap f4)

pageKindCata
  view
  userView
  viewModify
  modify
  data_
  q = case q of
    View p -> view p
    UserView p -> userView p
    ViewModify p -> viewModify p
    Modify p -> modify p
    Data p -> data_ p

pageCata' = pageKindCata

pageValue :: Page a a a a a -> a
pageValue = pageCata' viewPageValue userViewPageValue viewModifyPageValue modifyPageValue dataPageValue

type Page' a = Page a a a a a

type PageDesc = Page' ()

pageToPageDesc = pfmap unit unit unit unit unit where
  unit = const ()

login                   = View . Login
logout                  = View . Logout
home                    = View . Home
courseOverview ck       = View . CourseOverview ck
evaluationTable         = View . EvaluationTable
viewAssignment ak       = View . ViewAssignment ak
submissionList          = View . SubmissionList
userSubmissions         = View . UserSubmissions
administration          = View . Administration
courseAdmin             = View . CourseAdmin
getSubmission sk        = Data . GetSubmission sk

newGroupAssignmentPreview gk  = UserView . NewGroupAssignmentPreview gk
newCourseAssignmentPreview ck = UserView . NewCourseAssignmentPreview ck
modifyAssignmentPreview ak    = UserView . ModifyAssignmentPreview ak

profile                = ViewModify . Profile
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
#ifndef LDAPEnabled
setUserPassword        = ViewModify . SetUserPassword
#endif
submissionDetails ak sk = ViewModify . SubmissionDetails ak sk
submission              = ViewModify . Submission

createCourse      = Modify . CreateCourse
createGroup       = Modify . CreateGroup
assignCourseAdmin = Modify . AssignCourseAdmin
assignGroupAdmin  = Modify . AssignGroupAdmin
changePassword    = Modify . ChangePassword
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
#ifndef LDAPEnabled
  setUserPassword
#endif
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  getSubmission
  p = case p of
    (View (Login a)) -> login a
    (View (Logout a)) -> logout a
    (View (Home a)) -> home a
    (ViewModify (Profile a)) -> profile a
    (View (Administration a)) -> administration a
    (View (CourseAdmin a)) -> courseAdmin a
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
    (ViewModify (Submission a)) -> submission a
    (View (SubmissionList a)) -> submissionList a
    (ViewModify (SubmissionDetails ak sk a)) -> submissionDetails ak sk a
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
#ifndef LDAPEnabled
    (ViewModify (SetUserPassword a)) -> setUserPassword a
#endif
    (Modify (DeleteUsersFromCourse ck a)) -> deleteUsersFromCourse ck a
    (Modify (DeleteUsersFromGroup gk a)) -> deleteUsersFromGroup gk a
    (Modify (UnsubscribeFromCourse gk a)) -> unsubscribeFromCourse gk a
    (Data (GetSubmission sk a)) -> getSubmission sk a

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
#ifndef LDAPEnabled
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  getSubmission_
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
#ifndef LDAPEnabled
      (c $ setUserPassword setUserPassword_)
#endif
      (\ck _ -> deleteUsersFromCourse ck deleteUsersFromCourse_)
      (\gk _ -> deleteUsersFromGroup gk deleteUsersFromGroup_)
      (\gk _ -> unsubscribeFromCourse gk unsubscribeFromCourse_)
      (\sk _ -> getSubmission sk getSubmission_)
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
#ifndef LDAPEnabled
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  getSubmission_
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
#ifndef LDAPEnabled
      (setUserPassword . setUserPassword_)
#endif
      (\ck a -> deleteUsersFromCourse ck (deleteUsersFromCourse_ ck a))
      (\gk a -> deleteUsersFromGroup gk (deleteUsersFromGroup_ gk a))
      (\gk a -> unsubscribeFromCourse gk (unsubscribeFromCourse_ gk a))
      (\sk a -> getSubmission sk (getSubmission_ sk a))

isLogin (View (Login _)) = True
isLogin _ = False

isLogout (View (Logout _)) = True
isLogout _ = False

isHome (View (Home _)) = True
isHome _ = False

isProfile (ViewModify (Profile _)) = True
isProfile _ = False

isAdministration (View (Administration _)) = True
isAdministration _ = False

isCourseAdmin (View (CourseAdmin _)) = True
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

isSubmission (ViewModify (Submission _)) = True
isSubmission _ = False

isSubmissionList (View (SubmissionList _)) = True
isSubmissionList _ = False

isSubmissionDetails (ViewModify (SubmissionDetails _ _ _)) = True
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

isAssignCourseAdmin (Modify (AssignCourseAdmin _)) = True
isAssignCourseAdmin _ = False

isAssignGroupAdmin (Modify (AssignGroupAdmin _)) = True
isAssignGroupAdmin _ = False

isChangePassword (Modify (ChangePassword _)) = True
isChangePassword _ = False

#ifndef LDAPEnabled
isSetUserPassword (ViewModify (SetUserPassword _)) = True
isSetUserPassword _ = False
#endif

isDeleteUsersFromCourse (Modify (DeleteUsersFromCourse _ _)) = True
isDeleteUsersFromCourse _ = False

isDeleteUsersFromGroup (Modify (DeleteUsersFromGroup _ _)) = True
isDeleteUsersFromGroup _ = False

isUnsubscribeFromCourse (Modify (UnsubscribeFromCourse _ _)) = True
isUnsubscribeFromCourse _ = False

isGetSubmission (Data (GetSubmission _ _)) = True
isGetSubmission _ = False

-- Returns the if the given page satisfies one of the given predicates in the page predicate
-- list
isPage :: [Page a b c d e -> Bool] -> Page a b c d e -> Bool
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
  , isGetSubmission
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
#ifndef LDAPEnabled
  , isSetUserPassword
#endif
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
#ifndef LDAPEnabled
  , isSetUserPassword
#endif
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
    isEvaluation
  , isModifyEvaluation
  , isDeleteUsersFromCourse
  , isDeleteUsersFromGroup
  , isUnsubscribeFromCourse
  ]

isUserViewPage :: Page a b c d e -> Bool
isUserViewPage = pageCata'
  false -- view
  true  -- userView
  false -- viewModify
  false -- modify
  false -- data
  where
    false = const False
    true  = const True

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
allowedPage :: E.Role -> (Page a b c d e -> Bool)
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
  (const Nothing)
  where
    c2 = const . const
    viewModifyParent = Just . viewModifyPageCata
      profile -- profile
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
#ifndef LDAPEnabled
      home           -- setUserPassword
#endif
      submissionDetails -- submissionDetails
      home           -- submission

    modifyParent = Just . modifyPageCata
      administration -- createCourse
      courseAdmin    -- createGroup
      administration -- assignCourseAdmin
      courseAdmin    -- assignGroupAdmin
      profile        -- changePassword
      (const home)     -- deleteUsersFromCourse
      (const home)     -- deleteUsersFromGroup
      (const home)     -- unsubscribeFromCourse

#ifdef TEST

-- * Invariants

pageDescTest = assertProperty
  "Total page union: Regular, Admin and NonMenu"
  (isPage ((join [ regularPages, groupAdminPages, courseAdminPages
                 , adminPages, dataModificationPages, menuPagePred
                 , nonActivePages ])))
  pageGen
  "Regular, Admin and NonMenu pages should cover all pages"
  where
    menuPagePred = [flip elem menuPageList]

pageGen :: Gen PageDesc
pageGen = oneof [
    nonParametricPages
  , parametricPages
  ] where
      showInt :: Int -> String
      showInt = show

      assignmentKey = AssignmentKey . showInt <$> choose (1,5000)
      submissionKey = SubmissionKey . showInt <$> choose (1,5000)
      evaluationKey = EvaluationKey . showInt <$> choose (1,5000)
      courseKey     = CourseKey . showInt     <$> choose (1,5000)
      groupKey      = GroupKey . showInt      <$> choose (1,5000)
      testScriptKey = TestScriptKey . showInt <$> choose (1,5000)

      nonParametricPages = elements [
          login ()
        , logout ()
        , home ()
        , profile ()
        , administration ()
        , courseAdmin ()
        , evaluationTable ()
        , submission ()
        , submissionList ()
        , groupRegistration ()
        , userDetails ()
        , userSubmissions ()
        , uploadFile ()
        , createCourse ()
        , createGroup ()
        , assignCourseAdmin ()
        , assignGroupAdmin ()
        , changePassword ()
#ifndef LDAPEnabled
        , setUserPassword ()
#endif
        , newTestScript ()
        ]

      parametricPages = oneof [
          evaluation <$> submissionKey <*> unit
        , courseOverview <$> courseKey <*> unit
        , modifyEvaluation <$> submissionKey <*> evaluationKey <*> unit
        , submissionDetails <$> assignmentKey <*> submissionKey <*> unit
        , deleteUsersFromCourse <$> courseKey <*> unit
        , deleteUsersFromGroup <$> groupKey <*> unit
        , unsubscribeFromCourse <$> groupKey <*> unit
        , modifyTestScript <$> testScriptKey <*> unit
        , newCourseAssignment <$> courseKey <*> unit
        , newGroupAssignment <$> groupKey <*> unit
        , modifyAssignment <$> assignmentKey <*> unit
        , viewAssignment <$> assignmentKey <*> unit
        , newCourseAssignmentPreview <$> courseKey <*> unit
        , newGroupAssignmentPreview <$> groupKey <*> unit
        , modifyAssignmentPreview <$> assignmentKey <*> unit
        , getSubmission <$> submissionKey <*> unit
        ]

      unit = return ()

#endif

