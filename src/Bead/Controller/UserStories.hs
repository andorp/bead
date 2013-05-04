{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bead.Controller.UserStories where

import Bead.Domain.Entities     as E
import Bead.Domain.Relationships
import Bead.Domain.Types
import Bead.Controller.ServiceContext
import Bead.Controller.Logging  as L
import Bead.Controller.Pages    as P
import Bead.Persistence.Persist as R

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Error (Error(..))
import Control.Concurrent.MVar
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Error  as CME
import qualified Control.Monad.Reader as CMR
import Control.Monad.Trans
import Control.Monad (join)
import Prelude hiding (log)
import Data.List (nub)
import Data.Time (UTCTime(..), getCurrentTime)
import Text.Printf (printf)

import Control.Monad.Transaction.TIO

data UserError = UserError String

userErrorMsg :: UserError -> String
userErrorMsg (UserError msg) = msg

instance Show UserError where
  show (UserError msg) = msg

instance Error UserError where
  noMsg    = UserError "Unknown Error: No message"
  strMsg e = UserError e

newtype UserStory a = UserStory {
    unStory :: CMR.ReaderT ServiceContext (CMS.StateT UserState (CME.ErrorT UserError IO)) a
  } deriving (Monad, CMS.MonadState UserState
                   , CME.MonadError UserError
                   , CMR.MonadReader ServiceContext
                   , Functor
                   , MonadIO)

runUserStory
  :: ServiceContext
  -> UserState
  -> UserStory a
  -> IO (Either UserError (a, UserState))
runUserStory context userState
  = CME.runErrorT
  . flip CMS.runStateT userState
  . flip CMR.runReaderT context
  . unStory

-- * High level user stories

-- | The user logs in with a given username and password
--   QUESTION: Is there multiple login for the given user?
--   ANSWER:   No, the user can log in once at a time
login :: Username -> Password -> String -> UserStory ()
login username password token = do
  usrContainer <- CMR.asks userContainer
  validUser    <- withPersist $ \p -> R.canUserLogin p username password
  notLoggedIn  <- liftIO $ isUserLoggedIn usrContainer (userToken (username, token))
  case (validUser, notLoggedIn) of
    (True, False) -> do
      loadUserData username password token P.Home
      s <- userState
      liftIO $ userLogsIn usrContainer (userToken s) s
    (True , True)  -> errorPage "The user is logged in somewhere else"
    (False,    _)  -> errorPage "Invalid user and password combination"

-- | The user logs out
logout :: UserStory ()
logout = do
  state <- userState
  users <- CMR.asks userContainer
  liftIO $ userLogsOut users (userToken state)
  CMS.put UserNotLoggedIn

doesUserExist :: Username -> UserStory Bool
doesUserExist u = logAction INFO ("Searches after user: " ++ show u) $ do
  authorize P_Open P_User
  withPersist $ flip R.doesUserExist u

-- | The user navigates to the next page
changePage :: P.Page -> UserStory ()
changePage p = do
  authorize P_Open (pageAsPermObj p)
  changeUserState $ \userState -> userState { page = p }
  where
    pageAsPermObj P.Administration = P_AdminPage
    pageAsPermObj _                = P_PlainPage

-- | The user changes his/her password
changePassword :: Password -> Password -> Password -> UserStory ()
changePassword old new new'
  | new /= new' = do
      logErrorMessage "Password does not match"
      errorPage "Password does not match"
  | otherwise = do
      username    <- CMS.gets user
      withPersist $ \p -> updatePwd p username old new

-- | The authorized user creates a new user
createUser :: User -> Password -> UserStory ()
createUser newUser newPassword = do
  withPersist $ \p -> saveUser p newUser newPassword
  logger      <- CMR.asks logger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("Updating user:" ++ (str . u_username $ u)) $ do
  withPersist $ flip R.updateUser u

-- | Selecting users that satisfy the given criteria
selectUsers :: (User -> Bool) -> UserStory [User]
selectUsers f = logAction INFO "Select some users" $ do
  authorize P_Open P_User
  withPersist $ flip R.filterUsers f

loadUser :: Username -> UserStory User
loadUser u = logAction INFO "Loading user information" $ do
  authorize P_Open P_User
  withPersist $ flip R.loadUser u

-- | The authorized user logically deletes the given user
--   QUESTION: What to do if the deleted user are logged in when the deletion does happen?
--   ANSWER: During his active session he can made authorized changes, after logging out
--           he will be not able to relogin.
deleteUser :: Username -> UserStory ()
deleteUser = error "deleteUser: undefined"

administratedCourses :: UserStory [(CourseKey, Course)]
administratedCourses = logAction INFO "Selecting adminstrated courses" $ do
  u <- CMS.gets user
  withPersist $ flip R.administratedCourses u

administratedGroups :: UserStory [(GroupKey, Group)]
administratedGroups = logAction INFO "Selection administrated groups" $ do
  u <- CMS.gets user
  withPersist $ flip R.administratedGroups u

-- | The 'create' function is an abstract function
--   for other creators like, createCourse and createExercise
create
  :: (PermissionObj o)
  => (o -> k -> String)      -- ^ Descriptor for the logger
  -> (Persist -> o -> TIO k) -- ^ Saver function of the persistence
  -> o                       -- ^ The object to save
  -> UserStory k
create descriptor saver object = do
  authorize P_Create (permissionObject object)
  key <- withPersist (flip saver object)
  logMessage INFO $ descriptor object key
  return key

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse = create descriptor saveCourse
  where
    descriptor course _ =
      printf "Course is created: %s"
        (show (courseName course))

selectCourses :: (CourseKey -> Course -> Bool) -> UserStory [(CourseKey, Course)]
selectCourses f = logAction INFO "Select Some Courses" $ do
  authorize P_Open P_Course
  withPersist $ flip filterCourses f

loadCourse :: CourseKey -> UserStory (Course,[GroupKey])
loadCourse k = logAction INFO ("Loading course: " ++ show k) $ do
  authorize P_Open P_Course
  withPersist $ \p -> do
    c  <- R.loadCourse p k
    ks <- R.groupKeysOfCourse p k
    return (c,ks)

createCourseAdmin :: Username -> CourseKey -> UserStory ()
createCourseAdmin u ck = logAction INFO "Set user to course admin" $ do
  authorize P_Create P_CourseAdmin
  authorize P_Open   P_User
  withPersist $ \p -> R.createCourseAdmin p u ck

createGroupProfessor :: Username -> GroupKey -> UserStory ()
createGroupProfessor u gk = logAction INFO "Set user as a professor of a group" $ do
  authorize P_Create P_Professor
  authorize P_Open   P_User
  withPersist $ \p -> R.createGroupProfessor p u gk

-- | Logically deletes an existing cousrse
deleteCourse :: CourseKey -> UserStory ()
deleteCourse = error "deleteCourse: undefined"

-- | Updates the course information
updateCourse :: CourseKey -> Course -> UserStory ()
updateCourse = error "updateCourse: undefined"

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("Creating group: " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  withPersist $ \p -> R.saveGroup p ck g

loadGroup :: GroupKey -> UserStory Group
loadGroup gk = logAction INFO ("Loading group: " ++ show gk) $ do
  authorize P_Open P_Group
  withPersist $ flip R.loadGroup gk

-- | Checks is the user is subscribed for the group
isUserInGroup :: GroupKey -> UserStory Bool
isUserInGroup gk = do
  authorize P_Open P_Group
  state <- userState
  withPersist $ \p -> R.isUserInGroup p (user state) gk

-- | Checks if the user is subscribed for the course
isUserInCourse :: CourseKey -> UserStory Bool
isUserInCourse ck = do
  authorize P_Open P_Course
  state <- userState
  withPersist $ \p -> R.isUserInCourse p (user state) ck


-- | Regsiter the user as a group intendee
subscribeToGroup :: GroupKey -> UserStory ()
subscribeToGroup gk = logAction INFO ("Subscribe to the group " ++ (show gk)) $ do
  authorize P_Open P_Group
  state <- userState
  withPersist $ \p -> do
    ck <- R.courseOfGroup p gk
    R.subscribe p (user state) ck gk

attendedGroups :: UserStory [(GroupKey, GroupDesc)]
attendedGroups = do
  uname <- CMS.gets user
  withPersist $ \p -> do
    ks <- R.userGroups p uname
    mapM (R.groupDescription p) ks

-- | Deletes logically the given course
deleteGroup :: GroupKey -> UserStory ()
deleteGroup = error "deleteGroup: undefined"

-- | Updates the group information
updateGroup :: GroupKey -> Group -> UserStory ()
updateGroup = error "updateGroup: undefined"

-- | Creates an assignment
createAssignment :: Assignment -> UserStory AssignmentKey
createAssignment = create descriptor saveAssignment
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)

createGroupAssignment :: GroupKey -> Assignment -> UserStory AssignmentKey
createGroupAssignment gk = create descriptor (\p -> saveGroupAssignment p gk)
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)

createCourseAssignment :: CourseKey -> Assignment -> UserStory AssignmentKey
createCourseAssignment ck = create descriptor (\p -> saveCourseAssignment p ck)
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)

selectAssignments :: (AssignmentKey -> Assignment -> Bool) -> UserStory [(AssignmentKey, Assignment)]
selectAssignments f = logAction INFO "Select Some Assignments" $ do
  authorize P_Open P_Assignment
  withPersist $ flip filterAssignment f

-- | The 'loadExercise' loads an exercise from the persistence layer
loadAssignment :: AssignmentKey -> UserStory Assignment
loadAssignment k = logAction INFO ("Loading assignment: " ++ show k) $ do
  authorize P_Open P_Assignment
  withPersist $ flip R.loadAssignment k

errorPage :: String -> UserStory ()
errorPage s = do
  liftIO . print $ s
  CME.throwError $ UserError s

-- * Low level user story functionality

-- | Authorize the user for the given operation.
--   It throws exception if the user is not authorized
--   for the given operation
authorize :: Permission -> PermissionObject -> UserStory ()
authorize permission pObject
  -- Only admin can open admin's page
  | pObject == P_AdminPage && canOpen permission = do
      userRole <- CMS.gets role
      case userRole of
        E.Admin -> return ()
        _       -> errorPage "Admin authorization is required"

  -- Only Course Admin and Admin can create courses
  | pObject == P_Course   && canCreate permission = adminAuthorization
  -- Only Course Admin and Admin can create exercises
  | pObject == P_Assignment && canCreate permission = groupAdminAuthorization

  | otherwise = return ()
  where
    groupAdminAuthorization = do
      userRole <- CMS.gets role
      case userRole >= E.Professor of
        False -> errorPage "Group Admin or greater authorization is required"
        True -> return ()

    adminAuthorization = do
      userRole <- CMS.gets role
      case userRole >= E.CourseAdmin of
        False -> errorPage "Course Admin or Admin authorization is required"
        True  -> return ()

-- | Checks if the user is authorized for a given operation
isAuthorized :: Permission -> PermissionObject -> UserStory Bool
isAuthorized = error "isAuthorized: undefined"

-- | No operational User Story
noOperation :: UserStory ()
noOperation = return ()

-- | Log error message through the log subsystem
logErrorMessage :: String -> UserStory ()
logErrorMessage = logMessage ERROR

-- | Log a message through the log subsystem
logMessage :: LogLevel -> String -> UserStory ()
logMessage level msg = do
  state  <- CMS.get
  let u = str . user $ state
      t = token $ state
  user   <- CMS.gets (str . user)
  CMR.asks logger >>= (\lgr -> (liftIO $ log lgr level $ join [user, " ", t, ": ", msg]))

-- | Change user state, if the user state is logged in
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState f = do
  state <- CMS.get
  case state of
    UserNotLoggedIn -> return ()
    state' -> CMS.put (f state')

loadUserData :: Username -> Password -> String -> Page -> UserStory ()
loadUserData uname pwd t p = do
  (userRole, userFamilyName) <- withPersist $ \p -> personalInfo p uname pwd
  CMS.put UserState {
              user = uname
            , page = p
            , name = userFamilyName
            , role = userRole
            , token = t
            }

userState :: UserStory UserState
userState = CMS.get

submitSolution :: AssignmentKey -> Submission -> UserStory ()
submitSolution ak s = do
  withUserAndPersist $ \u p -> do
    sk <- saveSubmission p ak u s
    return ()

availableGroups :: UserStory [(GroupKey, GroupDesc)]
availableGroups = do
  withPersist $ \p -> (mapM (R.groupDescription p . fst)) =<< (R.filterGroups p each)
  where
    each _ _ = True

userAssignmentKeys :: UserStory [AssignmentKey]
userAssignmentKeys = do
  uname <- CMS.gets user
  withPersist $ \p -> R.userAssignmentKeys p uname

userSubmissionKeys :: AssignmentKey -> UserStory [SubmissionKey]
userSubmissionKeys ak = withUserAndPersist $ \u p -> R.userSubmissions p u ak

submissionDetailsDesc :: SubmissionKey -> UserStory SubmissionDetailsDesc
submissionDetailsDesc sk = withPersist $ \p -> R.submissionDetailsDesc p sk

loadSubmission :: SubmissionKey -> UserStory Submission
loadSubmission sk = withPersist $ \p -> R.loadSubmission p sk

userAssignments :: UserStory [(AssignmentKey, AssignmentDesc)]
userAssignments = withUserAndPersist $
  \u p -> mapM (createDesc p) =<< R.userAssignmentKeys p u

  where
    asgGroup p (Nothing) (Nothing) = return id
    asgGroup p (Just gk) (Just ck) = do
      dg <- groupName <$> R.loadGroup p gk
      dc <- courseName <$> R.loadCourse p ck
      return $ \a -> a { aGroup = join [dg, " - ", dc] }
    asgGroup p (Nothing) (Just ck) = do
      d <- courseName <$> R.loadCourse p ck
      return $ \a -> a { aGroup = d }
    asgGroup p (Just gk) (Nothing) = do
      d <- groupName <$> R.loadGroup p gk
      return $ \a -> a { aGroup = d }

    createDesc :: Persist -> AssignmentKey -> TIO (AssignmentKey, AssignmentDesc)
    createDesc p ak = do
      a <- R.loadAssignment p ak
      mgk <- R.groupOfAssignment p ak
      mck <- R.courseOfAssignment p ak
      let desc = AssignmentDesc {
        aActive = True -- TODO
      , aTitle  = assignmentName a
      , aTeachers = ["Group Admin"] -- TODO
      , aGroup  = ""
      , aOk     = 0 -- TODO
      , aNew    = 0 -- TODO
      , aBad    = 0 -- TODO
      }
      f <- asgGroup p mgk mck
      return (ak, f desc)

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = do
  withPersist $ \p -> submissionDesc p sk

openSubmissions :: UserStory [SubmissionKey]
openSubmissions = do
  withUserAndPersist $ \uname p -> do
    cs <- (map fst) <$> R.administratedCourses p uname
    gs <- (map fst) <$> R.administratedGroups  p uname
    cas <- concat <$> mapM (courseAssignments p) cs
    gas <- concat <$> mapM (groupAssignments p) gs
    let as = nub (cas ++ gas)
        adminFor (_,a) = elem a as
    nonEvaulated <- R.openedSubmissions p
    assignments  <- mapM (assignmentOfSubmission p) nonEvaulated
    return $ map fst $ filter adminFor $ zip nonEvaulated assignments

submissionListDesc :: AssignmentKey -> UserStory SubmissionListDesc
submissionListDesc ak = do
  withUserAndPersist $ \uname p -> R.submissionListDesc p uname ak

submissionTables :: UserStory [SubmissionTableInfo]
submissionTables = do
  withUserAndPersist $ \uname p -> R.submissionTables p uname

-- TODO: Check if the user can evaulates only submissions that
-- are submitted for the assignment created by the user
newEvaulation :: SubmissionKey -> Evaulation -> UserStory ()
newEvaulation sk e = do
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    a <- isAdminedSubmission p u sk
    when a $ do
      R.saveEvaulation p sk e
      R.removeFromOpened p sk
      R.saveComment p sk (evaulationComment now e)
      return ()

modifyEvaulation :: EvaulationKey -> Evaulation -> UserStory ()
modifyEvaulation ek e = do
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    sk <- submissionOfEvaulation p ek
    a <- isAdminedSubmission p u sk
    when a $ do
      R.modifyEvaulation p ek e
      saveComment p sk (evaulationComment now e)
      return ()

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = do
  withUserAndPersist $ \u p -> do
    can <- canUserCommentOn p u sk
    when can $ do
      saveComment p sk c
      return ()

userSubmissions :: Username -> AssignmentKey -> UserStory (Maybe UserSubmissionDesc)
userSubmissions s ak = do
  withUserAndPersist $ \u p -> do
    -- The admin can see the submission of students who are belonging to him
    courses <- (map fst) <$> R.administratedCourses p u
    groups  <- (map fst) <$> R.administratedGroups  p u
    courseStudents <- concat <$> mapM (subscribedToCourse p) courses
    groupStudents  <- concat <$> mapM (subscribedToGroup p)  groups
    let students = nub (courseStudents ++ groupStudents)
    case elem s students of
      False -> return Nothing
      True  -> Just <$> R.userSubmissionDesc p s ak

modifyAssignment :: AssignmentKey -> Assignment -> UserStory ()
modifyAssignment ak a = do
  withUserAndPersist $ \u p -> do
    courseOrGroup <- R.courseOrGroupOfAssignment p ak
    ownedAssignment <- case courseOrGroup of
      Left  ck -> (elem ck . map fst) <$> R.administratedCourses p u
      Right gk -> (elem gk . map fst) <$> R.administratedGroups  p u
    when ownedAssignment $ R.modifyAssignment p ak a
    -- TODO: Log invalid access
    return ()

-- * User Story combinators

-- * Tools

-- | The 'logAction' first logs the message after runs the given operation
logAction :: LogLevel -> String -> UserStory a -> UserStory a
logAction level msg s = do
  logMessage level msg
  s

withUserAndPersist :: (Username -> Persist -> TIO a) -> UserStory a
withUserAndPersist f = do
  u <- CMS.gets user
  withPersist (f u)

-- | Lifting a persistence action
withPersist :: (Persist -> TIO a) -> UserStory a
withPersist m = do
  mp <- CMR.asks persist
  x <- liftIO $ modifyMVar mp $ \p -> do
         ea <- runPersist (m p)
         return (p,ea)
  case x of
    Left e -> CME.throwError $ strMsg e
    Right x -> return x

