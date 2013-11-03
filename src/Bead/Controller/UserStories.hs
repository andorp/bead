{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bead.Controller.UserStories where

import Bead.Domain.Entities     as E
import Bead.Domain.Relationships
import Bead.Domain.RolePermission (permission)
import Bead.Domain.Types
import Bead.Controller.ServiceContext
import Bead.Controller.Logging  as L
import Bead.Controller.Pages    as P
import Bead.Persistence.Persist as R

import Control.Applicative
import Control.Monad (when, unless)
import Control.Monad.Error (Error(..))
import Control.Concurrent.MVar
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Error  as CME
import qualified Control.Monad.Reader as CMR
import Control.Monad.Trans
import Control.Monad (join)
import Prelude hiding (log)
import Data.List (nub)
import Data.Maybe (catMaybes)
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
                   , Applicative
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
login :: Username -> String -> UserStory ()
login username token = do
  usrContainer <- CMR.asks userContainer
  validUser <- withPersist $ flip R.doesUserExist username
  notLoggedIn  <- liftIO $ isUserLoggedIn usrContainer (userToken (username, token))
  case (validUser, notLoggedIn) of
    (True, False) -> do
      loadUserData username token P.Home
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
  CMS.put userNotLoggedIn

doesUserExist :: Username -> UserStory Bool
doesUserExist u = logAction INFO ("searches after user " ++ show u) $ do
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

-- | The authorized user creates a new user
createUser :: User -> UserStory ()
createUser newUser = do
  authorize P_Create P_User
  withPersist $ \p -> saveUser p newUser
  logger      <- CMR.asks logger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

-- Updates the timezone of the current user
setTimeZone :: TimeZone -> UserStory ()
setTimeZone tz = do
  changeUserState $ \userState -> userState { timezone = tz }
  putStatusMessage $ join ["Time zone has been set to ", show tz, "."]

-- Updates the current user email address and full name in the persistence
changeUserDetails :: Email -> String -> TimeZone -> UserStory ()
changeUserDetails email name timezone = logAction INFO ("updates email address, fullname and timezone") $ do
  user <- currentUser
  withPersist $ flip R.updateUser user { u_email = email, u_name = name , u_timezone = timezone }
  putStatusMessage "User's details have been changed"

updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("updates user " ++ (str . u_username $ u)) $ do
  authorize P_Modify P_User
  withPersist $ flip R.updateUser u

-- | Selecting users that satisfy the given criteria
selectUsers :: (User -> Bool) -> UserStory [User]
selectUsers f = logAction INFO "selects some users" $ do
  authorize P_Open P_User
  withPersist $ flip R.filterUsers f

loadUser :: Username -> UserStory User
loadUser u = logAction INFO "Loading user information" $ do
  authorize P_Open P_User
  withPersist $ flip R.loadUser u

-- The UserStroy calculation returns the current user's profile data
currentUser :: UserStory User
currentUser = logAction INFO "Load the current user's data" $ do
  u <- user <$> userState
  withPersist $ flip R.loadUser u

administratedCourses :: UserStory [(CourseKey, Course)]
administratedCourses = logAction INFO "selects adminstrated courses" $ do
  authorize P_Open P_Course
  u <- CMS.gets user
  withPersist $ flip R.administratedCourses u

administratedGroups :: UserStory [(GroupKey, Group)]
administratedGroups = logAction INFO "selects administrated groups" $ do
  authorize P_Open P_Group
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

createUserReg :: UserRegistration -> UserStory UserRegKey
createUserReg u = logAction INFO "Creates user registration" $ do
  create descriptor saveUserReg u
  where
    descriptor x _ = reg_username x

loadUserReg :: UserRegKey -> UserStory UserRegistration
loadUserReg k = logAction INFO "Loading user registration" $ do
  authorize P_Open P_UserReg
  withPersist $ flip R.loadUserReg k

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse course = logAction INFO "creates course" $ do
  authorize P_Create P_Course
  key <- create descriptor saveCourse course
  putStatusMessage $ join ["'", courseName course, "' has been created."]
  return key
  where
    descriptor course _ =
      printf "Course is created: %s"
        (show (courseName course))

selectCourses :: (CourseKey -> Course -> Bool) -> UserStory [(CourseKey, Course)]
selectCourses f = logAction INFO "selects some courses" $ do
  authorize P_Open P_Course
  withPersist $ flip filterCourses f

loadCourse :: CourseKey -> UserStory (Course,[GroupKey])
loadCourse k = logAction INFO ("loads course: " ++ show k) $ do
  authorize P_Open P_Course
  withPersist $ \p -> do
    c  <- R.loadCourse p k
    ks <- R.groupKeysOfCourse p k
    return (c,ks)

createCourseAdmin :: Username -> CourseKey -> UserStory ()
createCourseAdmin u ck = logAction INFO "sets user to course admin" $ do
  authorize P_Create P_CourseAdmin
  authorize P_Open   P_User
  withPersist $ \p -> R.createCourseAdmin p u ck
  putStatusMessage $ join [user u, " is course admin now."]
  where
    user = usernameCata id

createGroupProfessor :: Username -> GroupKey -> UserStory ()
createGroupProfessor u gk = logAction INFO "sets user as a professor of a group" $ do
  authorize P_Create P_Professor
  authorize P_Open   P_User
  withPersist $ \p -> R.createGroupProfessor p u gk
  putStatusMessage $ join [user u, " is group admin now."]
  where
    user = usernameCata id

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("creats group " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  key <- withPersist $ \p -> R.saveGroup p ck g
  putStatusMessage $ join ["'", groupName g, "' has been created."]
  return key

loadGroup :: GroupKey -> UserStory Group
loadGroup gk = logAction INFO ("loads group " ++ show gk) $ do
  authorize P_Open P_Group
  withPersist $ flip R.loadGroup gk

-- | Checks is the user is subscribed for the group
isUserInGroup :: GroupKey -> UserStory Bool
isUserInGroup gk = logAction INFO ("checks if user is in the group " ++ show gk) $ do
  authorize P_Open P_Group
  state <- userState
  withPersist $ \p -> R.isUserInGroup p (user state) gk

-- | Checks if the user is subscribed for the course
isUserInCourse :: CourseKey -> UserStory Bool
isUserInCourse ck = logAction INFO ("checks if user is in the course " ++ show ck) $ do
  authorize P_Open P_Course
  state <- userState
  withPersist $ \p -> R.isUserInCourse p (user state) ck

-- | Regsiter the user as a group intendee
subscribeToGroup :: GroupKey -> UserStory ()
subscribeToGroup gk = logAction INFO ("subscribes to the group " ++ (show gk)) $ do
  authorize P_Open P_Group
  state <- userState
  withPersist $ \p -> do
    ck <- R.courseOfGroup p gk
    R.subscribe p (user state) ck gk
  putStatusMessage "Subscribed."

attendedGroups :: UserStory [(GroupKey, GroupDesc)]
attendedGroups = logAction INFO "selects courses attended in" $ do
  authorize P_Open P_Group
  uname <- CMS.gets user
  withPersist $ \p -> do
    ks <- R.userGroups p uname
    mapM (R.groupDescription p) ks

createGroupAssignment :: GroupKey -> Assignment -> UserStory AssignmentKey
createGroupAssignment gk a = logAction INFO msg $ do
  authorize P_Open   P_Group
  authorize P_Create P_Assignment
  ak <- create descriptor (\p -> saveGroupAssignment p gk) a
  statusMsg a
  return ak
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)
    msg = "creates assignment for group " ++ show gk
    statusMsg = assignmentCata $ \name _ _ _ _ _ ->
      putStatusMessage $ "'" ++ name ++ "' group assignment is created."

createCourseAssignment :: CourseKey -> Assignment -> UserStory AssignmentKey
createCourseAssignment ck a = logAction INFO msg $ do
  authorize P_Open P_Course
  authorize P_Create P_Assignment
  ak <- create descriptor (\p -> saveCourseAssignment p ck) a
  statusMsg a
  return ak
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)
    msg = "creates assignment for course " ++ show ck
    statusMsg = assignmentCata $ \name _ _ _ _ _ ->
      putStatusMessage $ "'" ++ name ++ "' course assignment is created."

selectAssignments :: (AssignmentKey -> Assignment -> Bool) -> UserStory [(AssignmentKey, Assignment)]
selectAssignments f = logAction INFO "selects some assignments" $ do
  authorize P_Open P_Assignment
  withPersist $ flip filterAssignment f

-- | The 'loadExercise' loads an exercise from the persistence layer
loadAssignment :: AssignmentKey -> UserStory Assignment
loadAssignment k = logAction INFO ("loads assignment " ++ show k) $ do
  authorize P_Open P_Assignment
  withPersist $ flip R.loadAssignment k

-- Puts the given status message to the actual user state
putStatusMessage :: String -> UserStory ()
putStatusMessage = changeUserState . setStatus

-- Clears the status message of the user
clearStatusMessage :: UserStory ()
clearStatusMessage = changeUserState clearStatus

errorPage :: String -> UserStory ()
errorPage s = do
  liftIO . print $ s
  CME.throwError $ UserError s

-- * Low level user story functionality


authPerms :: ObjectPermissions -> UserStory ()
authPerms = mapM_ (uncurry authorize) . permissions

-- | Authorize the user for the given operation.
--   It throws exception if the user is not authorized
--   for the given operation
authorize :: Permission -> PermissionObject -> UserStory ()
authorize p o = do
  er <- CMS.gets userRole
  case er of

    Left EmptyRole ->
      errorPage "User is not logged in."

    Left RegRole -> case elem (p,o) regPermObjects of
      True  -> return ()
      False -> errorPage $ join [
          "Registration tries to reach other authentication "
        , show p, " ", show o
        ]

    Right r -> case permission r p o of
      True  -> return ()
      False -> errorPage $ join [
          "Authorization is required: ", show r, " "
        , show p, " ", show o
        ]
  where
    regPermObjects = [
        (P_Create, P_User),    (P_Open, P_User)
      , (P_Create, P_UserReg), (P_Open, P_UserReg)
      ]

-- | No operational User Story
noOperation :: UserStory ()
noOperation = return ()

-- | Log error message through the log subsystem
logErrorMessage :: String -> UserStory ()
logErrorMessage = logMessage ERROR

-- | Log a message through the log subsystem
logMessage :: LogLevel -> String -> UserStory ()
logMessage level msg = do
  CMS.get >>=
    userStateCata
      userNotLoggedIn
      registration
      loggedIn
  where
    logMsg preffix =
      CMR.asks logger >>= (\lgr -> (liftIO $ log lgr level $ join [preffix, " ", msg, "."]))

    userNotLoggedIn    = logMsg "Not logged in user!"
    registration       = logMsg "Registration"
    loggedIn u _ _ _ t _ _ = logMsg (join [str u, " ", t])


-- | Change user state, if the user state is logged in
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState f = do
  state <- CMS.get
  case state of
    UserNotLoggedIn -> return ()
    state' -> CMS.put (f state')

loadUserData :: Username -> String -> Page -> UserStory ()
loadUserData uname t p = do
  info <- withPersist $ \p -> personalInfo p uname
  flip personalInfoCata info $ \r n tz -> do
    CMS.put $ UserState {
        user = uname
      , page = p
      , name = n
      , role = r
      , token = t
      , timezone = tz
      , status = Nothing
      }

userState :: UserStory UserState
userState = CMS.get

submitSolution :: AssignmentKey -> Submission -> UserStory ()
submitSolution ak s = logAction INFO ("submits solution for assignment " ++ show ak) $ do
  authorize P_Open   P_Assignment
  authorize P_Create P_Submission
  checkActiveAssignment
  withUserAndPersist $ \u p -> do
    sk <- saveSubmission p ak u s
    return ()
  where
    checkActiveAssignment :: UserStory ()
    checkActiveAssignment = do
      a <- Bead.Controller.UserStories.loadAssignment ak
      now <- liftIO getCurrentTime
      unless (isActivePeriod a now) $
        errorPage "Submission is closed for the assignment"


availableGroups :: UserStory [(GroupKey, GroupDesc)]
availableGroups = logAction INFO "lists available assignments" $ do
  authorize P_Open P_Group
  withPersist $ \p -> (mapM (R.groupDescription p . fst)) =<< (R.filterGroups p each)
  where
    each _ _ = True

userAssignmentKeys :: UserStory [AssignmentKey]
userAssignmentKeys = logAction INFO "lists its assignments" $ do
  authorize P_Open P_Assignment
  uname <- CMS.gets user
  withPersist $ \p -> R.userAssignmentKeys p uname

userSubmissionKeys :: AssignmentKey -> UserStory [SubmissionKey]
userSubmissionKeys ak = logAction INFO msg $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  withUserAndPersist $ \u p -> R.userSubmissions p u ak
  where
    msg = "lists the submissions for assignment " ++ show ak

submissionDetailsDesc :: SubmissionKey -> UserStory SubmissionDetailsDesc
submissionDetailsDesc sk = logAction INFO msg $ do
  authPerms submissionDetailsDescPermissions
  withPersist $ \p -> R.submissionDetailsDesc p sk
  where
    msg = "loads information about submission " ++ show sk

loadSubmission :: SubmissionKey -> UserStory Submission
loadSubmission sk = logAction INFO ("loads submission " ++ show sk) $ do
  authorize P_Open P_Submission
  withPersist $ \p -> R.loadSubmission p sk

userAssignments :: UserStory [(AssignmentKey, AssignmentDesc)]
userAssignments = logAction INFO "lists assignments" $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Course
  authorize P_Open P_Group
  now <- liftIO getCurrentTime
  withUserAndPersist $
    \u p -> catMaybes <$> (mapM (createDesc p now) =<< R.userAssignmentKeys p u)

  where

    -- Produces the assignment description if the assignment is active
    --   Nothing if the Urn assignment is not in the active state
    createDesc :: Persist -> UTCTime -> AssignmentKey -> TIO (Maybe (AssignmentKey, AssignmentDesc))
    createDesc p now ak = do
      a <- R.loadAssignment p ak
      case and [assignmentType a == Urn, not $ isActivePeriod a now] of
        True -> return Nothing
        False -> do
          (name, adminNames) <- R.courseNameAndAdmins p ak
          let desc = AssignmentDesc {
            aActive = isActivePeriod a now
          , aTitle  = assignmentName a
          , aTeachers = adminNames
          , aGroup  = name
          }
          return $ Just (ak, desc)

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = logAction INFO msg $ do
  authPerms submissionDescPermissions
  withPersist $ \p -> submissionDesc p sk
  where
    msg = "loads submission infomation for " ++ show sk

openSubmissions :: UserStory [SubmissionKey]
openSubmissions = logAction INFO ("lists unevaulated submissions") $ do
  authorize P_Open P_Submission
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
submissionListDesc ak = logAction INFO ("lists submissions for assignment " ++ show ak) $ do
  authPerms submissionListDescPermissions
  withUserAndPersist $ \uname p -> R.submissionListDesc p uname ak

submissionTables :: UserStory [SubmissionTableInfo]
submissionTables = logAction INFO "lists submission tables" $ do
  authPerms submissionTableInfoPermissions
  withUserAndPersist $ \uname p -> R.submissionTables p uname

-- TODO: Check if the user can evaulates only submissions that
-- are submitted for the assignment created by the user
newEvaulation :: SubmissionKey -> Evaulation -> UserStory ()
newEvaulation sk e = logAction INFO ("saves new evaulation for " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Evaulation
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    a <- isAdminedSubmission p u sk
    when a $ do
      R.saveEvaulation p sk e
      R.removeFromOpened p sk
      R.saveComment p sk (evaulationComment now e)
      return ()

modifyEvaulation :: EvaulationKey -> Evaulation -> UserStory ()
modifyEvaulation ek e = logAction INFO ("modifies evaulation " ++ show ek) $ do
  authorize P_Modify P_Evaulation
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    sk <- submissionOfEvaulation p ek
    a <- isAdminedSubmission p u sk
    when a $ do
      R.modifyEvaulation p ek e
      saveComment p sk (evaulationComment now e)
      return ()

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = logAction INFO ("comments on " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Comment
  withUserAndPersist $ \u p -> do
    can <- canUserCommentOn p u sk
    when can $ do
      saveComment p sk c
      return ()

userSubmissions :: Username -> AssignmentKey -> UserStory (Maybe UserSubmissionDesc)
userSubmissions s ak = logAction INFO msg $ do
  authPerms userSubmissionDescPermissions
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
  where
    msg = join ["lists ",show s,"'s submissions for assignment ", show ak]

modifyAssignment :: AssignmentKey -> Assignment -> UserStory ()
modifyAssignment ak a = logAction INFO ("modifies assignment " ++ show ak) $ do
  authorize P_Modify P_Assignment
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

