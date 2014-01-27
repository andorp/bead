{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bead.Controller.UserStories where

import Bead.Domain.Entities     as E
import Bead.Domain.Relationships
import Bead.Domain.RolePermission (permission)
import Bead.Domain.Types
import Bead.Controller.ServiceContext
import Bead.Controller.Logging  as L
import Bead.Controller.Pages    as P
import Bead.Persistence.Persist (Persist(..))
import qualified Bead.Persistence.Persist as R
import Bead.View.Snap.Translation

import Control.Arrow
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
import Data.Map (Map)
import qualified Data.Map as Map
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
    (True , True)  -> errorPage "Ez a felhasználó máshonnan is be van jelentkezve"
    (False,    _)  -> errorPage "Rossz jelszó vagy felhasználónév"

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
  putStatusMessage $ Msg_UserStory_SetTimeZone $ printf "Az időzóna %s lett." (show tz)

-- Updates the current user's full name, timezone and language in the persistence layer
changeUserDetails :: String -> TimeZone -> Language -> UserStory ()
changeUserDetails name timezone language = logAction INFO ("changes fullname, timezone and language") $ do
  user <- currentUser
  withPersist $ flip R.updateUser user { u_name = name , u_timezone = timezone , u_language = language }
  putStatusMessage $ Msg_UserStory_ChangedUserDetails "A beállítások megváltoztak."

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

-- Produces true if the given user is the student of the actual one
courseOrGroupStudent :: Username -> UserStory Bool
courseOrGroupStudent student = logAction INFO
  (concat ["Student ", str student, " of the actual user"])
  ((elem student . concatMap stUsers) <$> submissionTables)

administratedCourses :: UserStory [(CourseKey, Course)]
administratedCourses = logAction INFO "selects adminstrated courses" $ do
  authorize P_Open P_Course
  u <- CMS.gets user
  withPersist $ flip R.administratedCourses u

-- Produces a list of group keys, group and the full name of the group
administratedGroups :: UserStory [(GroupKey, Group, String)]
administratedGroups = logAction INFO "selects administrated groups" $ do
  authorize P_Open P_Group
  u <- CMS.gets user
  withPersist $ flip R.administratedGroupsWithCourseName u

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
  putStatusMessage $ Msg_UserStory_CreateCourse $ printf "A(z) '%s' tárgy létrejött." (courseName course)
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
  putStatusMessage $ Msg_UserStory_SetCourseAdmin $ printf "%s most már tárgyfelelős." (user u)
  where
    user = usernameCata id

createGroupAdmin :: Username -> GroupKey -> UserStory ()
createGroupAdmin u gk = logAction INFO "sets user as a group admin of a group" $ do
  authorize P_Create P_GroupAdmin
  authorize P_Open   P_User
  groupAdminSetted <- withPersist $ \p -> do
    info <- R.personalInfo p u
    flip personalInfoCata info $ \role _name _tz ->
      if (groupAdmin role)
        then R.createGroupAdmin p u gk >> return True
        else return False
  if groupAdminSetted
    then putStatusMessage $ Msg_UserStory_SetGroupAdmin $ printf "%s most már oktató." (user u)
    else CME.throwError . strMsg $ printf "%s nem lehet oktató!" (user u)
  where
    user = usernameCata id

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("creats group " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  key <- withPersist $ \p -> R.saveGroup p ck g
  putStatusMessage $ Msg_UserStory_CreateGroup $ printf "A(z) '%s' csoport létrejött." (groupName g)
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
    let u = user state
    ck <- R.courseOfGroup p gk
    gks <- R.groupsOfUsersCourse p u ck
    mapM_ (R.unsubscribe p u ck) gks
    R.subscribe p u ck gk
  putStatusMessage $ Msg_UserStory_SubscribedToGroup "Sikeresen regisztráltál a csoportba!"

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
    statusMsg = assignmentCata $ \name _ _ _ _ _ _ ->
      putStatusMessage $ Msg_UserStory_NewGroupAssignment $ printf "Létrejött a(z) '%s' című csoportszintű feladat." name

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
    statusMsg = assignmentCata $ \name _ _ _ _ _ _ ->
      putStatusMessage $ Msg_UserStory_NewCourseAssignment $ printf "Létrejött a(z) '%s' című tárgyszintű feladat." name

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
putStatusMessage :: Translation String -> UserStory ()
putStatusMessage = changeUserState . setStatus

-- Clears the status message of the user
clearStatusMessage :: UserStory ()
clearStatusMessage = changeUserState clearStatus

errorPage :: String -> UserStory ()
errorPage s = CME.throwError $ UserError s

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
      errorPage "A felhasználó nincs bejelentkezve"

    Left RegRole -> case elem (p,o) regPermObjects of
      True  -> return ()
      False -> errorPage $ join [
          "Regisztrációs folyamat hibás működése miatt más folyamatot akar elérni "
        , show p, " ", show o
        ]

    Right r -> case permission r p o of
      True  -> return ()
      False -> errorPage $ join [
          "Azonosítás szükséges: ", show r, " "
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
    removeUserOpenedSubmissions p u ak
    sk <- saveSubmission p ak u s
    return ()
  where
    checkActiveAssignment :: UserStory ()
    checkActiveAssignment = do
      a <- Bead.Controller.UserStories.loadAssignment ak
      now <- liftIO getCurrentTime
      unless (isActivePeriod a now) $
        errorPage "A beküldési határidő lejárt"

    removeUserOpenedSubmissions p u ak = do
      sks <- R.usersOpenedSubmissions p ak u
      mapM_ (R.removeFromOpened p ak u) sks

availableGroups :: UserStory [(GroupKey, GroupDesc)]
availableGroups = logAction INFO "lists available assignments" $ do
  authorize P_Open P_Group
  withPersist $ \p -> (mapM (R.groupDescription p . fst)) =<< (R.filterGroups p each)
  where
    each _ _ = True

-- Produces a list that contains the assignments for the actual user,
-- if the user is not subscribed to a course or group the list
-- will be empty.
userAssignmentKeys :: UserStory [AssignmentKey]
userAssignmentKeys = logAction INFO "lists its assignments" $ do
  authorize P_Open P_Assignment
  uname <- CMS.gets user
  withPersist $ \p -> (R.userAssignmentKeyList p uname)

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

-- Produces a list of assignments and information about the submissions for the
-- described assignment
userAssignments :: UserStory (Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)])
userAssignments = logAction INFO "lists assignments" $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Course
  authorize P_Open P_Group
  now <- liftIO getCurrentTime
  withUserAndPersist $ \u p -> do
    maybe (return Nothing) (fmap (Just . catMaybes) . (mapM (createDesc p u now))) =<< (R.userAssignmentKeys p u)

  where

    -- Produces the assignment description if the assignment is active
    --   Nothing if the Urn assignment is not in the active state
    createDesc :: Persist -> Username -> UTCTime -> AssignmentKey -> TIO (Maybe (AssignmentKey, AssignmentDesc, SubmissionInfo))
    createDesc p u now ak = do
      a <- R.loadAssignment p ak
      case and [assignmentType a == Urn, now < assignmentStart a] of
        True -> return Nothing
        False -> do
          (name, adminNames) <- R.courseNameAndAdmins p ak
          let desc = AssignmentDesc {
            aActive = isActivePeriod a now
          , aTitle  = assignmentName a
          , aTeachers = adminNames
          , aGroup  = name
          , aEndDate = assignmentEnd a
          }
          si <- R.userLastSubmissionInfo p u ak
          return $ Just (ak, desc, si)

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = logAction INFO msg $ do
  authPerms submissionDescPermissions
  withPersist $ \p -> R.submissionDesc p sk
  where
    msg = "loads submission infomation for " ++ show sk

openSubmissions :: UserStory [(SubmissionKey, SubmissionDesc)]
openSubmissions = logAction INFO ("lists unevaluated submissions") $ do
  authorize P_Open P_Submission
  withUserAndPersist $ \uname p -> do
    cs <- (map fst) <$> R.administratedCourses p uname
    gs <- (map fst) <$> R.administratedGroups  p uname
    cas <- concat <$> mapM (courseAssignments p) cs
    gas <- concat <$> mapM (groupAssignments p) gs
    let as = nub (cas ++ gas)
        adminFor (_,a,_) = elem a as
    nonEvaluated <- R.openedSubmissions p
    assignments  <- mapM (assignmentOfSubmission p) nonEvaluated
    descriptions <- mapM (R.submissionDesc p) nonEvaluated
    return $ map select $ filter adminFor $ zip3 nonEvaluated assignments descriptions
  where
    select (a,_,c) = (a,c)

submissionListDesc :: AssignmentKey -> UserStory SubmissionListDesc
submissionListDesc ak = logAction INFO ("lists submissions for assignment " ++ show ak) $ do
  authPerms submissionListDescPermissions
  withUserAndPersist $ \uname p -> R.submissionListDesc p uname ak

submissionTables :: UserStory [SubmissionTableInfo]
submissionTables = logAction INFO "lists submission tables" $ do
  authPerms submissionTableInfoPermissions
  withUserAndPersist $ \uname p -> R.submissionTables p uname

newEvaluation :: SubmissionKey -> Evaluation -> UserStory ()
newEvaluation sk e = logAction INFO ("saves new evaluation for " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Evaluation
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    a <- R.isAdminedSubmission p u sk
    when a $ do
      R.saveEvaluation p sk e
      R.removeOpenedSubmission p sk
      R.saveComment p sk (evaluationComment now e)
      return ()

modifyEvaluation :: EvaluationKey -> Evaluation -> UserStory ()
modifyEvaluation ek e = logAction INFO ("modifies evaluation " ++ show ek) $ do
  authorize P_Modify P_Evaluation
  now <- liftIO $ getCurrentTime
  withUserAndPersist $ \u p -> do
    sk <- R.submissionOfEvaluation p ek
    a <- R.isAdminedSubmission p u sk
    when a $ do
      R.modifyEvaluation p ek e
      saveComment p sk (evaluationComment now e)
      return ()

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = logAction INFO ("comments on " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Comment
  withUserAndPersist $ \u p -> do
    can <- R.canUserCommentOn p u sk
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
         ea <- R.runPersist (m p)
         return (p,ea)
  case x of
    Left e -> do
      logMessage ERROR ("Persistence error: " ++ e)
      CME.throwError $ strMsg e
    Right x -> return x
