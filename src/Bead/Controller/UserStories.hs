{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bead.Controller.UserStories where

import Bead.Domain.Entities     as E
import Bead.Domain.Relationships
import Bead.Domain.Types
import Bead.Controller.ServiceContext
import Bead.Controller.Logging  as L
import Bead.Controller.Pages    as P
import Bead.Persistence.Persist as R

import Control.Monad.Error (Error(..))
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Error  as CME
import qualified Control.Monad.Reader as CMR
import Control.Monad.Trans
import Control.Monad (join)
import Prelude hiding (log)
import Text.Printf (printf)

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
login :: Username -> Password -> UserStory ()
login username password = do
  prst         <- CMR.asks persist
  usrContainer <- CMR.asks userContainer
  validUser    <- liftIO $ R.doesUserExist prst username password
  notLoggedIn  <- liftIO $ isUserLoggedIn usrContainer username
  case (validUser, notLoggedIn) of
    (Left e    ,     _) -> errorPage "Internal error occurred"
    (Right True, False) -> do
      loadUserData username password P.Home
      s <- userState
      liftIO $ userLogsIn usrContainer username s
    (Right True , True)  -> errorPage "The user is logged in somewhere else"
    (Right False,    _)  -> errorPage "Invalid user and password combination"

-- | The user logs out
logout :: UserStory ()
logout = do
  state <- userState
  users <- CMR.asks userContainer
  liftIO $ userLogsOut users (user state)
  CMS.put UserNotLoggedIn

-- | The user submits a solution for the given exercise
submitSolution :: Stored ExerciseKey Exercise -> Solution -> UserStory SolutionKey
submitSolution = undefined

-- | The user navigates to the next page
changePage :: P.Page -> UserStory ()
changePage p = do
  authorize P_Open (pageAsPermObj p)
  changeUserState $ \userState -> userState { page = p }
  where
    pageAsPermObj P.Admin = P_AdminPage
    pageAsPermObj _       = P_PlainPage

-- | The user changes his/her password
changePassword :: Password -> Password -> Password -> UserStory ()
changePassword old new new'
  | new /= new' = do
      logErrorMessage "Password does not match"
      errorPage "Password does not match"
  | otherwise = do
      persistence <- CMR.asks persist
      username    <- CMS.gets user
      liftIOE $ updatePwd persistence username old new

-- | The authorized user creates a new user
createUser :: User -> Password -> UserStory ()
createUser newUser newPassword = do
  persistence <- CMR.asks persist
  liftIOE $ saveUser persistence newUser newPassword
  logger      <- CMR.asks logger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("Updating user:" ++ (str . u_username $ u)) $ do
  liftP $ flip R.updateUser u

-- | Selecting users that satisfy the given criteria
selectUsers :: (User -> Bool) -> UserStory [User]
selectUsers f = logAction INFO "Select some users" $ do
  authorize P_Open P_User
  liftP $ flip R.filterUsers f
  
loadUser :: Username -> UserStory User
loadUser u = logAction INFO "Loading user information" $ do
  authorize P_Open P_User
  liftP $ flip R.loadUser u

-- | The authorized user logically deletes the given user
--   QUESTION: What to do if the deleted user are logged in when the deletion does happen?
--   ANSWER: During his active session he can made authorized changes, after logging out
--           he will be not able to relogin.
deleteUser :: Username -> UserStory ()
deleteUser = undefined

-- | The 'create' function is an abstract function
--   for other creators like, createCourse and createExercise
create
  :: (PermissionObj o)
  => (o -> k -> String)                 -- ^ Descriptor for the logger
  -> (Persist -> o -> IO (Erroneous k)) -- ^ Saver function of the persistence
  -> o                                  -- ^ The object to save
  -> UserStory k
create descriptor saver object = do
  authorize P_Create (permissionObject object)
  key <- liftP (flip saver object)
  logMessage INFO $ descriptor object key
  return key

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse = create descriptor saveCourse
  where
    descriptor course _ =
      printf "Course is created: %s (%s)"
        (show (courseName course))
        (show (courseCode course))

selectCourses :: (CourseKey -> Course -> Bool) -> UserStory [(CourseKey, Course)]
selectCourses f = logAction INFO "Select Some Courses" $ do
  authorize P_Open P_Course
  liftP $ flip filterCourses f

loadCourse :: CourseKey -> UserStory (Course,[GroupKey])
loadCourse k = logAction INFO ("Loading course: " ++ show k) $ do
  authorize P_Open P_Course
  c  <- liftP $ flip R.loadCourse k
  ks <- liftP $ flip R.groupKeysOfCourse k
  return (c,ks)

-- | Logically deletes an existing cousrse
deleteCourse :: CourseKey -> UserStory ()
deleteCourse = undefined

-- | Updates the course information
updateCourse :: CourseKey -> Course -> UserStory ()
updateCourse = undefined

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("Creating group: " ++ show (groupCode g)) $ do
  authorize P_Create P_Group
  liftP $ \p -> R.saveGroup p ck g

-- | Checks is the user is subscribed for the group
isUserInGroup :: GroupKey -> UserStory Bool
isUserInGroup gk = do
  authorize P_Open P_Group
  state <- userState
  liftP $ \p -> R.isUserInGroup p (user state) gk

-- | Checks if the user is subscribed for the course
isUserInCourse :: CourseKey -> UserStory Bool
isUserInCourse ck = do
  authorize P_Open P_Course
  state <- userState
  liftP $ \p -> R.isUserInCourse p (user state) ck

-- | Regsiter the user as a group intendee
subscribeToGroup :: CourseKey -> GroupKey -> UserStory ()
subscribeToGroup ck gk = logAction INFO ("Subscribe to the group " ++ (show gk)) $ do
  authorize P_Open P_Group
  state <- userState
  liftP $ \p -> R.subscribe p (user state) ck gk

-- | Deletes logically the given course
deleteGroup :: GroupKey -> UserStory ()
deleteGroup = undefined

-- | Updates the group information
updateGroup :: GroupKey -> Group -> UserStory ()
updateGroup = undefined

-- | Creates an exercise
createExercise :: Exercise -> UserStory ExerciseKey
createExercise = create descriptor saveExercise
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)

selectExercises :: (ExerciseKey -> Exercise -> Bool) -> UserStory [(ExerciseKey, Exercise)]
selectExercises f = logAction INFO "Select Some Exercises" $ do
  authorize P_Open P_Exercise
  liftP $ flip filterExercises f

-- | The 'loadExercise' loads an exercise from the persistence layer
loadExercise :: ExerciseKey -> UserStory Exercise
loadExercise k = logAction INFO ("Loading exercise: " ++ show k) $ do
  authorize P_Open P_Exercise
  liftP $ flip R.loadExercise k

updateExercise :: ExerciseKey -> Exercise -> UserStory ()
updateExercise = undefined

deleteExercise :: ExerciseKey -> UserStory ()
deleteExercise = undefined

errorPage :: String -> UserStory ()
errorPage s = CME.throwError $ UserError s

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
  | pObject == P_Exercise && canCreate permission = adminAuthorization

  | otherwise = return ()
  where
    adminAuthorization = do
      userRole <- CMS.gets role
      case userRole >= E.CourseAdmin of
        False -> errorPage "Course Admin or Admin authorization is required"
        True  -> return ()

-- | Checks if the user is authorized for a given operation
isAuthorized :: Permission -> PermissionObject -> UserStory Bool
isAuthorized = undefined

-- | No operational User Story
noOperation :: UserStory ()
noOperation = return ()

-- | Log error message through the log subsystem
logErrorMessage :: String -> UserStory ()
logErrorMessage = logMessage ERROR

-- | Log a message through the log subsystem
logMessage :: LogLevel -> String -> UserStory ()
logMessage level msg = do
  user   <- CMS.gets (str . user)
  CMR.asks logger >>= (\lgr -> (liftIO $ log lgr level $ join [user, ": ", msg]))

-- | Change user state, if the user state is logged in
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState f = do
  state <- CMS.get
  case state of
    UserNotLoggedIn -> return ()
    state' -> CMS.put (f state')

loadUserData :: Username -> Password -> Page -> UserStory ()
loadUserData uname pwd p = do
  persistence <- CMR.asks persist
  (userRole, userFamilyName) <- liftIOE $ personalInfo persistence uname pwd
  CMS.put UserState {
              user = uname
            , page = p
            , name = userFamilyName
            , role = userRole
            }

userState :: UserStory UserState
userState = CMS.get

-- * User Story combinators

-- * Tools

liftIOE :: IO (Erroneous a) -> UserStory a
liftIOE a = do
  x <- liftIO a
  case x of
    Left err -> CME.throwError $ strMsg err
    Right x  -> return x

-- | The 'logAction' first logs the message after runs the given operation
logAction :: LogLevel -> String -> UserStory a -> UserStory a
logAction level msg s = do
  logMessage level msg
  s

-- | Lifting a persistence action
liftP :: (Persist -> IO (Erroneous a)) -> UserStory a
liftP f = CMR.asks persist >>= (liftIOE . f)
