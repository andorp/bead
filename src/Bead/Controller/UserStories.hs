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
logout :: Username -> UserStory ()
logout username = do
  users <- CMR.asks userContainer
  liftIO $ userLogsOut users username
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

  persistence <- CMR.asks persist
  key         <- liftIOE $ saver persistence object
  logger      <- CMR.asks logger
  liftIO $ log logger INFO $ descriptor object key

  return key


-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse = create descriptor saveCourse
  where
    descriptor course _ =
      printf "Course is created: %s (%s)"
        (courseName course)
        (show (courseCode course))

-- | Logically deletes an existing cousrse
deleteCourse :: CourseKey -> UserStory ()
deleteCourse = undefined

-- | Updates the course information
updateCourse :: CourseKey -> Course -> UserStory ()
updateCourse = undefined

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup = undefined

-- | Regsiter the user as a group intendee
registerInAGroup :: User -> GroupKey -> UserStory ()
registerInAGroup = undefined

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
    descriptor _ key = printf "Exercise is created with id: " (str key)

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
logMessage lvl msg = do
  l <- CMR.asks logger
  liftIO $ L.log l lvl msg

-- | Change user state, if the user state is logged in
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState f = do
  state <- CMS.get
  case state of
    UserNotLoggedIn -> return ()
    state' -> CMS.put (f state')

renderPageData :: Page -> UserStory ()
renderPageData page = do
  renderData <- pageData page
  changeUserState $ \userState -> userState { pageRenderData = renderData }

  where
    pageData Home = return $ HomePageData [] [] []
    pageData _    = return $ NoPageData

loadUserData :: Username -> Password -> Page -> UserStory ()
loadUserData uname pwd p =  do
  persistence <- CMR.asks persist
  (userRole, userFamilyName) <- liftIOE $ personalInfo persistence uname pwd
  CMS.put UserState { 
              user = uname
            , page = p
            , name = userFamilyName
            , role = userRole
            , availablePages = []
            , pageRenderData = NoPageData
            }
  renderPageData p

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
