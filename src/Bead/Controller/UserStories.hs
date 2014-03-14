{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Bead.Controller.UserStories where

import           Bead.Domain.Entities     as E
import           Bead.Domain.Relationships
import           Bead.Domain.RolePermission (permission)
import           Bead.Domain.Types
import           Bead.Controller.ServiceContext
import           Bead.Controller.Logging  as L
import           Bead.Controller.Pages    as P
import           Bead.Persistence.Persist (Persist)
import qualified Bead.Persistence.Persist as Persist
import qualified Bead.Persistence.Relations as Persist
import           Bead.View.Snap.Translation

import           Control.Applicative
import           Control.Exception
import           Control.Monad (filterM, forM_, when, unless)
import           Control.Monad.Error (Error(..))
import           Control.Concurrent.MVar
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Error  as CME
import qualified Control.Monad.Reader as CMR
import           Control.Monad.Trans
import           Control.Monad (join)
import           Prelude hiding (log, userError)
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Data.List (nub)
import           Data.Maybe (catMaybes)
import           Data.Time (UTCTime(..), getCurrentTime)
import           Numeric (showHex)
import           Text.Printf (printf)

import Control.Monad.Transaction.TIO

-- User error can be a message that need to be displayed, or
-- a parametrized message with a string parameter that needs
-- to be resolved in the place where the message is rendered
newtype UserError = UserError TransMsg
  deriving (Show)

-- Template method for the UserError functions
userErrorCata f (UserError t) = f t

-- Creates a user error that contains a non-parametrized message
userError :: Translation String -> UserError
userError = UserError . TransMsg

-- Creates a user error that contains a parametrized message, with one parameter
userParamError :: Translation String -> String -> UserError
userParamError t p = UserError (TransPrmMsg t p)

-- Creates a user error that contains a parametrized message, with 2 parameters
userPrm2Error :: Translation String -> String -> String -> UserError
userPrm2Error t p1 p2 = UserError (TransPrm2Msg t p1 p2)

-- Creates a user error that contains a parametrized message, with 3 parameters
userPrm3Error :: Translation String -> String -> String -> String -> UserError
userPrm3Error t p1 p2 p3 = UserError (TransPrm3Msg t p1 p2 p3)

-- Translates the given user error with the given translation function,
-- applying the parameters if necessary to the parametrized messages
translateUserError :: (Translation String -> String) -> UserError -> String
translateUserError = userErrorCata . translateMessage

instance Error UserError where
  noMsg    = userError (Msg_UserStoryError_UnknownError "Unknown Error: No message.")
  strMsg m = userParamError (Msg_UserStoryError_Message "Some error happened: %s") m

-- The User Story Context contains a service context and the localization transformation.
-- The service context is used for user manipulation.
-- The localization is used for translation of the messages that will be stored in
--  the persistence layer
type UserStoryContext = (ServiceContext, I18N)

newtype UserStory a = UserStory {
    unStory :: CMR.ReaderT UserStoryContext (CMS.StateT UserState (CME.ErrorT UserError IO)) a
  } deriving (Monad, CMS.MonadState UserState
                   , CME.MonadError UserError
                   , CMR.MonadReader UserStoryContext
                   , Functor
                   , Applicative
                   , MonadIO)

runUserStory
  :: ServiceContext
  -> I18N
  -> UserState
  -> UserStory a
  -> IO (Either UserError (a, UserState))
runUserStory context i18n userState
  = CME.runErrorT
  . flip CMS.runStateT userState
  . flip CMR.runReaderT (context,i18n)
  . unStory

-- * High level user stories

-- | The user logs in with a given username and password
--   QUESTION: Is there multiple login for the given user?
--   ANSWER:   No, the user can log in once at a time
login :: Username -> String -> UserStory ()
login username token = do
  withUsername username $ \uname ->
    logMessage INFO $ concat [uname, " is trying to login, with session ", token, " ."]
  usrContainer <- asksUserContainer
  validUser    <- persistence $ Persist.doesUserExist username
  notLoggedIn  <- liftIO $ isUserLoggedIn usrContainer (userToken (username, token))
  case (validUser, notLoggedIn) of
    (True, False) -> do
      loadUserData username token P.Home
      s <- userState
      liftIO $ userLogsIn usrContainer (userToken s) s
    (True , True)  -> errorPage . userError $ Msg_UserStoryError_SameUserIsLoggedIn "This user is logged in somewhere else."
    (False,    _)  -> errorPage . userError $ Msg_UserStoryError_InvalidUsernameOrPassword "Invalid username or password."

-- | The user logs out
logout :: UserStory ()
logout = do
  state <- userState
  users <- asksUserContainer
  liftIO $ userLogsOut users (userToken state)
  CMS.put userNotLoggedIn

doesUserExist :: Username -> UserStory Bool
doesUserExist u = logAction INFO ("searches after user " ++ show u) $ do
  authorize P_Open P_User
  persistence $ Persist.doesUserExist u

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
  persistence $ Persist.saveUser newUser
  logger      <- asksLogger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

-- Updates the timezone of the current user
setTimeZone :: TimeZone -> UserStory ()
setTimeZone tz = do
  changeUserState $ \userState -> userState { timezone = tz }
  putStatusMessage $ Msg_UserStory_SetTimeZone "The time zone has been set."

-- Updates the current user's full name, timezone and language in the persistence layer
changeUserDetails :: String -> TimeZone -> Language -> UserStory ()
changeUserDetails name timezone language = logAction INFO ("changes fullname, timezone and language") $ do
  user <- currentUser
  persistence $ Persist.updateUser user { u_name = name , u_timezone = timezone , u_language = language }
  putStatusMessage $ Msg_UserStory_ChangedUserDetails "The user details have been updated."

updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("updates user " ++ (str . u_username $ u)) $ do
  authorize P_Modify P_User
  persistence $ Persist.updateUser u

-- | Selecting users that satisfy the given criteria
selectUsers :: (User -> Bool) -> UserStory [User]
selectUsers f = logAction INFO "selects some users" $ do
  authorize P_Open P_User
  persistence $ Persist.filterUsers f

-- | Load another user's data if the current user is authorized to open
-- other users' profile
loadUser :: Username -> UserStory User
loadUser u = logAction INFO "Loading user information" $ do
  authorize P_Open P_User
  persistence $ Persist.loadUser u

-- Returns the username who is active in the current userstory
username :: UserStory Username
username = CMS.gets user

-- The UserStory calculation returns the current user's profile data
currentUser :: UserStory User
currentUser = logAction INFO "Load the current user's data" $ do
  u <- user <$> userState
  persistence $ Persist.loadUser u

-- Saves (copies) a file to the actual directory from the given filepath
-- which will be determined. If the user has no permission for the uploading
-- an error is thrown
saveUsersFile :: FilePath -> UsersFile -> UserStory ()
saveUsersFile tempPath usersfile = logAction INFO logMessage $ do
  authorize P_Create P_File
  u <- username
  persistence $ Persist.copyFile u tempPath usersfile
  where
    logMessage = usersFileCata (\u -> " uploads a file " ++ show u) usersfile

-- List all the user's file. If the user has no permission for the listing
-- of files an error is thrown
listUsersFiles :: UserStory [(UsersFile, FileInfo)]
listUsersFiles = logAction INFO " lists all his files" $ do
  authorize P_Open P_File
  u <- username
  persistence $ Persist.listFiles u

-- Returns the user's data file real path, for further processing, if
-- the user has authentication, otherwise throws an error page
getFilePath :: UsersFile -> UserStory FilePath
getFilePath usersfile = logAction INFO logMessage $ do
  authorize P_Open P_File
  u <- username
  persistence $ Persist.getFile u usersfile
  where
    logMessage = usersFileCata (\u -> " asks the file path: " ++ show u) usersfile

-- Produces true if the given user is the student of the actual course or group
courseOrGroupStudent :: Username -> UserStory Bool
courseOrGroupStudent student = logAction INFO
  (concat ["Student ", str student, " of the actual user"])
  ((elem student . concatMap stiUsers) <$> submissionTables)

administratedCourses :: UserStory [(CourseKey, Course)]
administratedCourses = logAction INFO "selects adminstrated courses" $ do
  authorize P_Open P_Course
  u <- username
  persistence $ Persist.administratedCourses u

-- Produces a list of group keys, group and the full name of the group
administratedGroups :: UserStory [(GroupKey, Group, String)]
administratedGroups = logAction INFO "selects administrated groups" $ do
  authorize P_Open P_Group
  u <- username
  persistence $ Persist.administratedGroupsWithCourseName u

-- | The 'create' function is an abstract function
--   for other creators like, createCourse and createExercise
create
  :: (PermissionObj o)
  => (o -> k -> String)      -- ^ Descriptor for the logger
  -> (o -> TIO k)            -- ^ Saver function of the persistence
  -> o                       -- ^ The object to save
  -> UserStory k
create descriptor saver object = do
  authorize P_Create (permissionObject object)
  key <- persistence (saver object)
  logMessage INFO $ descriptor object key
  return key

createUserReg :: UserRegistration -> UserStory UserRegKey
createUserReg u = logAction INFO "Creates user registration" $ do
  create descriptor Persist.saveUserReg u
  where
    descriptor x _ = reg_username x

loadUserReg :: UserRegKey -> UserStory UserRegistration
loadUserReg k = logAction INFO "Loading user registration" $ do
  authorize P_Open P_UserReg
  persistence $ Persist.loadUserReg k

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse course = logAction INFO "creates course" $ do
  authorize P_Create P_Course
  key <- create descriptor Persist.saveCourse course
  putStatusMessage $ Msg_UserStory_CreateCourse "The course has been created."
  return key
  where
    descriptor course _ =
      printf "Course is created: %s"
        (show (courseName course))

selectCourses :: (CourseKey -> Course -> Bool) -> UserStory [(CourseKey, Course)]
selectCourses f = logAction INFO "selects some courses" $ do
  authorize P_Open P_Course
  persistence $ Persist.filterCourses f

loadCourse :: CourseKey -> UserStory (Course,[GroupKey])
loadCourse k = logAction INFO ("loads course: " ++ show k) $ do
  authorize P_Open P_Course
  persistence $ do
    c  <- Persist.loadCourse k
    ks <- Persist.groupKeysOfCourse k
    return (c,ks)

createCourseAdmin :: Username -> CourseKey -> UserStory ()
createCourseAdmin u ck = logAction INFO "sets user to course admin" $ do
  authorize P_Create P_CourseAdmin
  authorize P_Open   P_User
  persistence $ Persist.createCourseAdmin u ck
  putStatusMessage $ Msg_UserStory_SetCourseAdmin "The user has become a course administrator."
  where
    user = usernameCata id

-- Deletes the given users from the given course if the current user is a course
-- admin for the given course, otherwise redirects to the error page
deleteUsersFromCourse :: CourseKey -> [Username] -> UserStory ()
deleteUsersFromCourse ck sts = logAction INFO ("deletes users from course: " ++ show ck) $ do
  authorize P_Modify P_Course
  u <- username
  join $ persistence $ do
    cs <- map fst <$> Persist.administratedCourses u
    case ck `elem` cs of
      False -> return . errorPage . userError $ Msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."
      True -> do
        mapM_ (Persist.deleteUserFromCourse ck) sts
        return . putStatusMessage $
          Msg_UserStory_UsersAreDeletedFromCourse "The students have been removed from the course."

-- Saves the given test script associated with the given course, if the
-- current user have authorization for the operation and if he administrates the
-- course given in the parameter. If authorization violation happens the page
-- redirects to the error page
saveTestScript :: CourseKey -> TestScript -> UserStory ()
saveTestScript ck ts = logAction INFO ("creates new test script for course: " ++ show ck) $ do
  authorize P_Create P_TestScript
  user <- username
  join $ persistence $ do
    cs <- map fst <$> Persist.administratedCourses user
    case ck `elem` cs of
      False -> return . errorPage . userError $ Msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."
      True -> do
        Persist.saveTestScript ck ts
        return . putStatusMessage $
          Msg_UserStory_NewTestScriptIsCreated "The test script has been created."

-- Overwrite the test script with the given one if the current user administrates
-- the course that are of the given test script otherwise redirects to the error page
modifyTestScript :: TestScriptKey -> TestScript -> UserStory ()
modifyTestScript tsk ts = logAction INFO ("modifies the existing test script: " ++ show tsk) $ do
  authorize P_Modify P_TestScript
  user <- username
  join $ persistence $ do
    cs <- map fst <$> Persist.administratedCourses user
    ck <- Persist.courseOfTestScript tsk
    case ck `elem` cs of
      False -> return . errorPage . userError $ Msg_UserStoryError_NoAssociatedTestScript "You are trying to modify someone else's test script."
      True -> do
        Persist.modifyTestScript tsk ts
        return . putStatusMessage $
          Msg_UserStory_ModifyTestScriptIsDone "The test script has been updated."

-- | Loads the test script if the user has authorization for the load, and
-- otherwise redirects to the error page
loadTestScript :: TestScriptKey -> UserStory (TestScript, CourseKey)
loadTestScript tsk = logAction INFO ("loads the test script: " ++ show tsk) $ do
  authorize P_Open P_TestScript
  join $ persistence $ do
    ck <- Persist.courseOfTestScript tsk
    ts <- Persist.loadTestScript tsk
    return (return (ts, ck))

-- | Returns Just test case key and test case for the given assignment if there any, otherwise Nothing
testCaseOfAssignment :: AssignmentKey -> UserStory (Maybe (TestCaseKey, TestCase, TestScriptKey))
testCaseOfAssignment ak = logAction INFO (" loads the test case for assignment: " ++ show ak) $ do
  join $ persistence $ do
    mtk <- Persist.testCaseOfAssignment ak
    maybe
      (return (return Nothing))
      (\tk -> do tc  <- Persist.loadTestCase tk
                 tsk <- Persist.testScriptOfTestCase tk
                 return (return (Just (tk, tc, tsk))))
      mtk

-- | Returns the test scrips of the given assignments, that are attached to the course of the assignment
testScriptInfosOfAssignment :: AssignmentKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfAssignment ak = do
  authorize P_Open P_TestScript
  join $ persistence $ do
    keys <- Persist.courseOrGroupOfAssignment ak
    ck   <- either (return) Persist.courseOfGroup keys
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return (return tss)
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given group, that are arrached to the course of the group
testScriptInfosOfGroup :: GroupKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfGroup gk = do
  authorize P_Open P_TestScript
  join $ persistence $ do
    ck   <- Persist.courseOfGroup gk
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return (return tss)
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given course
testScriptInfosOfCourse :: CourseKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfCourse ck = do
  authorize P_Open P_TestScript
  join $ persistence $ do
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return (return tss)
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- Deletes the given users from the given group if the current user is a group
-- admin for the given group, otherwise redirects to the error page
deleteUsersFromGroup :: GroupKey -> [Username] -> UserStory ()
deleteUsersFromGroup gk sts = logAction INFO ("delets users form group: " ++ show gk) $ do
  authorize P_Modify P_Group
  u <- username
  join $ persistence $ do
    gs <- map fst <$> Persist.administratedGroups u
    case gk `elem` gs of
      False -> return . errorPage . userError $ Msg_UserStoryError_NoGroupAdminOfGroup "You are not a group admin for the group."
      True -> do
        ck <- Persist.courseOfGroup gk
        mapM_ (\student -> Persist.unsubscribe student ck gk) sts
        return . putStatusMessage $
          Msg_UserStory_UsersAreDeletedFromGroup "The students have been removed from the group."

createGroupAdmin :: Username -> GroupKey -> UserStory ()
createGroupAdmin u gk = logAction INFO "sets user as a group admin of a group" $ do
  authorize P_Create P_GroupAdmin
  authorize P_Open   P_User
  groupAdminSetted <- persistence $ do
    info <- Persist.personalInfo u
    flip personalInfoCata info $ \role _name _tz ->
      if (groupAdmin role)
        then Persist.createGroupAdmin u gk >> return True
        else return False
  if groupAdminSetted
    then putStatusMessage $ Msg_UserStory_SetGroupAdmin "The user has become a teacher."
    else CME.throwError $ userParamError (Msg_UserStoryError_NoGroupAdmin "%s is not a group admin!") (user u)
  where
    user = usernameCata id

-- Unsubscribes the student from the given group (and course) if the group is one of the student's group
-- and the sutdent did not submit any solutions for the assignments of the group. In that
-- case the error page is rendered
unsubscribeFromCourse :: GroupKey -> UserStory ()
unsubscribeFromCourse gk = logAction INFO ("unsubscribes from group: " ++ show gk) $ do
  u <- username
  join $ persistence $ do
    registered <- Persist.isUserInGroup u gk
    case registered of
      False -> return . errorPage . userError $ Msg_UserStoryError_NoGroupAdminOfGroup "You are not group admin for the group."
      True -> do
        ck <- Persist.courseOfGroup gk
        s <- (&&) <$> Persist.isThereASubmissionForGroup u gk
                  <*> Persist.isThereASubmissionForCourse u ck
        if s then (return . errorPage . userError $ Msg_UserStoryError_AlreadyHasSubmission "You have already submitted some solution for the assignments of the course.")
             else do
               Persist.unsubscribe u ck gk
               return . putStatusMessage $
                 Msg_UserStory_SuccessfulCourseUnsubscription "Unregistration was successful."

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("creats group " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  key <- persistence $ Persist.saveGroup ck g
  putStatusMessage $ Msg_UserStory_CreateGroup "The group has been created."
  return key

loadGroup :: GroupKey -> UserStory Group
loadGroup gk = logAction INFO ("loads group " ++ show gk) $ do
  authorize P_Open P_Group
  persistence $ Persist.loadGroup gk

-- | Checks is the user is subscribed for the group
isUserInGroup :: GroupKey -> UserStory Bool
isUserInGroup gk = logAction INFO ("checks if user is in the group " ++ show gk) $ do
  authorize P_Open P_Group
  state <- userState
  persistence $ Persist.isUserInGroup (user state) gk

-- | Checks if the user is subscribed for the course
isUserInCourse :: CourseKey -> UserStory Bool
isUserInCourse ck = logAction INFO ("checks if user is in the course " ++ show ck) $ do
  authorize P_Open P_Course
  state <- userState
  persistence $ Persist.isUserInCourse (user state) ck

-- | Regsiter the user in the group, if the user does not submitted
-- any solutions for the other groups of the actual course, otherwise
-- puts a message on the UI, indicating that the course change is
-- not allowed.
subscribeToGroup :: GroupKey -> UserStory ()
subscribeToGroup gk = logAction INFO ("subscribes to the group " ++ (show gk)) $ do
  authorize P_Open P_Group
  state <- userState
  message <- persistence $ do
    let u = user state
    ck  <- Persist.courseOfGroup gk
    gks <- Persist.groupsOfUsersCourse u ck
    hasSubmission <- isThereASubmission u gks
    case hasSubmission of
      True -> return $ Msg_UserStory_SubscribedToGroup_ChangeNotAllowed
        "It is not possible to move between groups as there are submission for the current group."
      False -> do
        mapM_ (Persist.unsubscribe u ck) gks
        Persist.subscribe u ck gk
        return $ Msg_UserStory_SubscribedToGroup "Successful registration."
  putStatusMessage message
  where
    isThereASubmission u gks = do
      aks <- concat <$> mapM Persist.groupAssignments gks
      (not . null . catMaybes) <$> (mapM (flip Persist.lastSubmission u) aks)

-- Returns a list of elements of group key, description and a boolean value indicating
-- that the user already submitted a solution for the group or the course of the group
attendedGroups :: UserStory [(GroupKey, GroupDesc, Bool)]
attendedGroups = logAction INFO "selects courses attended in" $ do
  authorize P_Open P_Group
  uname <- username
  persistence $ do
    ks <- Persist.userGroups uname
    ds <- mapM Persist.groupDescription ks
    mapM (isThereASubmissionDesc uname) ds
  where
    isThereASubmissionDesc u (gk, desc) = do
      ck <- Persist.courseOfGroup gk
      s <- (||) <$> Persist.isThereASubmissionForGroup u gk
                <*> Persist.isThereASubmissionForCourse u ck
      return (gk,desc,s)

testCaseModificationForAssignment :: AssignmentKey -> TCModification -> UserStory ()
testCaseModificationForAssignment ak = tcModificationCata noModification fileOverwrite textOverwrite tcDelete where
  noModification = return ()

  fileOverwrite tsk uf = do
    u <- username
    persistence $ do
      let usersFileName = usersFileCata id uf
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ""
            , tcType        = TestCaseZipped
            , tcInfo        = usersFileName
            }
      mtk <- Persist.testCaseOfAssignment ak
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk
      Persist.copyTestCaseFile tk u uf
    return ()

  textOverwrite tsk t = do
    persistence $ do
      a <- Persist.loadAssignment ak
      let name = assignmentName a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = BS.pack t
            , tcType        = TestCaseSimple
            , tcInfo        = ""
            }
      mtk <- Persist.testCaseOfAssignment ak
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk

  tcDelete = do
    persistence $ do
      mtk <- Persist.testCaseOfAssignment ak
      case mtk of
        Nothing -> return ()
        Just tk -> Persist.removeTestCaseAssignment tk ak

-- Interprets the TCCreation value, copying a binary file or filling up the
-- normal test case file with the plain value, creating the test case for the
-- given assingment
testCaseCreationForAssignment :: AssignmentKey -> TCCreation -> UserStory ()
testCaseCreationForAssignment ak = tcCreationCata noCreation fileCreation textCreation where

  noCreation = return ()

  fileCreation tsk usersfile = do
    u <- username
    persistence $ do
      let usersFileName = usersFileCata id usersfile
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ""
            , tcType        = TestCaseZipped
            , tcInfo        = usersFileName
            }
      tk <- Persist.saveTestCase tsk ak testCase
      Persist.copyTestCaseFile tk u usersfile
    return ()

  -- Set plain text as test case value
  textCreation tsk plain = do
    persistence $ do
      a <- Persist.loadAssignment ak
      let name = assignmentName a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = BS.pack plain
            , tcType        = TestCaseSimple
            , tcInfo        = ""
            }
      Persist.saveTestCase tsk ak testCase
    return ()

createGroupAssignment :: GroupKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createGroupAssignment gk a tc = logAction INFO msg $ do
  authorize P_Open   P_Group
  authorize P_Create P_Assignment
  when (null $ assignmentName a) $
    errorPage . userError $ Msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ assignmentDesc a) $
    errorPage . userError $ Msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."
  ak <- create descriptor (Persist.saveGroupAssignment gk) a
  testCaseCreationForAssignment ak tc
  statusMsg a
  return ak
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)
    msg = "creates assignment for group " ++ show gk
    statusMsg = assignmentCata $ \name _ _ _ _ _ _ ->
      putStatusMessage $ Msg_UserStory_NewGroupAssignment "The group assignment has been created."

createCourseAssignment :: CourseKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createCourseAssignment ck a tc = logAction INFO msg $ do
  authorize P_Open P_Course
  authorize P_Create P_Assignment
  when (null $ assignmentName a) $
    errorPage . userError $ Msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ assignmentDesc a) $
    errorPage . userError $ Msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."
  ak <- create descriptor (Persist.saveCourseAssignment ck) a
  testCaseCreationForAssignment ak tc
  statusMsg a
  return ak
  where
    descriptor _ key = printf "Exercise is created with id: %s" (str key)
    msg = "creates assignment for course " ++ show ck
    statusMsg = assignmentCata $ \name _ _ _ _ _ _ ->
      putStatusMessage $ Msg_UserStory_NewCourseAssignment "The course assignment has been created."

selectAssignments :: (AssignmentKey -> Assignment -> Bool) -> UserStory [(AssignmentKey, Assignment)]
selectAssignments f = logAction INFO "selects some assignments" $ do
  authorize P_Open P_Assignment
  persistence $ Persist.filterAssignment f

-- | The 'loadExercise' loads an exercise from the persistence layer
loadAssignment :: AssignmentKey -> UserStory Assignment
loadAssignment k = logAction INFO ("loads assignment " ++ show k) $ do
  authorize P_Open P_Assignment
  persistence $ Persist.loadAssignment k

-- Puts the given status message to the actual user state
putStatusMessage :: Translation String -> UserStory ()
putStatusMessage = changeUserState . setStatus

-- Clears the status message of the user
clearStatusMessage :: UserStory ()
clearStatusMessage = changeUserState clearStatus

-- Logs the error message into the logfile and, also throw as an error
errorPage :: UserError -> UserStory ()
errorPage e = do
  logMessage ERROR $ translateUserError trans e
  CME.throwError e

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
      errorPage $ userError (Msg_UserStoryError_UserIsNotLoggedIn "The user is not logged in.")

    Left RegRole -> case elem (p,o) regPermObjects of
      True  -> return ()
      False -> errorPage $ userPrm2Error
        (Msg_UserStoryError_RegistrationProcessError $ unlines [
           "During the registration process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Left TestAgentRole -> case elem (p,o) testAgentPermObjects of
      True -> return ()
      False -> errorPage $ userPrm2Error
        (Msg_UserStoryError_TestAgentError $ unlines [
           "During the automated testing process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Right r -> case permission r p o of
      True  -> return ()
      False -> errorPage $ userPrm3Error
        (Msg_UserStoryError_AuthenticationNeeded "Authentication needed %s %s %s")
          (show r) (show p) (show o)
  where
    regPermObjects = [
        (P_Create, P_User),    (P_Open, P_User)
      , (P_Create, P_UserReg), (P_Open, P_UserReg)
      ]

    testAgentPermObjects = [
        (P_Open, P_TestIncoming), (P_Open, P_Submission), (P_Create, P_Comment)
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
      testAgent
      loggedIn
  where
    logMsg preffix =
      asksLogger >>= (\lgr -> (liftIO $ log lgr level $ join [preffix, " ", msg, "."]))

    userNotLoggedIn    = logMsg "[USER NOT LOGGED IN]"
    registration       = logMsg "[REGISTRATION]"
    testAgent          = logMsg "[TEST AGENT]"
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
  info <- persistence $ Persist.personalInfo uname
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
  withUserAndPersist $ \u -> do
    removeUserOpenedSubmissions u ak
    sk <- Persist.saveSubmission ak u s
    Persist.saveTestJob sk
    return ()
  where
    checkActiveAssignment :: UserStory ()
    checkActiveAssignment = do
      a <- Bead.Controller.UserStories.loadAssignment ak
      now <- liftIO getCurrentTime
      unless (isActivePeriod a now) . errorPage . userError $
        Msg_UserStoryError_SubmissionDeadlineIsReached "The submission deadline is reached."

    removeUserOpenedSubmissions u ak = do
      sks <- Persist.usersOpenedSubmissions ak u
      mapM_ (Persist.removeFromOpened ak u) sks

-- Returns all the group for that the user does not submitted a soultion already
availableGroups :: UserStory [(GroupKey, GroupDesc)]
availableGroups = logAction INFO "lists available groups" $ do
  authorize P_Open P_Group
  u <- username
  persistence $ do
    allGroups <- map fst <$> Persist.filterGroups each
    available <- filterM (thereIsNoSubmission u) allGroups
    mapM Persist.groupDescription available
  where
    each _ _ = True
    thereIsNoSubmission u gk = not <$> Persist.isThereASubmissionForGroup u gk

-- Produces a list that contains the assignments for the actual user,
-- if the user is not subscribed to a course or group the list
-- will be empty.
userAssignmentKeys :: UserStory [AssignmentKey]
userAssignmentKeys = logAction INFO "lists its assignments" $ do
  authorize P_Open P_Assignment
  uname <- username
  persistence $ Persist.userAssignmentKeyList uname

userSubmissionKeys :: AssignmentKey -> UserStory [SubmissionKey]
userSubmissionKeys ak = logAction INFO msg $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  withUserAndPersist $ \u -> Persist.userSubmissions u ak
  where
    msg = "lists the submissions for assignment " ++ show ak

submissionDetailsDesc :: SubmissionKey -> UserStory SubmissionDetailsDesc
submissionDetailsDesc sk = logAction INFO msg $ do
  authPerms submissionDetailsDescPermissions
  persistence $ Persist.submissionDetailsDesc sk
  where
    msg = "loads information about submission " ++ show sk

loadSubmission :: SubmissionKey -> UserStory Submission
loadSubmission sk = logAction INFO ("loads submission " ++ show sk) $ do
  authorize P_Open P_Submission
  persistence $ Persist.loadSubmission sk

-- Produces a list of assignments and information about the submissions for the
-- described assignment
userAssignments :: UserStory (Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)])
userAssignments = logAction INFO "lists assignments" $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Course
  authorize P_Open P_Group
  now <- liftIO getCurrentTime
  withUserAndPersist $ \u -> do
    maybe (return Nothing) (fmap (Just . catMaybes) . (mapM (createDesc u now))) =<< (Persist.userAssignmentKeys u)

  where

    -- Produces the assignment description if the assignment is active
    --   Nothing if the Urn assignment is not in the active state
    createDesc :: Username -> UTCTime -> AssignmentKey -> Persist (Maybe (AssignmentKey, AssignmentDesc, SubmissionInfo))
    createDesc u now ak = do
      a <- Persist.loadAssignment ak
      case (now < assignmentStart a) of
        True -> return Nothing
        False -> do
          (name, adminNames) <- Persist.courseNameAndAdmins ak
          let desc = AssignmentDesc {
            aActive = isActivePeriod a now
          , aTitle  = assignmentName a
          , aTeachers = adminNames
          , aGroup  = name
          , aEndDate = assignmentEnd a
          }
          si <- Persist.userLastSubmissionInfo u ak
          return $ Just (ak, desc, si)

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = logAction INFO msg $ do
  authPerms submissionDescPermissions
  persistence $ Persist.submissionDesc sk
  where
    msg = "loads submission infomation for " ++ show sk

openSubmissions :: UserStory [(SubmissionKey, SubmissionDesc)]
openSubmissions = logAction INFO ("lists unevaluated submissions") $ do
  authorize P_Open P_Submission
  withUserAndPersist $ \uname -> do
    cs <- (map fst) <$> Persist.administratedCourses uname
    gs <- (map fst) <$> Persist.administratedGroups  uname
    cas <- concat <$> mapM Persist.courseAssignments cs
    gas <- concat <$> mapM Persist.groupAssignments gs
    let as = nub (cas ++ gas)
        adminFor (_,a,_) = elem a as
    nonEvaluated <- Persist.openedSubmissions
    assignments  <- mapM Persist.assignmentOfSubmission nonEvaluated
    descriptions <- mapM Persist.submissionDesc nonEvaluated
    return $ map select $ filter adminFor $ zip3 nonEvaluated assignments descriptions
  where
    select (a,_,c) = (a,c)

submissionListDesc :: AssignmentKey -> UserStory SubmissionListDesc
submissionListDesc ak = logAction INFO ("lists submissions for assignment " ++ show ak) $ do
  authPerms submissionListDescPermissions
  withUserAndPersist $ \uname -> Persist.submissionListDesc uname ak

courseSubmissionTable :: CourseKey -> UserStory SubmissionTableInfo
courseSubmissionTable ck = logAction INFO ("gets submission table for course " ++ show ck) $ do
  authPerms submissionTableInfoPermissions
  persistence $ Persist.courseSubmissionTableInfo ck

submissionTables :: UserStory [SubmissionTableInfo]
submissionTables = logAction INFO "lists submission tables" $ do
  authPerms submissionTableInfoPermissions
  withUserAndPersist $ Persist.submissionTables

-- Calculates the test script infos for the given course
testScriptInfos :: CourseKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfos ck = persistence $
  mapM testScriptInfoAndKey =<< (Persist.testScriptsOfCourse ck)
  where
    testScriptInfoAndKey tk = do
      ts <- Persist.testScriptInfo tk
      return (tk,ts)

newEvaluation :: SubmissionKey -> Evaluation -> UserStory ()
newEvaluation sk e = logAction INFO ("saves new evaluation for " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Evaluation
  now <- liftIO $ getCurrentTime
  userData <- currentUser
  i18n <- asksI18N
  msg <- withUserAndPersist $ \u -> do
    a <- Persist.isAdminedSubmission u sk
    case a of
      True -> do
        mek <- Persist.evaluationOfSubmission sk
        case mek of
          Nothing -> do
            Persist.saveEvaluation sk e
            Persist.removeOpenedSubmission sk
            Persist.saveComment sk (evaluationComment i18n now userData e)
            return Nothing
          Just _ -> return . Just $ Msg_UserStory_AlreadyEvaluated
            "Other admin just evaluated this submission"
      False -> do
        return Nothing

  maybe (return ()) putStatusMessage msg

modifyEvaluation :: EvaluationKey -> Evaluation -> UserStory ()
modifyEvaluation ek e = logAction INFO ("modifies evaluation " ++ show ek) $ do
  authorize P_Modify P_Evaluation
  now <- liftIO $ getCurrentTime
  userData <- currentUser
  i18n <- asksI18N
  withUserAndPersist $ \u -> do
    sk <- Persist.submissionOfEvaluation ek
    a <- Persist.isAdminedSubmission u sk
    when a $ do
      Persist.modifyEvaluation ek e
      Persist.saveComment sk (evaluationComment i18n now userData e)
      return ()

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = logAction INFO ("comments on " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Comment
  withUserAndPersist $ \u -> do
    can <- Persist.canUserCommentOn u sk
    when can $ do
      Persist.saveComment sk c
      return ()

-- Test agent user story, that reads out all the comments that the test daemon left
-- and saves the comments
testAgentComments :: UserStory ()
testAgentComments = do
  authorize P_Open P_TestIncoming
  authorize P_Open P_Submission
  authorize P_Create P_Comment
  persistence $ do
    comments <- Persist.testComments
    forM_ comments $ \(sk,c) -> do
      Persist.saveComment sk c
      Persist.deleteTestComment sk

userSubmissions :: Username -> AssignmentKey -> UserStory (Maybe UserSubmissionDesc)
userSubmissions s ak = logAction INFO msg $ do
  authPerms userSubmissionDescPermissions
  withUserAndPersist $ \u -> do
    -- The admin can see the submission of students who are belonging to him
    courses <- (map fst) <$> Persist.administratedCourses u
    groups  <- (map fst) <$> Persist.administratedGroups  u
    courseStudents <- concat <$> mapM Persist.subscribedToCourse courses
    groupStudents  <- concat <$> mapM Persist.subscribedToGroup  groups
    let students = nub (courseStudents ++ groupStudents)
    case elem s students of
      False -> return Nothing
      True  -> Just <$> Persist.userSubmissionDesc s ak
  where
    msg = join ["lists ",show s,"'s submissions for assignment ", show ak]

modifyAssignment :: AssignmentKey -> Assignment -> TCModification -> UserStory ()
modifyAssignment ak a tc = logAction INFO ("modifies assignment " ++ show ak) $ do
  authorize P_Modify P_Assignment
  withUserAndPersist $ \u -> do
    courseOrGroup <- Persist.courseOrGroupOfAssignment ak
    ownedAssignment <- case courseOrGroup of
      Left  ck -> (elem ck . map fst) <$> Persist.administratedCourses u
      Right gk -> (elem gk . map fst) <$> Persist.administratedGroups  u
    when ownedAssignment $ Persist.modifyAssignment ak a
    -- TODO: Log invalid access
  testCaseModificationForAssignment ak tc


-- * User Story combinators

-- * Tools

asksUserContainer :: UserStory (UserContainer UserState)
asksUserContainer = CMR.asks (userContainer . fst)

asksLogger :: UserStory Logger
asksLogger = CMR.asks (logger . fst)

asksPersistMutex :: UserStory (MVar ())
asksPersistMutex = CMR.asks (persist . fst)

asksI18N :: UserStory I18N
asksI18N = CMR.asks snd

-- | The 'logAction' first logs the message after runs the given operation
logAction :: LogLevel -> String -> UserStory a -> UserStory a
logAction level msg s = do
  logMessage level (concat [msg, " ..."])
  x <- s
  logMessage level (concat [msg, " ... DONE"])
  return x

withUserAndPersist :: (Username -> Persist a) -> UserStory a
withUserAndPersist f = do
  u <- username
  persistence (f u)

-- | Lifting a persistence action, if some error happens
-- during the action we create a unique hash ticket and we display
-- the ticket to the user, and log the original message with the
-- ticket itself
persistence :: Persist a -> UserStory a
persistence m = do
  mp <- asksPersistMutex
  x <- liftIO . try . modifyMVar mp $ \p -> do
         ea <- Persist.runPersist m
         return (p,ea)
  case x of
    (Left e) -> do
      -- Exception happened somewhere
      up <- userPart
      let err = showSomeException e
      let xid = encodeMessage (concat [up, " ", err])
      logMessage ERROR $ concat ["Exception in persistence layer: ", err, " XID: ", xid]
      CME.throwError $ userParamError
        (Msg_UserStoryError_XID "Some internal error happened, XID: %s")
        xid
    (Right (Left e)) -> do
      -- No exception but error processing the persistence command
      up <- userPart
      let xid = encodeMessage (concat [up, " ", e])
      logMessage ERROR $ concat ["Persistence error: ", e, "XID: ", xid]
      CME.throwError $ userParamError
        (Msg_UserStoryError_XID "Some internal error happened, XID: %s")
        xid
    (Right (Right x)) -> return x -- Everything went fine
  where
    showSomeException :: SomeException -> String
    showSomeException = show

    encodeMessage :: String -> String
    encodeMessage = flip showHex "" . abs . hash

    userPart = (userStateCata userNotLoggedIn registration testAgent loggedIn) <$> CMS.get
      where
        userNotLoggedIn    = "Not logged in user!"
        registration       = "Registration"
        testAgent          = "Test Agent"
        loggedIn u _ _ _ t _ _ = concat [str u, " ", t]
