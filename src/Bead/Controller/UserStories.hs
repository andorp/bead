{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Controller.UserStories where

import           Bead.Domain.Entities hiding (name, uid)
import qualified Bead.Domain.Entities as Entity (name, uid)
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Relationships
import           Bead.Domain.RolePermission (permission)
import           Bead.Controller.ServiceContext
import           Bead.Controller.Logging  as L
import           Bead.Controller.Pages    as P
import           Bead.Persistence.Persist (Persist)
import qualified Bead.Persistence.Persist   as Persist
import qualified Bead.Persistence.Relations as Persist
import qualified Bead.Persistence.Guards    as Persist
import           Bead.View.Translation

import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding (guard)
import           Control.Monad.Error (Error(..))
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Error  as CME
import qualified Control.Monad.Reader as CMR
import           Control.Monad.Trans
import           Prelude hiding (log, userError)
import           Data.Hashable
import           Data.List (nub)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime(..), getCurrentTime)
import           Numeric (showHex)
import           Text.Printf (printf)

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
  noMsg    = userError (msg_UserStoryError_UnknownError "Unknown Error: No message.")
  strMsg m = userParamError (msg_UserStoryError_Message "Some error happened: %s") m

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
      loadUserData username token home'
      s <- userState
      liftIO $ userLogsIn usrContainer (userToken s) s
    (True , True)  -> errorPage . userError $ msg_UserStoryError_SameUserIsLoggedIn "This user is logged in somewhere else."
    (False,    _)  -> errorPage . userError $ msg_UserStoryError_InvalidUsernameOrPassword "Invalid username or password."
  where
    home' = home ()

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
changePage :: P.PageDesc -> UserStory ()
changePage p = do
  authorize P_Open (pageAsPermObj p)
  changeUserState $ \userState -> userState { page = p }
  where
    pageAsPermObj p
      | isAdministration p = P_AdminPage
      | otherwise          = P_PlainPage

-- | The authorized user creates a new user
createUser :: User -> UserStory ()
createUser newUser = do
  authorize P_Create P_User
  persistence $ Persist.saveUser newUser
  logger      <- asksLogger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

-- Updates the timezone of the current user
setTimeZone :: TimeZoneName -> UserStory ()
setTimeZone tz = do
  changeUserState $ \userState -> userState { timezone = tz }
  putStatusMessage $ msg_UserStory_SetTimeZone "The time zone has been set."

-- Updates the current user's full name, timezone and language in the persistence layer
changeUserDetails :: String -> TimeZoneName -> Language -> UserStory ()
changeUserDetails name timezone language = logAction INFO ("changes fullname, timezone and language") $ do
  user <- currentUser
  persistence $ Persist.updateUser user { u_name = name , u_timezone = timezone , u_language = language }
  putStatusMessage $ msg_UserStory_ChangedUserDetails "The user details have been updated."

-- Updates the user information
updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("updates user " ++ (usernameCata id $ u_username u)) $ do
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
    msg u = " uploads a file " ++ show u
    logMessage = usersFile msg msg usersfile

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
    msg u = " asks the file path: " ++ show u
    logMessage = usersFile msg msg usersfile

-- Produces true if the given user is the student of the courses or groups which
-- the actual user administrates.
isStudentOfMine :: Username -> UserStory Bool
isStudentOfMine student = logAction INFO (concat ["Student ", usernameCata id student, " of the actual user"]) $ do
  authorize P_Modify P_StudentPassword
  u <- username
  persistence $ Persist.isStudentOf student u

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
  => (o -> v -> String)      -- ^ Descriptor for the logger
  -> o                       -- ^ The object to save
  -> (o -> Persist v)        -- ^ Saver function of the persistence
  -> UserStory v
create descriptor object saver = do
  authorize P_Create (permissionObject object)
  value <- persistence (saver object)
  logMessage INFO $ descriptor object value
  return value

#ifndef SSO
createUserReg :: UserRegistration -> UserStory UserRegKey
createUserReg u = logAction INFO "Creates user registration" $ do
  create descriptor u Persist.saveUserReg
  where
    descriptor x _ = reg_username x

loadUserReg :: UserRegKey -> UserStory UserRegistration
loadUserReg k = logAction INFO "Loading user registration" $ do
  authorize P_Open P_UserReg
  persistence $ Persist.loadUserReg k
#endif

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse course = logAction INFO "creates course" $ do
  authorize P_Create P_Course
  key <- create descriptor course Persist.saveCourse
  putStatusMessage $ msg_UserStory_CreateCourse "The course has been created."
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
  putStatusMessage $ msg_UserStory_SetCourseAdmin "The user has become a course administrator."
  where
    user = usernameCata id

-- Produces a list of courses and users who administrators for the courses
courseAdministrators :: UserStory [(Course, [User])]
courseAdministrators = logAction INFO "lists the course and admins" $ do
  authorize P_Open P_Course
  authorize P_Open P_User
  persistence $ do
    Persist.filterCourses (\_ _ -> True) >>= (mapM $ \(ck,c) -> do
      admins <- mapM Persist.loadUser =<< Persist.courseAdmins ck
      return (c,admins))

-- Produces a list of courses that the user administrates and for all the courses
-- the list of groups and users who are groups admins for the given group
groupAdministrators :: UserStory [(Course, [(Group, [User])])]
groupAdministrators = logAction INFO "lists the groups and admins" $ do
  authorize P_Open P_Course
  authorize P_Open P_Group
  authorize P_Open P_User
  u <- username
  persistence $ do
    courses <- Persist.administratedCourses u
    forM courses $ \(ck,course) -> do
      gks <- Persist.groupKeysOfCourse ck
      groups <- mapM groupAndAdmins gks
      return (course, groups)
  where
    groupAndAdmins gk = do
      g <- Persist.loadGroup gk
      as <- mapM Persist.loadUser =<< Persist.groupAdmins gk
      return (g,as)

-- Deletes the given users from the given course if the current user is a course
-- admin for the given course, otherwise redirects to the error page
deleteUsersFromCourse :: CourseKey -> [Username] -> UserStory ()
deleteUsersFromCourse ck sts = logAction INFO ("deletes users from course: " ++ show ck) $ do
  authorize P_Modify P_Course
  u <- username
  join $ persistence $ do
    admined <- Persist.isAdministratedCourse u ck
    if admined
      then do mapM_ (Persist.deleteUserFromCourse ck) sts
              return . putStatusMessage $
                msg_UserStory_UsersAreDeletedFromCourse "The students have been removed from the course."
      else return $ do
             logMessage INFO . violation $ printf "The user tries to delete users from a course (%s) which not belongs to him" (courseKeyMap id ck)
             errorPage . userError $ msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."

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
      False -> return . errorPage . userError $ msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."
      True -> do
        Persist.saveTestScript ck ts
        return . putStatusMessage $
          msg_UserStory_NewTestScriptIsCreated "The test script has been created."

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
      False -> return . errorPage . userError $ msg_UserStoryError_NoAssociatedTestScript "You are trying to modify someone else's test script."
      True -> do
        Persist.modifyTestScript tsk ts
        return . putStatusMessage $
          msg_UserStory_ModifyTestScriptIsDone "The test script has been updated."

-- | Loads the test script if the user has authorization for the load, and
-- otherwise redirects to the error page
loadTestScript :: TestScriptKey -> UserStory (TestScript, CourseKey)
loadTestScript tsk = logAction INFO ("loads the test script: " ++ show tsk) $ do
  authorize P_Open P_TestScript
  persistence $ do
    ck <- Persist.courseOfTestScript tsk
    ts <- Persist.loadTestScript tsk
    return (ts, ck)

-- | Returns Just test case key and test case for the given assignment if there any, otherwise Nothing
testCaseOfAssignment :: AssignmentKey -> UserStory (Maybe (TestCaseKey, TestCase, TestScriptKey))
testCaseOfAssignment ak = logAction INFO (" loads the test case for assignment: " ++ show ak) $ do
  persistence $ do
    mtk <- Persist.testCaseOfAssignment ak
    maybe
      (return Nothing)
      (\tk -> do tc  <- Persist.loadTestCase tk
                 tsk <- Persist.testScriptOfTestCase tk
                 return (Just (tk, tc, tsk)))
      mtk

-- | Returns the test scrips of the given assignments, that are attached to the course of the assignment
testScriptInfosOfAssignment :: AssignmentKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfAssignment ak = do
  authorize P_Open P_TestScript
  persistence $ do
    keys <- Persist.courseOrGroupOfAssignment ak
    ck   <- either (return) Persist.courseOfGroup keys
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given group, that are arrached to the course of the group
testScriptInfosOfGroup :: GroupKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfGroup gk = do
  authorize P_Open P_TestScript
  persistence $ do
    ck   <- Persist.courseOfGroup gk
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given course
testScriptInfosOfCourse :: CourseKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfCourse ck = do
  authorize P_Open P_TestScript
  persistence $ do
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
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
    admined <- Persist.isAdministratedGroup u gk
    if admined
      then do ck <- Persist.courseOfGroup gk
              mapM_ (\student -> Persist.unsubscribe student ck gk) sts
              return . putStatusMessage $
                msg_UserStory_UsersAreDeletedFromGroup "The students have been removed from the group."
      else return $ do
             logMessage INFO . violation $ printf "The user tries to delete users from group (%s) which is not administrated by him" (groupKeyMap id gk)
             errorPage . userError $ msg_UserStoryError_NoGroupAdminOfGroup "You are not a group admin for the group."

createGroupAdmin :: Username -> GroupKey -> UserStory ()
createGroupAdmin u gk = logAction INFO "sets user as a group admin of a group" $ do
  authorize P_Create P_GroupAdmin
  authorize P_Open   P_User
  admin <- username
  join . persistence $ do
    info <- Persist.personalInfo u
    withPersonalInfo info $ \role _name _tz _ui -> do
      admined <- Persist.isAdministratedCourseOfGroup admin gk
      if (and [groupAdmin role, admined])
        then do Persist.createGroupAdmin u gk
                return $ putStatusMessage $ msg_UserStory_SetGroupAdmin "The user has become a teacher."
        else return . CME.throwError $ userParamError (msg_UserStoryError_NoGroupAdmin "%s is not a group admin!") (user u)
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
      False -> return . errorPage . userError $ msg_UserStoryError_NoGroupAdminOfGroup "You are not group admin for the group."
      True -> do
        ck <- Persist.courseOfGroup gk
        s <- (&&) <$> Persist.isThereASubmissionForGroup u gk
                  <*> Persist.isThereASubmissionForCourse u ck
        if s then (return . errorPage . userError $ msg_UserStoryError_AlreadyHasSubmission "You have already submitted some solution for the assignments of the course.")
             else do
               Persist.unsubscribe u ck gk
               return . putStatusMessage $
                 msg_UserStory_SuccessfulCourseUnsubscription "Unregistration was successful."

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("creats group " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  u <- username
  join $ persistence $ do
    admined <- Persist.isAdministratedCourse u ck
    if admined
      then do key <- Persist.saveGroup ck g
              return $ do
                putStatusMessage $ msg_UserStory_CreateGroup "The group has been created."
                return key
      else return . errorPage $ userError nonAdministratedCourse

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

-- | Lists all users subscribed for the given course
subscribedToCourse :: CourseKey -> UserStory [Username]
subscribedToCourse ck = logAction INFO ("lists all users in course " ++ show ck) $ do
  authorize P_Open P_Course
  isAdministratedCourse ck
  persistence $ Persist.subscribedToCourse ck

-- | Lists all users subscribed for the given group
subscribedToGroup :: GroupKey -> UserStory [Username]
subscribedToGroup gk = logAction INFO ("lists all users in group " ++ show gk) $ do
  authorize P_Open P_Group
  isAdministratedGroup gk
  persistence $ Persist.subscribedToGroup gk
                       

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
      True -> return $ msg_UserStory_SubscribedToGroup_ChangeNotAllowed
        "It is not possible to move between groups as there are submission for the current group."
      False -> do
        mapM_ (Persist.unsubscribe u ck) gks
        Persist.subscribe u ck gk
        return $ msg_UserStory_SubscribedToGroup "Successful registration."
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

testCaseModificationForAssignment :: Username -> AssignmentKey -> TCModification -> Persist ()
testCaseModificationForAssignment u ak = tcModificationCata noModification fileOverwrite textOverwrite tcDelete where
  noModification = return ()

  fileOverwrite tsk uf = do
      let usersFileName = usersFile id id uf
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ZippedTestCase ""
            , tcInfo        = usersFileName
            }
      mtk <- Persist.testCaseOfAssignment ak
      -- TODO: Join the test case creation and test case file copy
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk
      Persist.copyTestCaseFile tk u uf
      return ()

  textOverwrite tsk t = do
      a <- Persist.loadAssignment ak
      let name = Assignment.name a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = SimpleTestCase t
            , tcInfo        = ""
            }
      mtk <- Persist.testCaseOfAssignment ak
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk

  tcDelete = do
      mtk <- Persist.testCaseOfAssignment ak
      case mtk of
        Nothing -> return ()
        Just tk -> Persist.removeTestCaseAssignment tk ak

-- Interprets the TCCreation value, copying a binary file or filling up the
-- normal test case file with the plain value, creating the test case for the
-- given assingment
testCaseCreationForAssignment :: Username -> AssignmentKey -> TCCreation -> Persist ()
testCaseCreationForAssignment u ak = tcCreationCata noCreation fileCreation textCreation where

  noCreation = return ()

  fileCreation tsk usersfile = do
      let usersFileName = usersFile id id usersfile
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ZippedTestCase ""
            , tcInfo        = usersFileName
            }
      tk <- Persist.saveTestCase tsk ak testCase
      Persist.copyTestCaseFile tk u usersfile
      return ()

  -- Set plain text as test case value
  textCreation tsk plain = do
      a <- Persist.loadAssignment ak
      let name = Assignment.name a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = SimpleTestCase plain
            , tcInfo        = ""
            }
      Persist.saveTestCase tsk ak testCase
      return ()

createGroupAssignment :: GroupKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createGroupAssignment gk a tc = logAction INFO msg $ do
  authorize P_Open   P_Group
  authorize P_Create P_Assignment
  when (null $ Assignment.name a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ Assignment.desc a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."

  u <- username
  join . persistence $ do
    admined <- Persist.isAdministratedGroup u gk
    if admined
      then do ak <- Persist.saveGroupAssignment gk a
              testCaseCreationForAssignment u ak tc
              return $ do
                statusMsg a
                logMessage INFO $ descriptor ak
                return ak
      else return $ do
             logMessage INFO . violation $ printf "User tries to access to group: %s" (groupKeyMap id gk)
             errorPage $ userError nonAdministratedGroup
  where
    descriptor key = printf "Exercise is created with id: %s" (assignmentKeyMap id key)
    msg = "creates assignment for group " ++ show gk
    statusMsg = const .
      putStatusMessage $ msg_UserStory_NewGroupAssignment "The group assignment has been created."

createCourseAssignment :: CourseKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createCourseAssignment ck a tc = logAction INFO msg $ do
  authorize P_Open P_Course
  authorize P_Create P_Assignment
  when (null $ Assignment.name a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ Assignment.desc a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."

  u <- username
  join . persistence $ do
    admined <- Persist.isAdministratedCourse u ck
    if admined
      then do ak <- Persist.saveCourseAssignment ck a
              testCaseCreationForAssignment u ak tc
              return $ do
                statusMsg a
                logMessage INFO $ descriptor ak
                return ak
      else return $ do
             logMessage INFO . violation $ printf "User tries to access to course: %s" (courseKeyMap id ck)
             errorPage $ userError nonAdministratedCourse
  where
    descriptor key = printf "Exercise is created with id: %s" (assignmentKeyMap id key)
    msg = "creates assignment for course " ++ show ck
    statusMsg = const .
      putStatusMessage $ msg_UserStory_NewCourseAssignment "The course assignment has been created."

-- | The 'loadExercise' loads an exercise from the persistence layer
loadAssignment :: AssignmentKey -> UserStory Assignment
loadAssignment k = logAction INFO ("loads assignment " ++ show k) $ do
  authorize P_Open P_Assignment
  persistence $ Persist.loadAssignment k

createGroupAssessment :: GroupKey -> Assessment -> UserStory AssessmentKey
createGroupAssessment gk a = logAction INFO ("creates assessment for group " ++ show gk) $ do
  authorize P_Open P_Group
  authorize P_Create P_Assessment
  isAdministratedGroup gk
  persistence (Persist.saveGroupAssessment gk a)

createCourseAssessment :: CourseKey -> Assessment -> UserStory AssessmentKey
createCourseAssessment ck a = logAction INFO ("creates assessment for course " ++ show ck) $ do
  authorize P_Open P_Course
  authorize P_Create P_Assessment
  isAdministratedCourse ck
  persistence (Persist.saveCourseAssessment ck a)

saveScoresOfCourseAssessment :: CourseKey -> Assessment -> Map Username Evaluation -> UserStory ()
saveScoresOfCourseAssessment ck a evaluations = do
  ak <- createCourseAssessment ck a
  logAction INFO ("saves scores of assessment " ++ show ak ++ " of course " ++ show ck) $ do
    users <- subscribedToCourse ck
    persistence (mapM_ (saveEvaluation ak) users)
      where
        saveEvaluation ak user = case Map.lookup user evaluations of
                                   Just eval -> do sk <- Persist.saveScore user ak score
                                                   void $ Persist.saveScoreEvaluation sk eval
                                   Nothing   -> return ()

        score = Score ()

saveScoresOfGroupAssessment :: GroupKey -> Assessment -> Map Username Evaluation -> UserStory ()
saveScoresOfGroupAssessment gk a evaluations = do
  ak <- createGroupAssessment gk a
  logAction INFO ("saves scores of assessment " ++ show ak ++ " of group " ++ show gk) $ do
    users <- subscribedToGroup gk
    persistence (mapM_ (saveEvaluation ak) users)
      where
        saveEvaluation ak user = case Map.lookup user evaluations of
                                   Just eval -> do sk <- Persist.saveScore user ak score
                                                   void $ Persist.saveScoreEvaluation sk eval
                                   Nothing   -> return ()

        score = Score ()

-- Produces a map of assessments and information about the submissions for the
-- described assignment, which is associated with the course or group
userAssessments :: UserStory (Map CourseKey (Course, [(AssessmentKey, ScoreInfo)]))
userAssessments = logAction INFO "lists assessments" $ do
--  authorize P_Open P_Assessment
  authorize P_Open P_Course
  authorize P_Open P_Group
  withUserAndPersist $ \u -> do
    asgMap <- Persist.userAssessmentKeys u
    newMap <- forM (Map.toList asgMap) $ \(key,aks) -> do
      key' <- Persist.loadCourse key
      infos <- catMaybes <$> mapM (getInfo u) (Set.toList aks)
      return $! (key, (key', infos))
    return $! Map.fromList newMap

  where
    -- Produces the scoreinfo for the specific user and assessment.
    -- Returns Nothing if there are no scoreinfo or there are multiple ones.
    getInfo :: Username -> AssessmentKey -> Persist (Maybe (AssessmentKey, ScoreInfo))
    getInfo u ak = do
      scoreKeys <- Persist.scoreOfAssessmentAndUser u ak
      case scoreKeys of
        [sk] -> do info <- Persist.scoreInfo sk
                   return . Just $ (ak,info)
        _    -> return Nothing

scoreBoards :: UserStory (Map (Either CourseKey GroupKey) ScoreBoard)
scoreBoards = logAction INFO "lists scoreboards" $ do
  authPerms scoreBoardPermissions
  withUserAndPersist Persist.scoreBoards

-- Puts the given status message to the actual user state
putStatusMessage :: Translation String -> UserStory ()
putStatusMessage = changeUserState . setStatus . SmNormal

-- Puts the given message as the error status message to the actual user state
putErrorMessage :: Translation String -> UserStory ()
putErrorMessage = changeUserState . setStatus . SmError

-- Clears the status message of the user
clearStatusMessage :: UserStory ()
clearStatusMessage = changeUserState clearStatus

-- Logs the error message into the logfile and, also throw as an error
errorPage :: UserError -> UserStory a
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
      errorPage $ userError (msg_UserStoryError_UserIsNotLoggedIn "The user is not logged in.")

    Left RegRole -> case elem (p,o) regPermObjects of
      True  -> return ()
      False -> errorPage $ userPrm2Error
        (msg_UserStoryError_RegistrationProcessError $ unlines [
           "During the registration process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Left TestAgentRole -> case elem (p,o) testAgentPermObjects of
      True -> return ()
      False -> errorPage $ userPrm2Error
        (msg_UserStoryError_TestAgentError $ unlines [
           "During the automated testing process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Right r -> case permission r p o of
      True  -> return ()
      False -> errorPage $ userPrm3Error
        (msg_UserStoryError_AuthenticationNeeded "Authentication needed %s %s %s")
          (show r) (show p) (show o)
  where
    regPermObjects = [
        (P_Create, P_User),    (P_Open, P_User)
      , (P_Create, P_UserReg), (P_Open, P_UserReg)
      , (P_Modify, P_User)
      ]

    testAgentPermObjects = [
        (P_Open, P_TestIncoming), (P_Open, P_Submission), (P_Create, P_Feedback)
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
    loggedIn _ u _ _ _ t _ _ = logMsg (join [Entity.uid id u, " ", t])


-- | Change user state, if the user state is logged in
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState f = do
  state <- CMS.get
  case state of
    UserNotLoggedIn -> return ()
    state' -> CMS.put (f state')

loadUserData :: Username -> String -> PageDesc -> UserStory ()
loadUserData uname t p = do
  info <- persistence $ Persist.personalInfo uname
  flip personalInfoCata info $ \r n tz ui -> do
    CMS.put $ UserState {
        user = uname
      , uid = ui
      , page = p
      , name = n
      , role = r
      , token = t
      , timezone = tz
      , status = Nothing
      }

userState :: UserStory UserState
userState = CMS.get

submitSolution :: AssignmentKey -> Submission -> UserStory SubmissionKey
submitSolution ak s = logAction INFO ("submits solution for assignment " ++ show ak) $ do
  authorize P_Open   P_Assignment
  authorize P_Create P_Submission
  checkActiveAssignment
  join $ withUserAndPersist $ \u -> do
    attended <- Persist.isUsersAssignment u ak
    if attended
      then do removeUserOpenedSubmissions u ak
              sk <- Persist.saveSubmission ak u s
              Persist.saveTestJob sk
              return (return sk)
      else return $ do
             logMessage INFO . violation $
               printf "The user tries to submit a solution for an assignment which not belongs to him: (%s)" (assignmentKeyMap id ak)
             errorPage $ userError nonRelatedAssignment
  where
    checkActiveAssignment :: UserStory ()
    checkActiveAssignment = do
      a <- Bead.Controller.UserStories.loadAssignment ak
      now <- liftIO getCurrentTime
      unless (Assignment.isActive a now) . errorPage . userError $
        msg_UserStoryError_SubmissionDeadlineIsReached "The submission deadline is reached."

    -- TODO: Change the ABI to remove the unevaluated automatically
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
-- will be empty. The map consist of all the assignment key groupped by the
-- course key, through group keys if necessary.
userAssignmentKeys :: UserStory (Map CourseKey (Set AssignmentKey))
userAssignmentKeys = logAction INFO "lists its assignments" $ do
  authorize P_Open P_Assignment
  uname <- username
  persistence $ Persist.userAssignmentKeys uname

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

-- Checks if the submission is accessible for the user and loads it,
-- otherwise throws an exception.
getSubmission :: SubmissionKey -> UserStory (Submission, SubmissionDesc)
getSubmission sk = logAction INFO ("downloads submission " ++ show sk) $ do
  authorize P_Open P_Submission
  isAccessibleSubmission sk
  persistence $ do
    s <- Persist.loadSubmission sk
    d <- Persist.submissionDesc sk
    return (s,d)

-- Creates a submission limit for the given assignment
assignmentSubmissionLimit :: AssignmentKey -> UserStory SubmissionLimit
assignmentSubmissionLimit key = logAction INFO msg $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  withUserAndPersist $ \user -> Persist.submissionLimitOfAssignment user key
  where
    msg = "user assignments submission Limit"

-- Loads the assignment and the assignment description if the given assignment key
-- refers an assignment accessible by the user for submission
userAssignmentForSubmission :: AssignmentKey -> UserStory (AssignmentDesc, Assignment)
userAssignmentForSubmission key = logAction INFO "check user assignment for submission" $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  isUsersAssignment key
  now <- liftIO getCurrentTime
  withUserAndPersist $ \user ->
    (,) <$> (assignmentDesc now user key) <*> (Persist.loadAssignment key)

-- Helper function which computes the assignment description
assignmentDesc :: UTCTime -> Username -> AssignmentKey -> Persist AssignmentDesc
assignmentDesc now user key = do
  a <- Persist.loadAssignment key
  limit <- Persist.submissionLimitOfAssignment user key
  let aspects = Assignment.aspects a
  (name, adminNames) <- Persist.courseNameAndAdmins key
  return $! AssignmentDesc {
      aActive = Assignment.isActive a now
    , aIsolated = Assignment.isIsolated aspects
    , aLimit = limit
    , aTitle  = Assignment.name a
    , aTeachers = adminNames
    , aGroup  = name
    , aEndDate = Assignment.end a
    }

-- Produces a map of assignments and information about the submissions for the
-- described assignment, which is associated with the course or group
userAssignments :: UserStory (Map CourseKey (Course,[(AssignmentKey, AssignmentDesc, SubmissionInfo)]))
userAssignments = logAction INFO "lists assignments" $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Course
  authorize P_Open P_Group
  now <- liftIO getCurrentTime
  withUserAndPersist $ \u -> do
    asgMap <- Persist.userAssignmentKeys u
    newMap <- forM (Map.toList asgMap) $ \(key,aks) -> do
      key' <- Persist.loadCourse key
      descs <- catMaybes <$> mapM (createDesc u now) (Set.toList aks)
      return $! (key, (key', descs))
    return $! Map.fromList newMap

  where
    -- Produces the assignment description if the assignment is active
    -- Returns Nothing if the assignment is not visible for the user
    createDesc :: Username -> UTCTime -> AssignmentKey -> Persist (Maybe (AssignmentKey, AssignmentDesc, SubmissionInfo))
    createDesc u now ak = do
      a <- Persist.loadAssignment ak
      case (now < Assignment.start a) of
        True -> return Nothing
        False -> do
          desc <- assignmentDesc now u ak
          si <- Persist.userLastSubmissionInfo u ak
          return $ (Just (ak, desc, si))

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = logAction INFO msg $ do
  authPerms submissionDescPermissions
  u <- username
  join . persistence $ do
    admined <- Persist.isAdministratedSubmission u sk
    if admined
      then do sd <- Persist.submissionDesc sk
              return (return sd)
      else return $ do
             logMessage INFO . violation $ printf "The user tries to evaluate a submission that not belongs to him."
             errorPage $ userError nonAdministratedSubmission
  where
    msg = "loads submission infomation for " ++ show sk

openSubmissions :: UserStory OpenedSubmissions
openSubmissions = logAction INFO ("lists unevaluated submissions") $ do
  authorize P_Open P_Submission
  u <- username
  persistence $ Persist.openedSubmissionInfo u

submissionListDesc :: AssignmentKey -> UserStory SubmissionListDesc
submissionListDesc ak = logAction INFO ("lists submissions for assignment " ++ show ak) $ do
  authPerms submissionListDescPermissions
  withUserAndPersist $ \uname -> Persist.submissionListDesc uname ak

courseSubmissionTable :: CourseKey -> UserStory SubmissionTableInfo
courseSubmissionTable ck = logAction INFO ("gets submission table for course " ++ show ck) $ do
  authPerms submissionTableInfoPermissions
  u  <- username
  join . persistence $ do
    admined <- Persist.isAdministratedCourse u ck
    if admined
      then do sti <- Persist.courseSubmissionTableInfo ck
              return (return sti)
      else return $ do
             logMessage INFO . violation $ printf "The user tries to open a course overview (%s) that is not administrated by him." (courseKeyMap id ck)
             errorPage $ userError nonAdministratedCourse

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
  join . withUserAndPersist $ \u -> do
    admined <- Persist.isAdminedSubmission u sk
    if admined
      then do mek <- Persist.evaluationOfSubmission sk
              case mek of
                Nothing -> do
                  Persist.saveSubmissionEvaluation sk e
                  Persist.removeOpenedSubmission sk
                  Persist.saveFeedback sk (evaluationToFeedback now userData e)
                  return (return ())
                Just _ -> return $ do
                            logMessage INFO "Other admin just evaluated this submission"
                            putStatusMessage $ msg_UserStory_AlreadyEvaluated
                              "Other admin just evaluated this submission"
      else return $ do
             logMessage INFO . violation $ printf "The user tries to save evalution for a submission (%s) that not belongs to him" (show sk)
             errorPage $ userError nonAdministratedSubmission

modifyEvaluation :: EvaluationKey -> Evaluation -> UserStory ()
modifyEvaluation ek e = logAction INFO ("modifies evaluation " ++ show ek) $ do
  authorize P_Modify P_Evaluation
  now <- liftIO $ getCurrentTime
  userData <- currentUser
  join . withUserAndPersist $ \u -> do
    sbk <- Persist.submissionOfEvaluation ek
    sck <- Persist.scoreOfEvaluation ek
    case (sbk, sck) of
      (Just _, Just _) -> return . errorPage $ strMsg "Impossible, submission and score have the same evaluation"
      (Nothing, Just _sk) -> do
        let admined = True
        if admined
          then do Persist.modifyEvaluation ek e
                  -- Persist.saveFeedback sk (evaluationToFeedback now userData e)
                  return (return ())
          else return $ do
                  logMessage INFO . violation $
                    printf "The user tries to modify an evaluation (%s) that not belongs to him."
                           (show ek)
                  errorPage $ userError nonAdministratedSubmission
      (Just sk, Nothing) -> do
        admined <- Persist.isAdminedSubmission u sk
        if admined
          then do Persist.modifyEvaluation ek e
                  Persist.saveFeedback sk (evaluationToFeedback now userData e)
                  return (return ())
          else return $ do
                  logMessage INFO . violation $
                    printf "The user tries to modify an evaluation (%s) that not belongs to him."
                           (show ek)
                  errorPage $ userError nonAdministratedSubmission
      (Nothing, Nothing) -> return (return ())

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = logAction INFO ("comments on " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Comment
  join $ withUserAndPersist $ \u -> do
    canComment <- Persist.canUserCommentOn u sk
    admined  <- Persist.isAdministratedSubmission u sk
    attended <- Persist.isUserSubmission u sk
    if (canComment && (admined || attended))
      then do Persist.saveComment sk c
              return (return ())
      else return $ do
              logMessage INFO . violation $ printf "The user tries to comment on a submission (%s) that not belongs to him" (show sk)
              errorPage . userError $ msg_UserStoryError_NonCommentableSubmission "The submission is not commentable"

#ifdef TEST
-- Insert test feedback with the TestAgent only for testing purposes.
insertTestFeedback :: SubmissionKey -> FeedbackInfo -> UserStory ()
insertTestFeedback sk fb = do
  authorize P_Create P_Feedback
  persistence $ do
    Persist.insertTestFeedback sk fb
    Persist.finalizeTestFeedback sk
#endif

-- Test agent user story, that reads out all the feedbacks that the test daemon left
-- and saves them
testAgentFeedbacks :: UserStory ()
testAgentFeedbacks = do
  authorize P_Open P_TestIncoming
  authorize P_Open P_Submission
  authorize P_Create P_Feedback
  persistence $ do
    feedbacks <- Persist.testFeedbacks
    forM_ feedbacks (uncurry Persist.saveFeedback)
    mapM_ Persist.deleteTestFeedbacks (nub $ map submission feedbacks)
  where
    submission = fst

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

-- Helper function: checks if there at least one submission for the given
isThereSubmissionPersist = fmap (not . null) . Persist.submissionsForAssignment

-- | Checks if there is at least one submission for the given assignment
isThereASubmission :: AssignmentKey -> UserStory Bool
isThereASubmission ak = logAction INFO ("" ++ show ak) $ do
  authorize P_Open P_Assignment
  isAdministratedAssignment ak
  persistence $ isThereSubmissionPersist ak

-- | Modify the given assignment but keeps the evaluation type if there is
-- a submission for the given assignment, also shows a warning message if the
-- modification of the assignment type is not viable.
modifyAssignment :: AssignmentKey -> Assignment -> TCModification -> UserStory ()
modifyAssignment ak a tc = logAction INFO ("modifies assignment " ++ show ak) $ do
  authorize P_Modify P_Assignment
  join . withUserAndPersist $ \u -> do
    admined <- Persist.isAdministratedAssignment u ak
    if admined
      then do hasSubmission <- isThereSubmissionPersist ak
              new <- if hasSubmission
                       then do -- Overwrite the assignment type with the old one
                               -- if there is submission for the given assignment
                               ev <- Assignment.evType <$> Persist.loadAssignment ak
                               return (a { Assignment.evType = ev })
                       else return a
              Persist.modifyAssignment ak new
              testCaseModificationForAssignment u ak tc
              if and [hasSubmission, Assignment.evType a /= Assignment.evType new]
                then return . putStatusMessage . msg_UserStory_EvalTypeWarning $ concat
                  [ "The evaluation type of the assignment is not modified. "
                  , "A solution is submitted already."
                  ]
                else (return (return ()))
      else return $ do
             logMessage INFO . violation $ printf "User tries to modify the assignment: (%s)" (assignmentKeyMap id ak)
             errorPage $ userError nonAdministratedAssignment

-- * Guards

-- Checks with the given guard function if the user has passed the guard,
-- otherwise logs a violation printf message, and renders the error page with
-- the given user error
guard
  :: (Show a)
  => (Username -> a -> Persist Bool) -- Guard
  -> String                          -- Violation printf message
  -> UserError                       -- Error for the error page
  -> a                               -- The value for the guard
  -> UserStory ()
guard g m e x = do
  u <- username
  join . persistence $ do
    passed <- g u x
    if passed
      then (return (return ()))
      else return $ do
             logMessage INFO . violation $ printf m (show x)
             errorPage e

-- Checks if the given course is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedCourse :: CourseKey -> UserStory ()
isAdministratedCourse = guard
  Persist.isAdministratedCourse
  "The user tries to access a course (%s) which is not administrated by him."
  (userError nonAdministratedCourse)

-- Checks if the given group is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedGroup :: GroupKey -> UserStory ()
isAdministratedGroup = guard
  Persist.isAdministratedGroup
  "The user tries to access a group (%s) which is not administrated by him."
  (userError nonAdministratedGroup)

-- Checks if the given assignment is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedAssignment :: AssignmentKey -> UserStory ()
isAdministratedAssignment = guard
  Persist.isAdministratedAssignment
  "The user tries to access an assignment (%s) which is not administrated by him."
  (userError nonAdministratedAssignment)

-- Checks if the given assignment is an assignment of a course or group that
-- the users attend otherwise, renders the error page
isUsersAssignment :: AssignmentKey -> UserStory ()
isUsersAssignment = guard
  Persist.isUsersAssignment
  "The user tries to access an assignment (%s) which is not of him's."
  (userError nonRelatedAssignment)

isAdministratedTestScript :: TestScriptKey -> UserStory ()
isAdministratedTestScript = guard
  Persist.isAdministratedTestScript
  "The user tries to access a test script (%s) which is not administrated by him."
  (userError nonAdministratedTestScript)

-- Checks if the submission is submitted by the user, or is part of a course
-- or group that the user administrates
isAccessibleSubmission :: SubmissionKey -> UserStory ()
isAccessibleSubmission = guard
  Persist.isAccessibleSubmission
  "The user tries to download a submission (%s) which is not accessible for him."
  (userError nonAccessibleSubmission)

doesBlockSubmissionView :: SubmissionKey -> UserStory ()
doesBlockSubmissionView = guard
  (const Persist.doesBlockSubmissionView)
  "The user tries to access a blocked submission (%s)."
  (userError blockedSubmission)

doesBlockAssignmentView :: AssignmentKey -> UserStory ()
doesBlockAssignmentView = guard
  (const Persist.doesBlockAssignmentView)
  "The user tries to access a blocked submissions (%s)."
  (userError blockedSubmission)

-- * User Story combinators

-- * Tools

-- Creates a message adding the "VIOLATION: " preffix to it
violation :: String -> String
violation = ("[GUARD VIOLATION]: " ++)

asksUserContainer :: UserStory (UserContainer UserState)
asksUserContainer = CMR.asks (userContainer . fst)

asksLogger :: UserStory Logger
asksLogger = CMR.asks (logger . fst)

asksPersistInterpreter :: UserStory Persist.Interpreter
asksPersistInterpreter = CMR.asks (persistInterpreter . fst)

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
  interp <- asksPersistInterpreter
  x <- liftIO . try $ Persist.runPersist interp m
  case x of
    (Left e) -> do
      -- Exception happened somewhere
      up <- userPart
      let err = showSomeException e
      let xid = encodeMessage (concat [up, " ", err])
      logMessage ERROR $ concat ["Exception in persistence layer: ", err, " XID: ", xid]
      CME.throwError $ userParamError
        (msg_UserStoryError_XID "Some internal error happened, XID: %s")
        xid
    (Right (Left e)) -> do
      -- No exception but error processing the persistence command
      up <- userPart
      let xid = encodeMessage (concat [up, " ", e])
      logMessage ERROR $ concat ["Persistence error: ", e, "XID: ", xid]
      CME.throwError $ userParamError
        (msg_UserStoryError_XID "Some internal error happened, XID: %s")
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
        loggedIn _ ui _ _ _ t _ _ = concat [Entity.uid id ui, " ", t]

-- * User Error Messages

nonAdministratedCourse = msg_UserStoryError_NonAdministratedCourse "The course is not administrated by you"
nonAdministratedGroup  = msg_UserStoryError_NonAdministratedGroup "This group is not administrated by you."
nonAdministratedAssignment = msg_UserStoryError_NonAdministratedAssignment "This assignment is not administrated by you."
nonAdministratedSubmission = msg_UserStoryError_NonAdministratedSubmission "The submission is not administrated by you."
nonAdministratedTestScript = msg_UserStoryError_NonAdministratedTestScript "The test script is not administrated by you."
nonRelatedAssignment = msg_UserStoryError_NonRelatedAssignment "The assignment is not belongs to you."
nonAccessibleSubmission = msg_UserStoryError_NonAccessibleSubmission "The submission is not belongs to you."
blockedSubmission = msg_UserStoryError_BlockedSubmission "The submission is blocked by an isolated assignment."
