{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Bead.Persistence.NoSQLDir (
    Persist
  , Config
  , defaultConfig
  , Interpreter
  , runPersist
  , runInterpreter
  , ForeignKey(..)
  , saveUser
  , personalInfo
  , filterUsers
  , loadUser
  , updateUser
  , doesUserExist
  , userDescription
  , userSubmissions
  , administratedCourses
  , administratedGroups
  , scoresOfUser
  , attachNotificationToUser
  , notificationsOfUser

  , copyFile
  , listFiles
  , getFile

  , saveUserReg
  , loadUserReg

  , saveCourse
  , courseKeys
  , filterCourses
  , loadCourse
  , groupKeysOfCourse
  , isUserInCourse
  , userCourses
  , createCourseAdmin
  , courseAdmins
  , subscribedToCourse
  , unsubscribedFromCourse
  , testScriptsOfCourse

  , saveGroup
  , loadGroup
  , courseOfGroup
  , filterGroups
  , isUserInGroup
  , userGroups
  , subscribe
  , unsubscribe
  , groupAdmins
  , createGroupAdmin
  , subscribedToGroup
  , unsubscribedFromGroup

  , saveTestScript
  , loadTestScript
  , courseOfTestScript
  , modifyTestScript

  , saveTestCase
  , loadTestCase
  , testScriptOfTestCase
  , modifyTestCase
  , removeTestCaseAssignment
  , copyTestCaseFile
  , modifyTestScriptOfTestCase

  , saveTestJob

  , insertTestFeedback
  , testFeedbacks
  , deleteTestFeedbacks

  , filterAssignment
  , assignmentKeys
  , saveAssignment
  , loadAssignment
  , modifyAssignment
  , saveCourseAssignment
  , saveGroupAssignment
  , courseAssignments
  , groupAssignments
  , courseOfAssignment
  , groupOfAssignment
  , submissionsForAssignment
  , assignmentCreatedTime
  , lastSubmission
  , testCaseOfAssignment

  , saveSubmission
  , loadSubmission
  , assignmentOfSubmission
  , usernameOfSubmission
  , filterSubmissions
  , submissionKeys
  , evaluationOfSubmission
  , commentsOfSubmission
  , feedbacksOfSubmission

  , saveCommentNotification
  , saveFeedbackNotification
  , saveSystemNotification
  , loadNotification
  , commentOfNotification
  , feedbackOfNotification
  , usersOfNotification

  , removeFromOpened
  , openedSubmissions
  , usersOpenedSubmissions

  , saveSubmissionEvaluation
  , saveScoreEvaluation
  , loadEvaluation
  , modifyEvaluation
  , submissionOfEvaluation
  , scoreOfEvaluation

  , saveFeedback
  , loadFeedback
  , submissionOfFeedback

  , saveComment
  , loadComment
  , submissionOfComment

  , saveCourseAssessment
  , saveGroupAssessment
  , loadAssessment
  , modifyAssessment
  , courseOfAssessment
  , groupOfAssessment
  , scoresOfAssessment
  , assessmentsOfGroup
  , assessmentsOfCourse

  , saveScore
  , loadScore
  , assessmentOfScore
  , usernameOfScore
  , evaluationOfScore

  , testIncomingDataDir

  , isPersistenceSetUp
  , initPersistence
  , createPersistInit
  , createPersistInterpreter
  , parseConfig
#ifdef TEST
  , tests
#endif
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Entity.Notification hiding (Feedback)
import Bead.Domain.Relationships
import Bead.Persistence.Initialization
import Bead.Persistence.NoSQLDirFile
import Control.Monad.Transaction.TIO

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Exception (IOException)
import Control.Monad (join, liftM, filterM, when, unless, forM)
import Data.Maybe (catMaybes)
import System.FilePath
import System.Directory hiding (copyFile)
import System.Posix.Types (COff(..))
import System.Posix.Files (getFileStatus, fileExist, fileSize, modificationTime)
import Data.Function (on)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List (sortBy, isSuffixOf)

#ifdef TEST
import Test.Themis.Test (Test)
#endif

type Persist a = TIO a

reason :: Either IOException a -> (Erroneous a)
reason (Left e)  = Left . show $ e
reason (Right x) = Right x

-- No configuration is necessary
data Config = Config

-- | Creates a persist initialization structure.
createPersistInit :: Config -> IO PersistInit
createPersistInit _ = return PersistInit {
    isSetUp = isPersistenceSetUp
  , initPersist = initPersistence
  , tearDown = nTearDown
  }

-- | Creates an interpreter for the persistent compuation
createPersistInterpreter :: Config -> IO Interpreter
createPersistInterpreter _ = do
  mutex <- newMVar ()
  let run cmd = modifyMVar mutex $ \m -> do
                  x <- runPersist' cmd
                  return (m,x)
  return (Interpreter run)
  where
    runPersist' :: Persist a -> IO (Erroneous a)
    runPersist' = liftM reason . atomically

parseConfig :: String -> Config
parseConfig _ = Config

defaultConfig = Config

newtype Interpreter = Interpreter { unInt :: forall a . Persist a -> IO (Erroneous a) }

runInterpreter (Interpreter run) = run

runPersist = runInterpreter

-- Returns True if all the necessary persistence directories exist on the disk
-- otherwise false
isPersistenceSetUp :: IO Bool
isPersistenceSetUp = and <$> mapM doesDirectoryExist persistenceDirs

initPersistence :: IO ()
initPersistence = mapM_ createDirWhenDoesNotExist persistenceDirs
  where
    createDirWhenDoesNotExist d = do
      existDir <- doesDirectoryExist d
      unless existDir . createDirectory $ d

nTearDown :: IO ()
nTearDown = do
  exists <- doesDirectoryExist dataDir
  when exists $ removeDirectoryRecursive dataDir

saveUserReg :: UserRegistration -> Persist UserRegKey
saveUserReg u = do
  dirName <- createTmpDir userRegDataDir "ur"
  let userRegKey = UserRegKey . takeBaseName $ dirName
  save dirName u
  return userRegKey

loadUserReg :: UserRegKey -> Persist UserRegistration
loadUserReg u = do
  let p = userRegDirPath u
  isU <- isUserRegDir p
  unless isU . throwEx . userError . join $ [userRegKeyFold id u, " user registration does not exist."]
  liftM snd $ tLoadUserReg p

saveUser :: User -> Persist ()
saveUser usr = do
  userExist <- isThereAUser (u_username usr)
  case userExist of
    True -> throwEx $ userError $ "The user already exists: " ++ show (u_username usr)
    False -> do
      let dirname = dirName usr
      createDir dirname
      save    dirname usr

-- Checks if the given username exist in the persistence layer
-- and it has a correct structure
checkIfUserDir :: Username -> Persist ()
checkIfUserDir username = do
  let dirname = dirName username
  exist <- hasNoRollback $ doesDirectoryExist dirname
  unless exist . throwEx . userError $ "User directory does not exist: " ++ show username
  correct <- hasNoRollback $ isCorrectStructure dirname userDirStructure
  unless correct . throwEx . userError $ "User directory is not correct: " ++ show username

copyFile :: Username -> FilePath -> UsersFile -> Persist ()
copyFile username tmpPath userfile = do
  checkIfUserDir username
  let dirname = dirName username
      datadir = dirname </> (usersFile (const "public-files") (const "private-files") userfile)
  copy tmpPath (datadir </> usersFile id id userfile)

-- Calculates the file modification time in UTC time from the File status
fileModificationInUTCTime = posixSecondsToUTCTime . realToFrac . modificationTime

listFiles :: Username -> Persist [(UsersFile, FileInfo)]
listFiles username = do
  checkIfUserDir username
  let dirname = dirName username
  publicPaths <- getFilesInFolder (dirname </> "public-files")
  publicFiles <- forM publicPaths $ \path -> do
    status <- hasNoRollback $ getFileStatus path
    let info = FileInfo
                 (fileOffsetToInt $ fileSize status)
                 (fileModificationInUTCTime status)
    return (UsersPublicFile $ takeFileName path, info)
  privatePaths <- getFilesInFolder (dirname </> "private-files")
  privateFiles <- forM privatePaths $ \path -> do
    status <- hasNoRollback $ getFileStatus path
    let info = FileInfo
                 (fileOffsetToInt $ fileSize status)
                 (fileModificationInUTCTime status)
    return (UsersPrivateFile $ takeFileName path, info)
  return $! publicFiles ++ privateFiles
  where
    fileOffsetToInt (COff x) = fromIntegral x

getFile :: Username -> UsersFile -> Persist FilePath
getFile username userfile = do
  checkIfUserDir username
  let dirname = dirName username
      fname   = dirname </> (usersFile (const "public-files") (const "private-files") userfile)
                        </> (usersFile id id userfile)
  exist <- hasNoRollback $ doesFileExist fname
  unless exist . throwEx . userError $ concat [
      "File (", fname, ") does not exist in users folder ("
    , show username, ")"
    ]
  return fname

isThereAUser :: Username -> Persist Bool
isThereAUser uname = hasNoRollback $ do
  let dirname = dirName uname
  exist <- doesDirectoryExist dirname
  case exist of
    False -> return False
    True  -> isCorrectStructure dirname userDirStructure

doesUserExist :: Username -> Persist Bool
doesUserExist = hasNoRollback . doesDirectoryExist . dirName

personalInfo :: Username -> Persist PersonalInfo
personalInfo uname = do
  user <- loadUser uname
  return $ flip userCata user $ \role _username _email name timezone _lang uid ->
    PersonalInfo (role, name, timezone, uid)

isUserDir :: FilePath -> Persist Bool
isUserDir = isCorrectDirStructure userDirStructure

filterUsers :: (User -> Bool) -> Persist [User]
filterUsers f = filterDirectory userDataDir isUserDir load (filter f)

loadUser :: Username -> Persist User
loadUser = load . dirName

userDescription :: Username -> Persist UserDesc
userDescription = liftM mkUserDescription . loadUser

updateUser :: User -> Persist ()
updateUser user = update (dirName . u_username $ user) user

administratedCourses :: Username -> Persist [(CourseKey, Course)]
administratedCourses u = do
  let dirname = joinPath [dirName u, "courseadmin"]
  (selectValidDirsFrom dirname isCourseDir) >>= (mapM tLoadCourse)

administratedGroups :: Username -> Persist [(GroupKey, Group)]
administratedGroups u = do
  let dirname = joinPath [dirName u, "groupadmin"]
  (selectValidDirsFrom dirname isGroupDir) >>= (mapM tLoadGroup)

attachNotificationToUser :: Username -> NotificationKey -> Persist ()
attachNotificationToUser u nk = do
  link u nk "user"
  link nk u "notification"

notificationsOfUser :: Username -> Persist [NotificationKey]
notificationsOfUser =
  objectsIn "notification" NotificationKey isNotificationDir

courseDirPath :: CourseKey -> FilePath
courseDirPath (CourseKey c) = joinPath [courseDataDir, c]

groupDirPath :: GroupKey -> FilePath
groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

userRegDirPath :: UserRegKey -> FilePath
userRegDirPath = userRegKeyFold $ \u -> joinPath [userRegDataDir, u]

loadCourse :: CourseKey -> Persist Course
loadCourse c = do
  let p = courseDirPath c
  isC <- isCourseDir p
  -- GUARD: Course dir does not exist
  unless isC . throwEx . userError . join $ [courseKeyMap id c, " course does not exist."]
  -- Course found
  liftM snd $ tLoadCourse p

tLoadCourse :: FilePath -> Persist (CourseKey, Course)
tLoadCourse = tLoadPersistenceObject CourseKey

tLoadUserReg :: FilePath -> Persist (UserRegKey, UserRegistration)
tLoadUserReg = tLoadPersistenceObject UserRegKey

groupKeysOfCourse :: CourseKey -> Persist [GroupKey]
groupKeysOfCourse c = do
  let p = courseDirPath c
      g = joinPath [p, "groups"]
  subdirs <- getSubDirectories g
  return . map (GroupKey . takeBaseName) $ subdirs

createCourseAdmin :: Username -> CourseKey -> Persist ()
createCourseAdmin u ck = do
  usr <- loadUser u
  case atLeastCourseAdmin . u_role $ usr of
    False -> throwEx . userError . join $ [usernameCata id u, " is not course admin"]
    True  -> do
      link u ck "admins"
      link ck u "courseadmin"

subscribedToCourse :: CourseKey -> Persist [Username]
subscribedToCourse = objectsIn "users" Username isUserDir

subscribedToGroup :: GroupKey -> Persist [Username]
subscribedToGroup = objectsIn "users" Username isUserDir

unsubscribedFromCourse :: CourseKey -> Persist [Username]
unsubscribedFromCourse = objectsIn "unsubscribed" Username isUserDir

unsubscribedFromGroup :: GroupKey -> Persist [Username]
unsubscribedFromGroup = objectsIn "unsubscribed" Username isUserDir

isCorrectDirStructure :: DirStructure -> FilePath -> Persist Bool
isCorrectDirStructure d p = hasNoRollback $ isCorrectStructure p d

scoresOfUser :: Username -> Persist [ScoreKey]
scoresOfUser = objectsIn "score" ScoreKey isScoreDir

isGroupDir :: FilePath -> Persist Bool
isGroupDir = isCorrectDirStructure groupDirStructure

loadGroup :: GroupKey -> Persist Group
loadGroup g = do
  let p = groupDirPath g
  isG <- isGroupDir p
  -- GUARD: Group id does not exist
  unless isG . throwEx . userError . join $ [groupKeyMap id g, " group does not exist."]
  liftM snd $ tLoadGroup p
  where
    groupDirPath :: GroupKey -> FilePath
    groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

admins :: (DirName k) => k -> Persist [Username]
admins k = do
  let dirname = joinPath [dirName k, "admins"]
  mapM (liftM u_username . load) =<< (selectValidDirsFrom dirname isUserDir)

groupAdmins :: GroupKey -> Persist [Username]
groupAdmins = admins

courseAdmins :: CourseKey -> Persist [Username]
courseAdmins = admins

testScriptsOfCourse :: CourseKey -> Persist [TestScriptKey]
testScriptsOfCourse = objectsIn "test-script" TestScriptKey isTestScriptDir

isUserInGroup :: Username -> GroupKey -> Persist Bool
isUserInGroup u gk = isLinkedIn u gk "users"

isUserInCourse :: Username -> CourseKey -> Persist Bool
isUserInCourse u ck = isLinkedIn u ck "users"

subscribe :: Username -> CourseKey -> GroupKey -> Persist ()
subscribe username ck gk = do
  link username gk "users"
  link username ck "users"
  link gk username "group"
  link ck username "course"

unsubscribe :: Username -> CourseKey -> GroupKey -> Persist ()
unsubscribe username ck gk = do
  unlink username gk "users"
  unlink username ck "users"
  unlink gk username "group"
  unlink ck username "course"
  link username gk "unsubscribed"
  link username ck "unsubscribed"

createGroupAdmin :: Username -> GroupKey -> Persist ()
createGroupAdmin u gk = do
  link u gk "admins"
  link gk u "groupadmin"

courseOfGroup :: GroupKey -> Persist CourseKey
courseOfGroup = objectOrError "No course was found for " "course" CourseKey isCourseDir

tLoadPersistenceObject :: (Load o)
  => (String -> k) -- ^ Key constructor
  -> FilePath      -- ^ Base path
  -> Persist (k,o) -- ^ Key and the loaded object
tLoadPersistenceObject f d = do
  let key = takeBaseName d
  object <- load d
  return (f key, object)

tLoadGroup :: FilePath -> Persist (GroupKey, Group)
tLoadGroup = tLoadPersistenceObject GroupKey

saveCourse :: Course -> Persist CourseKey
saveCourse c = do
  dirName <- createTmpDir courseDataDir "cr"
  let courseKey = CourseKey . takeBaseName $ dirName
  save dirName c
  return courseKey

userCourses :: Username -> Persist [CourseKey]
userCourses u = do
  let dirname = joinPath [dirName u, "course"]
  map (CourseKey . takeBaseName) <$> (selectValidDirsFrom dirname isCourseDir)

userGroups :: Username -> Persist [GroupKey]
userGroups u = do
  let dirname = joinPath [dirName u, "group"]
  map (GroupKey . takeBaseName) <$> (selectValidDirsFrom dirname isGroupDir)

class ForeignKey k where
  referredPath :: k -> DirPath
  baseName     :: k -> String

foreignKey :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> Persist ()
foreignKey object linkto subdir =
  createLink
    (joinPath ["..", "..", "..", "..", (referredPath object)])
    (joinPath [(referredPath linkto), subdir, baseName object])

removeForeignKey :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> Persist ()
removeForeignKey object linkto subdir =
  deleteLink
    (joinPath ["..", "..", "..", "..", (referredPath object)])
    (joinPath [(referredPath linkto), subdir, baseName object])

isLinkedIn :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> Persist Bool
isLinkedIn object linkto subdir =
  hasNoRollback . doesDirectoryExist . joinPath $ [referredPath linkto, subdir, baseName object]

link :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> Persist ()
link object linkto subdir = do
  exist <- isLinkedIn object linkto subdir
  unless exist $ foreignKey object linkto subdir

unlink :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> Persist ()
unlink object linkto subdir = do
  exist <- isLinkedIn object linkto subdir
  when exist $ removeForeignKey object linkto subdir

instance ForeignKey Username where
  referredPath = dirName
  baseName     = takeBaseName . dirName

instance ForeignKey GroupKey where
  referredPath (GroupKey g) = joinPath [groupDataDir, g]
  baseName     (GroupKey g) = g

instance ForeignKey CourseKey where
  referredPath (CourseKey c) = joinPath [courseDataDir, c]
  baseName     (CourseKey c) = c

instance ForeignKey AssignmentKey where
  referredPath (AssignmentKey a) = joinPath [assignmentDataDir, a]
  baseName     (AssignmentKey a) = a

instance ForeignKey SubmissionKey where
  referredPath (SubmissionKey s) = joinPath [submissionDataDir, s]
  baseName     (SubmissionKey s) = s

instance ForeignKey TestJobKey where
  referredPath (TestJobKey s) = joinPath [testOutgoingDataDir, s]
  baseName     (TestJobKey s) = s

instance ForeignKey FeedbackKey where
  referredPath (FeedbackKey s) = joinPath [feedbackDataDir, s]
  baseName     (FeedbackKey s) = s

instance ForeignKey NotificationKey where
  referredPath (NotificationKey k) = joinPath [notificationDataDir, k]
  baseName     (NotificationKey k) = k

{- * One primitve value is stored in the file with the same name as the row.
   * One combined value is stored in the given directory into many files. The name
     of the directory is the primary key for the record.
   * The foreign keys are the symlinks for the other row of the given combined object.
-}
saveGroup :: CourseKey -> Group -> Persist GroupKey
saveGroup ck group = do
  dirName <- createTmpDir groupDataDir "gr"
  let groupKey = GroupKey . takeBaseName $ dirName
  save dirName group
  link groupKey ck "groups"
  link ck groupKey "course"
  return groupKey

filterGroups :: (GroupKey -> Group -> Bool) -> Persist [(GroupKey, Group)]
filterGroups f = filterDirectory groupDataDir isGroupDir tLoadGroup (filter (uncurry f))

currentTime :: Persist UTCTime
currentTime = hasNoRollback getCurrentTime

saveAssignment :: Assignment -> Persist AssignmentKey
saveAssignment a = do
  dirName <- createTmpDir assignmentDataDir "a"
  let assignmentKey = takeBaseName dirName
  save dirName a
  saveCreatedTime dirName =<< currentTime
  return . AssignmentKey $ assignmentKey

modifyAssignment :: AssignmentKey -> Assignment -> Persist ()
modifyAssignment ak a = do
  let p = assignmentDirPath ak
  isA <- isAssignmentDir p
  unless isA . throwEx . userError $ "Assignment does not exist"
  update p a

assignmentCreatedTime :: AssignmentKey -> Persist UTCTime
assignmentCreatedTime ak = do
  let p = assignmentDirPath ak
  isDir <- isAssignmentDir p
  case isDir of
    False -> throwEx $ userError $ join [assignmentKeyMap id ak, " assignment does not exist."]
    True  -> getCreatedTime p

selectValidDirsFrom :: FilePath -> (FilePath -> Persist Bool) -> Persist [FilePath]
selectValidDirsFrom dir isValidDir = getSubDirectories dir >>= filterM isValidDir

assignmentKeys :: Persist [AssignmentKey]
assignmentKeys =
  (selectValidDirsFrom assignmentDataDir isAssignmentDir) >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (AssignmentKey . takeBaseName)

filterAssignment :: (AssignmentKey -> Assignment -> Bool) -> Persist [(AssignmentKey, Assignment)]
filterAssignment f = filterDirectory assignmentDataDir isAssignmentDir tLoadAssignment (filter (uncurry f))

loadAssignment :: AssignmentKey -> Persist Assignment
loadAssignment a = do
  let p = assignmentDirPath a
  isEx <- isAssignmentDir p
  case isEx of
    False -> throwEx $ userError $ join [assignmentKeyMap id a, " assignment does not exist."]
    True  -> liftM snd $ tLoadAssignment p

assignmentDirPath :: AssignmentKey -> FilePath
assignmentDirPath (AssignmentKey e) = joinPath [assignmentDataDir, e]

tLoadAssignment :: FilePath -> Persist (AssignmentKey, Assignment)
tLoadAssignment dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (AssignmentKey exerciseKey, e)


objectsIn
  :: (Show sk, DirName sk)
  => FilePath               -- Subdir where to look at
  -> (String -> rk)         -- Result's key constructor
  -> (FilePath -> Persist Bool) -- Checks if the given subdir is appropiate
  -> sk                     -- The source's key
  -> Persist [rk]
objectsIn subdir keyConstructor isValidDir sourceKey = do
  let dirname = joinPath [dirName sourceKey, subdir]
  map (keyConstructor . takeBaseName) <$> (selectValidDirsFrom dirname isValidDir)

objectIn s k v sk = objectsIn s k v sk >>= just
  where
    just []  = return Nothing
    just [k] = return $ Just k
    just _   = throwEx . userError $ "Impossible: found more than one object found for: " ++ show sk

-- Browses the objects in the given directory all the subdirectories,
-- checks if entry with the 'isValidDir'
objectOrError msg subdir keyConstructor isValidDir sourceKey = do
  m <- objectIn subdir keyConstructor isValidDir sourceKey
  case m of
    Nothing -> throwEx . userError $ msg ++ show sourceKey
    Just  x -> return x

courseOfAssignment :: AssignmentKey -> Persist (Maybe CourseKey)
courseOfAssignment = objectIn "course" CourseKey isCourseDir

groupOfAssignment :: AssignmentKey -> Persist (Maybe GroupKey)
groupOfAssignment = objectIn  "group" GroupKey isGroupDir

testCaseOfAssignment :: AssignmentKey -> Persist (Maybe TestCaseKey)
testCaseOfAssignment = objectIn "test-case" TestCaseKey isTestCaseDir

submissionsForAssignment :: AssignmentKey -> Persist [SubmissionKey]
submissionsForAssignment = objectsIn "submission" SubmissionKey isSubmissionDir

saveAndLinkAssignment :: (ForeignKey k) => FilePath -> k -> Assignment -> Persist AssignmentKey
saveAndLinkAssignment subdir k a = do
  ak <- saveAssignment a
  link ak k "assignments"
  link k ak subdir
  return ak

saveCourseAssignment :: CourseKey -> Assignment -> Persist AssignmentKey
saveCourseAssignment = saveAndLinkAssignment "course"

saveGroupAssignment :: GroupKey  -> Assignment -> Persist AssignmentKey
saveGroupAssignment = saveAndLinkAssignment "group"

isAssignmentDir :: FilePath -> Persist Bool
isAssignmentDir = isCorrectDirStructure assignmentDirStructure

assignmentsFor :: (a -> FilePath) -> a -> Persist [AssignmentKey]
assignmentsFor dirPath k = do
  fp <- (selectValidDirsFrom (joinPath [dirPath k, "assignments"]) isAssignmentDir)
  return ((AssignmentKey . takeBaseName) <$> fp)

courseAssignments :: CourseKey -> Persist [AssignmentKey]
courseAssignments = assignmentsFor courseDirPath

groupAssignments :: GroupKey -> Persist [AssignmentKey]
groupAssignments = assignmentsFor groupDirPath

courseKeys :: Persist [CourseKey]
courseKeys =
  (selectValidDirsFrom courseDataDir isCourseDir) >>=
  calcCourseKeys
    where
      calcCourseKeys = return . map (CourseKey . takeBaseName)

isCourseDir :: FilePath -> Persist Bool
isCourseDir = isCorrectDirStructure courseDirStructure

isUserRegDir :: FilePath -> Persist Bool
isUserRegDir = isCorrectDirStructure userRegDirStructure


filterCourses :: (CourseKey -> Course -> Bool) -> Persist [(CourseKey, Course)]
filterCourses f = filterDirectory courseDataDir isCourseDir tLoadCourse (filter (uncurry f))

-- * Notification

isNotificationDir :: FilePath -> Persist Bool
isNotificationDir = isCorrectDirStructure notificationDirStructure

saveNotification :: Notification -> Persist NotificationKey
saveNotification n = do
  dirName <- createTmpDir notificationDataDir "n"
  let notificationKey = NotificationKey . takeBaseName $ dirName
  save dirName n
  return notificationKey

saveCommentNotification :: CommentKey -> Notification -> Persist NotificationKey
saveCommentNotification ck n = do
  nk <- saveNotification n
  link ck nk "comment"
  return nk

saveFeedbackNotification :: FeedbackKey -> Notification -> Persist NotificationKey
saveFeedbackNotification fk n = do
  nk <- saveNotification n
  link fk nk "feedback"
  return nk

saveSystemNotification :: Notification -> Persist NotificationKey
saveSystemNotification = saveNotification

loadNotification :: NotificationKey -> Persist Notification
loadNotification = load . dirName

commentOfNotification :: NotificationKey -> Persist (Maybe CommentKey)
commentOfNotification = objectIn "comment" CommentKey isCommentDir

feedbackOfNotification :: NotificationKey -> Persist (Maybe FeedbackKey)
feedbackOfNotification = objectIn "feedback" FeedbackKey isFeedbackDir

usersOfNotification :: NotificationKey -> Persist [Username]
usersOfNotification = objectsIn "user" Username isUserDir

-- * Submission

isSubmissionDir :: FilePath -> Persist Bool
isSubmissionDir = isCorrectDirStructure submissionDirStructure

saveSubmission :: AssignmentKey -> Username -> Submission -> Persist SubmissionKey
saveSubmission ak u s = do
  dirName <- createTmpDir submissionDataDir "s"
  let submissionKey = SubmissionKey . takeBaseName $ dirName
  save dirName s
  link ak submissionKey "assignment"
  link u  submissionKey "user"
  link submissionKey ak "submission"
  linkUserSubmission submissionKey
  nPlaceToOpened ak u submissionKey
  return submissionKey
    where
      linkUserSubmission :: SubmissionKey -> Persist ()
      linkUserSubmission sk = do
        let dirName = userAsgSubmissionDir u ak
        createDirIfMissing dirName
        createLink
          (joinPath ["..","..","..","..","..",(referredPath sk)])
          (joinPath [dirName, (baseName sk)])

userAsgSubmissionDir :: Username -> AssignmentKey -> FilePath
userAsgSubmissionDir u ak = joinPath [referredPath u, "submissions", baseName ak]

lastSubmission :: AssignmentKey -> Username -> Persist (Maybe SubmissionKey)
lastSubmission ak u = do
  let dirName = userAsgSubmissionDir u ak
  e <- hasNoRollback $ doesDirectoryExist dirName
  case e of
    False -> return Nothing
    True -> do
      paths <- selectValidDirsFrom dirName isSubmissionDir
      case paths of
       [] -> return Nothing
       ps -> (Just . snd . last . sortBy (compare `on` (solutionPostDate . fst))) <$> (mapM loadSbm ps)
  where
    loadSbm p = do
      s <- load p
      return (s, SubmissionKey . takeBaseName $ p)

-- TODO: Validate the directory
loadSubmission :: SubmissionKey -> Persist Submission
loadSubmission = load . dirName

assignmentOfSubmission :: SubmissionKey -> Persist AssignmentKey
assignmentOfSubmission sk =
  objectOrError "No assignment was found for the " "assignment" AssignmentKey isAssignmentDir sk

usernameOfSubmission :: SubmissionKey -> Persist Username
usernameOfSubmission sk =
  objectOrError "No assignment was found for the " "user" Username isUserDir sk

evaluationOfSubmission :: SubmissionKey -> Persist (Maybe EvaluationKey)
evaluationOfSubmission =
  objectIn "evaluation" EvaluationKey isEvaluationDir

filterSubmissions :: (SubmissionKey -> Submission -> Bool) -> Persist [(SubmissionKey, Submission)]
filterSubmissions f = filterDirectory submissionDataDir isSubmissionDir tLoadSubmission (filter (uncurry f))

submissionKeys :: Persist [SubmissionKey]
submissionKeys = map fst <$> filterSubmissions (\_ _ -> True)

tLoadSubmission :: FilePath -> Persist (SubmissionKey, Submission)
tLoadSubmission dirName = do
  s <- load dirName
  return (SubmissionKey . takeBaseName $ dirName, s)

openedSubmissionDataDirPath :: AssignmentKey -> Username -> FilePath
openedSubmissionDataDirPath ak u =
  joinPath [openSubmissionDataDir, "assignment", baseName ak, baseName u]

nPlaceToOpened :: AssignmentKey -> Username -> SubmissionKey -> Persist ()
nPlaceToOpened ak u sk = do
  let lookupPath = openedSubmissionDataDirPath ak u
  createLink
    (joinPath ["..", "..", "..", (referredPath sk)])
    (joinPath [openSubmissionAllDataDir, baseName sk])
  createDirIfMissing lookupPath
  createLink
    (joinPath ["..", "..", "..", "..", "..", (referredPath sk)])
    (joinPath [lookupPath, baseName sk])

removeFromOpened :: AssignmentKey -> Username -> SubmissionKey -> Persist ()
removeFromOpened ak u sk = do

  let openedAllSubmissionKeyPath = joinPath [openSubmissionAllDataDir, baseName sk]
  exist <- hasNoRollback $ fileExist openedAllSubmissionKeyPath
  when exist $ removeSymLink openedAllSubmissionKeyPath

  let openedSubmissionDataDir = joinPath [openedSubmissionDataDirPath ak u, baseName sk]
  exist <- hasNoRollback $ fileExist openedSubmissionDataDir
  when exist $ removeSymLink openedSubmissionDataDir

usersOpenedSubmissions :: AssignmentKey -> Username -> Persist [SubmissionKey]
usersOpenedSubmissions ak u = do
  let path = openedSubmissionDataDirPath ak u
  exists <- doesDirExist path
  if exists
    then filterDirectory path isSubmissionDir tLoadSubmission (map fst)
    else return []

filterDirectory :: FilePath -> (FilePath -> Persist Bool) -> (FilePath -> Persist a) -> ([a] -> [b]) -> Persist [b]
filterDirectory dir isValid loader f = f <$> ((selectValidDirsFrom dir isValid) >>= (mapM loader))

-- | First it checks if the directory exist, if not the result is an empty list
--   else, the result is the original filtered data
safeFilterDirectory :: FilePath -> (FilePath -> Persist Bool) -> (FilePath -> Persist a) -> ([a] -> [b]) -> Persist [b]
safeFilterDirectory dir isValid loader f = do
  e <- doesDirExist dir
  case e of
    False -> return []
    True  -> filterDirectory dir isValid loader f

openedSubmissions :: Persist [SubmissionKey]
openedSubmissions = filterDirectory openSubmissionAllDataDir isSubmissionDir tLoadSubmission (map fst)

userDirPath :: Username -> FilePath
userDirPath (Username u) = joinPath [userDataDir, u]

submissionDirPath :: SubmissionKey -> FilePath
submissionDirPath (SubmissionKey sk) = joinPath [submissionDataDir, sk]

userSubmissions :: Username -> AssignmentKey -> Persist [SubmissionKey]
userSubmissions u ak =
  safeFilterDirectory
    (joinPath [userDirPath u, "submissions", baseName ak])
    isSubmissionDir
    (return . takeBaseName)
    (map SubmissionKey)

commentsOfSubmission :: SubmissionKey -> Persist [CommentKey]
commentsOfSubmission sk =
  filterDirectory
    (joinPath [submissionDirPath sk, "comment"])
    isCommentDir
    (return . takeBaseName)
    (map CommentKey)

-- * Evaluation

instance ForeignKey EvaluationKey where
  referredPath (EvaluationKey e) = joinPath [evaluationDataDir, e]
  baseName     (EvaluationKey e) = e

isEvaluationDir :: FilePath -> Persist Bool
isEvaluationDir = isCorrectDirStructure evaluationDirStructure

saveEvaluation :: Evaluation -> Persist EvaluationKey
saveEvaluation e = do
  dirName <- createTmpDir evaluationDataDir "ev"
  let evKey = EvaluationKey . takeBaseName $ dirName
  save dirName e
  return evKey

saveSubmissionEvaluation :: SubmissionKey -> Evaluation -> Persist EvaluationKey
saveSubmissionEvaluation sk e = do
  key <- saveEvaluation e
  link key sk "evaluation"
  link sk key "submission"
  return key

saveScoreEvaluation :: ScoreKey -> Evaluation -> Persist EvaluationKey
saveScoreEvaluation sk e = do
  key <- saveEvaluation e
  link key sk "evaluation"
  link sk key "score"
  return key

evaluationDirPath :: EvaluationKey -> FilePath
evaluationDirPath (EvaluationKey e) = joinPath [evaluationDataDir, e]

loadEvaluation :: EvaluationKey -> Persist Evaluation
loadEvaluation e = do
  let p = evaluationDirPath e
  isE <- isEvaluationDir p
  unless isE . throwEx . userError . join $ ["Evaluation does not exist."]
  liftM snd . tLoadPersistenceObject EvaluationKey $ p

modifyEvaluation :: EvaluationKey -> Evaluation -> Persist ()
modifyEvaluation ek e = do
  let p = evaluationDirPath ek
  isE <- isEvaluationDir p
  unless isE . throwEx . userError . join $ ["Evaluation does not exist."]
  update p e

submissionOfEvaluation :: EvaluationKey -> Persist (Maybe SubmissionKey)
submissionOfEvaluation =
  objectIn "submission" SubmissionKey isSubmissionDir

scoreOfEvaluation :: EvaluationKey -> Persist (Maybe ScoreKey)
scoreOfEvaluation =
  objectIn "score" ScoreKey isScoreDir

-- * Comment

instance ForeignKey CommentKey where
  referredPath (CommentKey c) = joinPath [commentDataDir, c]
  baseName     (CommentKey c) = c

commentDirPath :: CommentKey -> FilePath
commentDirPath (CommentKey c) = joinPath [commentDataDir, c]

isCommentDir :: FilePath -> Persist Bool
isCommentDir = isCorrectDirStructure commentDirStructure

saveComment :: SubmissionKey -> Comment -> Persist CommentKey
saveComment sk c = do
  dirName <- createTmpDir commentDataDir "cm"
  let key = CommentKey . takeBaseName $ dirName
  save dirName c
  link key sk "comment"
  link sk key "submission"
  return key

loadComment :: CommentKey -> Persist Comment
loadComment ck = do
  let p = commentDirPath ck
  isC <- isCommentDir p
  unless isC . throwEx $ userError "Comment does not exist."
  liftM snd . tLoadPersistenceObject CommentKey $ p

submissionOfComment :: CommentKey -> Persist SubmissionKey
submissionOfComment =
  objectOrError "No submission was found for " "submission" SubmissionKey isSubmissionDir

-- * Test Script

testScriptDirPath :: TestScriptKey -> FilePath
testScriptDirPath = testScriptKeyCata $ \k -> joinPath [testScriptDataDir, k]

isTestScriptDir :: FilePath -> Persist Bool
isTestScriptDir = isCorrectDirStructure testScriptDirStructure

instance ForeignKey TestScriptKey where
  referredPath = testScriptDirPath
  baseName     (TestScriptKey k) = k

saveTestScript :: CourseKey -> TestScript -> Persist TestScriptKey
saveTestScript ck ts = do
  dirName <- createTmpDir testScriptDataDir "ts"
  let key = TestScriptKey $ takeBaseName dirName
  save dirName ts
  link key ck "test-script"
  link ck key "course"
  return key

loadTestScript :: TestScriptKey -> Persist TestScript
loadTestScript tk = do
  let p = testScriptDirPath tk
  isTS <- isTestScriptDir p
  unless isTS . throwEx $ userError "Not a test script directory"
  snd <$> tLoadPersistenceObject TestScriptKey p

courseOfTestScript :: TestScriptKey -> Persist CourseKey
courseOfTestScript =
  objectOrError "No course was found for " "course" CourseKey isCourseDir

modifyTestScript :: TestScriptKey -> TestScript -> Persist ()
modifyTestScript tk ts = do
  let p = testScriptDirPath tk
  isTS <- isTestScriptDir p
  unless isTS . throwEx $ userError "Test Script does not exist"
  update p ts

-- * Test Case

testCaseDirPath :: TestCaseKey -> FilePath
testCaseDirPath = testCaseKeyCata $ \k -> joinPath [testCaseDataDir, k]

isTestCaseDir :: FilePath -> Persist Bool
isTestCaseDir = isCorrectDirStructure testCaseDirStructure

instance ForeignKey TestCaseKey where
  referredPath = testCaseDirPath
  baseName (TestCaseKey k) = k

saveTestCase :: TestScriptKey -> AssignmentKey -> TestCase -> Persist TestCaseKey
saveTestCase tk ak tc = do
  dirName <- createTmpDir testCaseDataDir "tc"
  let key = TestCaseKey $ takeBaseName dirName
  save dirName tc
  link key ak "test-case"
  link ak key "assignment"
  link tk key "test-script"
  return key

removeTestCaseAssignment :: TestCaseKey -> AssignmentKey -> Persist ()
removeTestCaseAssignment tk ak = do
  unlink tk ak "test-case"
  unlink ak tk "assignment"

modifyTestScriptOfTestCase :: TestCaseKey -> TestScriptKey -> Persist ()
modifyTestScriptOfTestCase tck tsk = do
  tskOld <- testScriptOfTestCase tck
  unlink tskOld tck "test-script"
  link   tsk    tck "test-script"

loadTestCase :: TestCaseKey -> Persist TestCase
loadTestCase tk = do
  let p = testCaseDirPath tk
  isTC <- isTestCaseDir p
  unless isTC . throwEx $ userError "Not a test case directory"
  snd <$> tLoadPersistenceObject TestCaseKey p

testScriptOfTestCase :: TestCaseKey -> Persist TestScriptKey
testScriptOfTestCase =
  objectOrError "No Test Script was found for " "test-script" TestScriptKey isTestScriptDir

modifyTestCase :: TestCaseKey -> TestCase -> Persist ()
modifyTestCase tk tc = do
  let p = testCaseDirPath tk
  isTC <- isTestCaseDir p
  unless isTC . throwEx $ userError "Test Case does not exist"
  update p tc

copyTestCaseFile :: TestCaseKey -> Username -> UsersFile -> Persist ()
copyTestCaseFile tk u uf = do
  let p = testCaseDirPath tk
  isTC <- isTestCaseDir p
  unless isTC . throwEx $ userError "Test Case does not exist"
  ufp <- getFile u uf
  overwriteFile ufp (p </> "value")

-- Collects the test script, test case and the submission and copies them to the
-- the directory named after the submission key placed in the test-outgoing directory
saveTestJob :: SubmissionKey -> Persist ()
saveTestJob sk = do
  ak <- assignmentOfSubmission sk
  mtk <- testCaseOfAssignment ak
  maybe (return ()) copyParts mtk
  where
    -- If there is a test case, we copy the information to the desired
    copyParts :: TestCaseKey -> Persist ()
    copyParts tk = do
      tsk <- testScriptOfTestCase tk
      let submissionFile = referredPath sk  </> "solution"
          testcaseFile   = referredPath tk  </> "value"
          testscriptFile = referredPath tsk </> "script"
          tjk = submissionKeyToTestJobKey sk
          tjPath = referredPath tjk
      exist <- hasNoRollback $ doesDirectoryExist tjPath
      when exist . throwEx . userError $ concat ["Test job directory already exist:", show tjk]
      createDirLocked tjPath $ \p -> do
        copy submissionFile (p </> "submission")
        copy testscriptFile (p </> "script")
        copy testcaseFile   (p </> "tests")

insertTestFeedback :: SubmissionKey -> FeedbackInfo -> Persist ()
insertTestFeedback sk info = do
  let sDir = submissionKeyMap (testIncomingDataDir </>) sk
  hasNoRollback $ createDirectoryIfMissing True sDir
  let student comment = fileSave sDir "public" comment
      admin   comment = fileSave sDir "private" comment
      result  bool    = fileSave sDir "result" (show bool)
  feedbackInfo result student admin evaluated info
  where
    evaluated _ _ = error "insertTestComment: Evaluation should not be inserted by test."

-- Test Feedbacks are stored in the persistence layer, in the test-incomming directory
-- each one in a file, named after an existing submission in the system
testFeedbacks :: Persist [(SubmissionKey, Feedback)]
testFeedbacks = createFeedbacks =<< processables
  where
    processables = filter (not . (`isSuffixOf` ".locked")) <$>
      getSubDirectories testIncomingDataDir

    createFeedbacks = fmap join . mapM createFeedback

    createFeedback path = do
      let sk = SubmissionKey . last $ splitDirectories path
          addKey x = (sk, x)

      files <- getFilesInFolder path
      fmap (map addKey . catMaybes) $
        forM files $ \file -> do
          fileDate <- fileModificationInUTCTime <$> (hasNoRollback $ getFileStatus file)

          let (dir,fname) = splitFileName file
              feedback f = Feedback f fileDate

          case fname of
            "private" -> Just . feedback . MessageForAdmin <$> fileLoad dir fname Just
            "public"  -> Just . feedback . MessageForStudent <$> fileLoad dir fname Just
            "result"  -> Just . feedback . TestResult <$> fileLoad dir fname readMaybe
            _         -> return Nothing

-- Deletes the comments (test-agent and message as well)
-- contained file from the test-incomming directory, named after
-- an existing submission
deleteTestFeedbacks :: SubmissionKey -> Persist ()
deleteTestFeedbacks =
  submissionKeyMap (hasNoRollback . removeDirectoryRecursive . (testIncomingDataDir </>))

-- Return all the feedbacks for the given submission
feedbacksOfSubmission :: SubmissionKey -> Persist [FeedbackKey]
feedbacksOfSubmission = objectsIn "feedback" FeedbackKey isFeedbackDir

-- * Feedback

feedbackDirPath :: FeedbackKey -> FilePath
feedbackDirPath = feedbackKey $ \k -> joinPath [feedbackDataDir, k]

isFeedbackDir :: FilePath -> Persist Bool
isFeedbackDir = isCorrectDirStructure feedbackDirStructure

-- Saves the feedback
saveFeedback :: SubmissionKey -> Feedback -> Persist FeedbackKey
saveFeedback sk f = do
  dirName <- createTmpDir feedbackDataDir "f"
  let key = FeedbackKey $ takeBaseName dirName
  save dirName f
  link key sk "feedback"
  link sk key "submission"
  return key

-- Loads the feedback
loadFeedback :: FeedbackKey -> Persist Feedback
loadFeedback fk = do
  let p = feedbackDirPath fk
  isF <- isFeedbackDir p
  unless isF . throwEx $ userError "Not a feedback directory"
  snd <$> tLoadPersistenceObject FeedbackKey p

-- Returns the submission of the feedback
submissionOfFeedback :: FeedbackKey -> Persist SubmissionKey
submissionOfFeedback =
  objectOrError "No Submission was found for " "submission" SubmissionKey isSubmissionDir

-- * Assessment

instance ForeignKey AssessmentKey where
  referredPath (AssessmentKey c) = joinPath [assessmentDataDir, c]
  baseName     (AssessmentKey c) = c

isAssessmentDir :: FilePath -> Persist Bool
isAssessmentDir = isCorrectDirStructure assessmentDirStructure

assessmentDirPath :: AssessmentKey -> FilePath
assessmentDirPath (AssessmentKey e) = joinPath [assessmentDataDir, e]

saveAssessment :: Assessment -> Persist AssessmentKey
saveAssessment as = do
  dirName <- createTmpDir assessmentDataDir "at"
  let key = AssessmentKey $ takeBaseName dirName
  save dirName as
  return key

saveCourseAssessment :: CourseKey -> Assessment -> Persist AssessmentKey
saveCourseAssessment ck as = do
  key <- saveAssessment as
  link key ck "assessments"
  link ck key "course"
  return key

saveGroupAssessment :: GroupKey -> Assessment -> Persist AssessmentKey
saveGroupAssessment gk as = do
  key <- saveAssessment as
  link key gk "assessments"
  link gk key "group"
  return key

loadAssessment :: AssessmentKey -> Persist Assessment
loadAssessment ak = do
  let p = assessmentDirPath ak
  isDir <- isAssessmentDir p
  unless isDir . throwEx $ userError "Not an assessment directory"
  snd <$> tLoadPersistenceObject AssessmentKey p

modifyAssessment :: AssessmentKey -> Assessment -> Persist ()
modifyAssessment ak a = do
  let p = assessmentDirPath ak
  isDir <- isAssessmentDir p
  unless isDir . throwEx $ userError "Not an assessment directory"
  update p a

courseOfAssessment :: AssessmentKey -> Persist (Maybe CourseKey)
courseOfAssessment = objectIn "course" CourseKey isCourseDir

groupOfAssessment :: AssessmentKey -> Persist (Maybe GroupKey)
groupOfAssessment = objectIn "group" GroupKey isGroupDir

scoresOfAssessment :: AssessmentKey -> Persist [ScoreKey]
scoresOfAssessment = objectsIn "score" ScoreKey isScoreDir

assessmentsOfGroup :: GroupKey -> Persist [AssessmentKey]
assessmentsOfGroup = objectsIn "assessments" AssessmentKey isAssessmentDir

assessmentsOfCourse :: CourseKey -> Persist [AssessmentKey]
assessmentsOfCourse = objectsIn "assessments" AssessmentKey isAssessmentDir

-- * Score

instance ForeignKey ScoreKey where
  referredPath (ScoreKey c) = joinPath [scoreDataDir, c]
  baseName     (ScoreKey c) = c

isScoreDir :: FilePath -> Persist Bool
isScoreDir = isCorrectDirStructure scoreDirStructure

scoreDirPath :: ScoreKey -> FilePath
scoreDirPath (ScoreKey e) = joinPath [scoreDataDir, e]

saveScore :: Username -> AssessmentKey -> Score -> Persist ScoreKey
saveScore u ak s = do
  dirName <- createTmpDir scoreDataDir "sc"
  let key = ScoreKey $ takeBaseName dirName
  save dirName s
  link key u  "score"
  link u key "user"
  link ak key "assessment"
  link key ak "score"
  return key

loadScore :: ScoreKey -> Persist Score
loadScore sk = do
  let p = scoreDirPath sk
  isDir <- isScoreDir p
  unless isDir . throwEx $ userError "Not a score directory"
  snd <$> tLoadPersistenceObject ScoreKey p

assessmentOfScore :: ScoreKey -> Persist AssessmentKey
assessmentOfScore = objectOrError "No assessment was found for " "assessment" AssessmentKey isAssessmentDir

usernameOfScore :: ScoreKey -> Persist Username
usernameOfScore = objectOrError "No user was found for " "user" Username isUserDir

evaluationOfScore :: ScoreKey -> Persist (Maybe EvaluationKey)
evaluationOfScore = objectIn "evaluation" EvaluationKey isEvaluationDir

-- * Tools

encodePwd :: String -> String
encodePwd = ordEncode

#ifdef TEST
tests :: Test ()
tests = return ()
#endif
