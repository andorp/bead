module Bead.Persistence.NoSQLDir (
    noSqlDirPersist
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDirFile
import Control.Monad.Transaction.TIO

import Control.Applicative ((<$>))
import Control.Monad (join, mapM, liftM, filterM, when, unless)
import Control.Exception (IOException, throwIO)
import System.FilePath (joinPath, takeBaseName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import Data.Function (on)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sortBy)

-- | Simple directory and file based NoSQL persistence implementation
noSqlDirPersist = Persist {
    saveUser      = nSaveUser
  , personalInfo  = nPersonalInfo
  , filterUsers   = nFilterUsers
  , loadUser      = nLoadUser
  , updateUser    = nUpdateUser
  , doesUserExist = nDoesUserExist
  , userDescription = nUserDescription
  , userSubmissions = nUserSubmissions
  , administratedCourses = nAdministratedCourses
  , administratedGroups = nAdministratedGroups

  , copyFile  = nCopyFile
  , listFiles = nListFiles
  , getFile   = nGetFile

  , saveUserReg = nSaveUserReg
  , loadUserReg = nLoadUserReg

  , saveCourse    = nSaveCourse
  , courseKeys    = nCourseKeys
  , filterCourses = nFilterCourses
  , loadCourse    = nLoadCourse
  , groupKeysOfCourse = nGroupKeysOfCourse
  , isUserInCourse = nIsUserInCourse
  , userCourses = nUserCourses
  , createCourseAdmin = nCreateCourseAdmin
  , courseAdmins = nCourseAdmins
  , subscribedToCourse = nSubscribedToCourse
  , unsubscribedFromCourse = nUnsubscribedFromCourse
  , testScriptsOfCourse = nTestScriptsOfCourse

  , saveGroup     = nSaveGroup
  , loadGroup     = nLoadGroup
  , courseOfGroup = nCourseOfGroup
  , filterGroups  = nFilterGroups
  , isUserInGroup = nIsUserInGroup
  , userGroups    = nUserGroups
  , subscribe     = nSubscribe
  , unsubscribe   = nUnsubscribe
  , groupAdmins   = nGroupAdmins
  , createGroupAdmin    = nCreateGroupAdmin
  , subscribedToGroup   = nSubscribedToGroup
  , unsubscribedFromGroup = nUnsubscribedFromGroup

  , saveTestScript = nSaveTestScript -- :: CourseKey -> TestScript -> TIO TestScriptKey
  , loadTestScript = nLoadTestScript -- :: TestScriptKey -> TIO TestScript
  , courseOfTestScript = nCourseOfTestScript -- :: TestScriptKey -> TIO CourseKey
  , modifyTestScript = nModifyTestScript -- :: TestScriptKey -> TestScript -> TIO ()

  , saveTestCase = nSaveTestCase -- :: TestScriptKey -> AssignmentKey -> TIO TestCaseKey
  , loadTestCase = nLoadTestCase -- :: TestCaseKey -> TIO TestCase
  , assignmentOfTestCase = nAssignmentOfTestCase -- :: TestCaseKey -> TIO AssignmentKey
  , testScriptOfTestCase = nTestScriptOfTestCase -- :: TestCaseKey -> TIO TestScriptKey
  , modifyTestCase = nModifyTestCase -- :: TestCaseKey -> TestCase -> TIO ()

  , filterAssignment     = nFilterAssignment
  , assignmentKeys       = nAssignmentKeys
  , saveAssignment       = nSaveAssignment
  , loadAssignment       = nLoadAssignment
  , modifyAssignment     = nModifyAssignment
  , saveCourseAssignment = nSaveCourseAssignment
  , saveGroupAssignment  = nSaveGroupAssignment
  , courseAssignments    = nCourseAssignments
  , groupAssignments     = nGroupAssignments
  , courseOfAssignment   = nCourseOfAssignment
  , groupOfAssignment    = nGroupOfAssignment
  , submissionsForAssignment = nSubmissionsForAssignment
  , assignmentCreatedTime    = nAssignmentCreatedTime
  , lastSubmission           = nLastSubmission
  , testCaseOfAssignment = nTestCaseOfAssignment

  , saveSubmission = nSaveSubmission
  , loadSubmission = nLoadSubmission
  , assignmentOfSubmission = nAssignmentOfSubmission
  , usernameOfSubmission   = nUsernameOfSubmission
  , filterSubmissions      = nFilterSubmissions
  , evaluationOfSubmission = nEvaluationOfSubmission
  , commentsOfSubmission   = nCommentsOfSubmission

  , removeFromOpened  = nRemoveFromOpened
  , openedSubmissions = nOpenedSubmission
  , usersOpenedSubmissions = nUsersOpenedSubmissions

  , saveEvaluation = nSaveEvaluation
  , loadEvaluation = nLoadEvaluation
  , modifyEvaluation = nModifyEvaluation
  , submissionOfEvaluation = nSubmissionOfEvaluation

  , saveComment = nSaveComment
  , loadComment = nLoadComment
  , submissionOfComment = nSubmissionOfComment

  , isPersistenceSetUp = nIsPersistenceSetUp
  , initPersistence    = nInitPersistence
  }

-- Returns True if all the necessary persistence directories exist on the disk
-- otherwise false
nIsPersistenceSetUp :: IO Bool
nIsPersistenceSetUp = and <$> mapM doesDirectoryExist persistenceDirs

nInitPersistence :: IO ()
nInitPersistence = mapM_ createDirWhenDoesNotExist persistenceDirs
  where
    createDirWhenDoesNotExist d = do
      existDir <- doesDirectoryExist d
      unless existDir . createDirectory $ d

nSaveUserReg :: UserRegistration -> TIO UserRegKey
nSaveUserReg u = do
  dirName <- createTmpDir userRegDataDir "ur"
  let userRegKey = UserRegKey . takeBaseName $ dirName
  save dirName u
  return userRegKey


nLoadUserReg :: UserRegKey -> TIO UserRegistration
nLoadUserReg u = do
  let p = userRegDirPath u
  isU <- isUserRegDir p
  unless isU . throwEx . userError . join $ [str u, " user registration does not exist."]
  liftM snd $ tLoadUserReg p

nSaveUser :: User -> TIO ()
nSaveUser usr = do
  userExist <- isThereAUser (u_username usr)
  case userExist of
    True -> throwEx $ userError $ "The user already exists: " ++ show (u_username usr)
    False -> do
      let dirname = dirName usr
      createDir dirname
      save    dirname usr

nCopyFile :: Username -> FilePath -> UsersFile -> TIO ()
nCopyFile = undefined

nListFiles :: Username -> TIO [UsersFile]
nListFiles = undefined

nGetFile :: Username -> UsersFile -> TIO (Maybe FilePath)
nGetFile = undefined

isThereAUser :: Username -> TIO Bool
isThereAUser uname = hasNoRollback $ do
  let dirname = dirName uname
  exist <- doesDirectoryExist dirname
  case exist of
    False -> return False
    True  -> isCorrectStructure dirname userDirStructure

nDoesUserExist :: Username -> TIO Bool
nDoesUserExist = hasNoRollback . doesDirectoryExist . dirName

nPersonalInfo :: Username -> TIO PersonalInfo -- (Role, String)
nPersonalInfo uname = do
  user <- nLoadUser uname
  return $ flip userCata user $ \role _ _ name timezone _lang ->
    PersonalInfo (role, name, timezone)

isUserDir :: FilePath -> TIO Bool
isUserDir = isCorrectDirStructure userDirStructure

nFilterUsers :: (User -> Bool) -> TIO [User]
nFilterUsers f = filterDirectory userDataDir isUserDir load (filter f)

nLoadUser :: Username -> TIO User
nLoadUser = load . dirName

nUserDescription :: Username -> TIO UserDesc
nUserDescription = liftM mkUserDescription . nLoadUser

nUpdateUser :: User -> TIO ()
nUpdateUser user = update (dirName . u_username $ user) user

nAdministratedCourses :: Username -> TIO [(CourseKey, Course)]
nAdministratedCourses u = do
  let dirname = joinPath [dirName u, "courseadmin"]
  (selectValidDirsFrom dirname isCourseDir) >>= (mapM tLoadCourse)

nAdministratedGroups :: Username -> TIO [(GroupKey, Group)]
nAdministratedGroups u = do
  let dirname = joinPath [dirName u, "groupadmin"]
  (selectValidDirsFrom dirname isGroupDir) >>= (mapM tLoadGroup)

courseDirPath :: CourseKey -> FilePath
courseDirPath (CourseKey c) = joinPath [courseDataDir, c]

groupDirPath :: GroupKey -> FilePath
groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

userRegDirPath :: UserRegKey -> FilePath
userRegDirPath = userRegKeyFold $ \u -> joinPath [userRegDataDir, u]

nLoadCourse :: CourseKey -> TIO Course
nLoadCourse c = do
  let p = courseDirPath c
  isC <- isCourseDir p
  -- GUARD: Course dir does not exist
  unless isC . throwEx . userError . join $ [str c, " course does not exist."]
  -- Course found
  liftM snd $ tLoadCourse p

tLoadCourse :: FilePath -> TIO (CourseKey, Course)
tLoadCourse = tLoadPersistenceObject CourseKey

tLoadUserReg :: FilePath -> TIO (UserRegKey, UserRegistration)
tLoadUserReg = tLoadPersistenceObject UserRegKey

nGroupKeysOfCourse :: CourseKey -> TIO [GroupKey]
nGroupKeysOfCourse c = do
  let p = courseDirPath c
      g = joinPath [p, "groups"]
  subdirs <- getSubDirectories g
  return . map (GroupKey . takeBaseName) $ subdirs

nCreateCourseAdmin :: Username -> CourseKey -> TIO ()
nCreateCourseAdmin u ck = do
  usr <- nLoadUser u
  case atLeastCourseAdmin . u_role $ usr of
    False -> throwEx . userError . join $ [str u, " is not course admin"]
    True  -> do
      link u ck "admins"
      link ck u "courseadmin"

nSubscribedToCourse :: CourseKey -> TIO [Username]
nSubscribedToCourse = objectsIn "users" Username isUserDir

nSubscribedToGroup :: GroupKey -> TIO [Username]
nSubscribedToGroup = objectsIn "users" Username isUserDir

nUnsubscribedFromCourse :: CourseKey -> TIO [Username]
nUnsubscribedFromCourse = objectsIn "unsubscribed" Username isUserDir

nUnsubscribedFromGroup :: GroupKey -> TIO [Username]
nUnsubscribedFromGroup = objectsIn "unsubscribed" Username isUserDir

isCorrectDirStructure :: DirStructure -> FilePath -> TIO Bool
isCorrectDirStructure d p = hasNoRollback $ isCorrectStructure p d

isGroupDir :: FilePath -> TIO Bool
isGroupDir = isCorrectDirStructure groupDirStructure

nLoadGroup :: GroupKey -> TIO Group
nLoadGroup g = do
  let p = groupDirPath g
  isG <- isGroupDir p
  -- GUARD: Group id does not exist
  unless isG . throwEx . userError . join $ [str g, " group does not exist."]
  liftM snd $ tLoadGroup p
  where
    groupDirPath :: GroupKey -> FilePath
    groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

admins :: (DirName k) => k -> TIO [Username]
admins k = do
  let dirname = joinPath [dirName k, "admins"]
  mapM (liftM u_username . load) =<< (selectValidDirsFrom dirname isUserDir)

nGroupAdmins :: GroupKey -> TIO [Username]
nGroupAdmins = admins

nCourseAdmins :: CourseKey -> TIO [Username]
nCourseAdmins = admins

nTestScriptsOfCourse :: CourseKey -> TIO [TestScriptKey]
nTestScriptsOfCourse = objectsIn "test-script" TestScriptKey isTestScriptDir

nIsUserInGroup :: Username -> GroupKey -> TIO Bool
nIsUserInGroup u gk = isLinkedIn u gk "users"

nIsUserInCourse :: Username -> CourseKey -> TIO Bool
nIsUserInCourse u ck = isLinkedIn u ck "users"

nSubscribe :: Username -> CourseKey -> GroupKey -> TIO ()
nSubscribe username ck gk = do
  link username gk "users"
  link username ck "users"
  link gk username "group"
  link ck username "course"

nUnsubscribe :: Username -> CourseKey -> GroupKey -> TIO ()
nUnsubscribe username ck gk = do
  unlink username gk "users"
  unlink username ck "users"
  unlink gk username "group"
  unlink ck username "course"
  link username gk "unsubscribed"
  link username ck "unsubscribed"

nCreateGroupAdmin :: Username -> GroupKey -> TIO ()
nCreateGroupAdmin u gk = do
  link u gk "admins"
  link gk u "groupadmin"

nCourseOfGroup :: GroupKey -> TIO CourseKey
nCourseOfGroup = objectIn' "No course was found for " "course" CourseKey isCourseDir

tLoadPersistenceObject :: (Load o)
  => (String -> k) -- ^ Key constructor
  -> FilePath      -- ^ Base path
  -> TIO (k,o)     -- ^ Key and the loaded object
tLoadPersistenceObject f d = do
  let key = takeBaseName d
  object <- load d
  return (f key, object)

tLoadGroup :: FilePath -> TIO (GroupKey, Group)
tLoadGroup = tLoadPersistenceObject GroupKey

nSaveCourse :: Course -> TIO CourseKey
nSaveCourse c = do
  dirName <- createTmpDir courseDataDir "cr"
  let courseKey = CourseKey . takeBaseName $ dirName
  save dirName c
  return courseKey

nUserCourses :: Username -> TIO [CourseKey]
nUserCourses u = do
  let dirname = joinPath [dirName u, "course"]
  map (CourseKey . takeBaseName) <$> (selectValidDirsFrom dirname isCourseDir)

nUserGroups :: Username -> TIO [GroupKey]
nUserGroups u = do
  let dirname = joinPath [dirName u, "group"]
  map (GroupKey . takeBaseName) <$> (selectValidDirsFrom dirname isGroupDir)

class ForeignKey k where
  referredPath :: k -> DirPath
  baseName     :: k -> String

foreignKey :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
foreignKey object linkto subdir =
  createLink
    (joinPath ["..", "..", "..", "..", (referredPath object)])
    (joinPath [(referredPath linkto), subdir, baseName object])

removeForeignKey :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
removeForeignKey object linkto subdir =
  deleteLink
    (joinPath ["..", "..", "..", "..", (referredPath object)])
    (joinPath [(referredPath linkto), subdir, baseName object])

isLinkedIn :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO Bool
isLinkedIn object linkto subdir =
  hasNoRollback . doesDirectoryExist . joinPath $ [referredPath linkto, subdir, baseName object]

link :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
link object linkto subdir = do
  exist <- isLinkedIn object linkto subdir
  unless exist $ foreignKey object linkto subdir

unlink :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
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

{- * One primitve value is stored in the file with the same name as the row.
   * One combined value is stored in the given directory into many files. The name
     of the directory is the primary key for the record.
   * The foreign keys are the symlinks for the other row of the given combined object.
-}
nSaveGroup :: CourseKey -> Group -> TIO GroupKey
nSaveGroup ck group = do
  dirName <- createTmpDir groupDataDir "gr"
  let groupKey = GroupKey . takeBaseName $ dirName
  save dirName group
  link groupKey ck "groups"
  link ck groupKey "course"
  return groupKey

nFilterGroups :: (GroupKey -> Group -> Bool) -> TIO [(GroupKey, Group)]
nFilterGroups f = filterDirectory groupDataDir isGroupDir tLoadGroup (filter (uncurry f))

currentTime :: TIO UTCTime
currentTime = hasNoRollback getCurrentTime

nSaveAssignment :: Assignment -> TIO AssignmentKey
nSaveAssignment a = do
  dirName <- createTmpDir assignmentDataDir "a"
  let assignmentKey = takeBaseName dirName
  save dirName a
  saveCreatedTime dirName =<< currentTime
  return . AssignmentKey $ assignmentKey

nModifyAssignment :: AssignmentKey -> Assignment -> TIO ()
nModifyAssignment ak a = do
  let p = assignmentDirPath ak
  isA <- isAssignmentDir p
  unless isA . throwEx . userError $ "Assignment does not exist"
  update p a

nAssignmentCreatedTime :: AssignmentKey -> TIO UTCTime
nAssignmentCreatedTime ak = do
  let p = assignmentDirPath ak
  isDir <- isAssignmentDir p
  case isDir of
    False -> throwEx $ userError $ join [str ak, " assignment does not exist."]
    True  -> getCreatedTime p

selectValidDirsFrom :: FilePath -> (FilePath -> TIO Bool) -> TIO [FilePath]
selectValidDirsFrom dir isValidDir = getSubDirectories dir >>= filterM isValidDir

nAssignmentKeys :: TIO [AssignmentKey]
nAssignmentKeys =
  (selectValidDirsFrom assignmentDataDir isAssignmentDir) >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (AssignmentKey . takeBaseName)

nFilterAssignment :: (AssignmentKey -> Assignment -> Bool) -> TIO [(AssignmentKey, Assignment)]
nFilterAssignment f = filterDirectory assignmentDataDir isAssignmentDir tLoadAssignment (filter (uncurry f))

nLoadAssignment :: AssignmentKey -> TIO Assignment
nLoadAssignment a = do
  let p = assignmentDirPath a
  isEx <- isAssignmentDir p
  case isEx of
    False -> throwEx $ userError $ join [str a, " assignment does not exist."]
    True  -> liftM snd $ tLoadAssignment p

assignmentDirPath :: AssignmentKey -> FilePath
assignmentDirPath (AssignmentKey e) = joinPath [assignmentDataDir, e]

tLoadAssignment :: FilePath -> TIO (AssignmentKey, Assignment)
tLoadAssignment dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (AssignmentKey exerciseKey, e)


objectsIn
  :: (Show sk, DirName sk)
  => FilePath               -- Subdir where to look at
  -> (String -> rk)         -- Result's key constructor
  -> (FilePath -> TIO Bool) -- Checks if the given subdir is appropiate
  -> sk                     -- The source's key
  -> TIO [rk]
objectsIn subdir keyConstructor isValidDir sourceKey = do
  let dirname = joinPath [dirName sourceKey, subdir]
  map (keyConstructor . takeBaseName) <$> (selectValidDirsFrom dirname isValidDir)

objectIn s k v sk = fmap just $ objectsIn s k v sk
  where
    just []  = Nothing
    just [k] = Just k
    just _   = error $ "Impossible: found more than one object found for: " ++ show sk

objectIn' msg subdir keyConstructor isValidDir sourceKey = do
  m <- objectIn subdir keyConstructor isValidDir sourceKey
  case m of
    Nothing -> error $ msg ++ show sourceKey
    Just  x -> return x

nCourseOfAssignment :: AssignmentKey -> TIO (Maybe CourseKey)
nCourseOfAssignment = objectIn "course" CourseKey isCourseDir

nGroupOfAssignment :: AssignmentKey -> TIO (Maybe GroupKey)
nGroupOfAssignment = objectIn  "group" GroupKey isGroupDir

nTestCaseOfAssignment :: AssignmentKey -> TIO (Maybe TestCaseKey)
nTestCaseOfAssignment = objectIn "test-case" TestCaseKey isTestCaseDir

nSubmissionsForAssignment :: AssignmentKey -> TIO [SubmissionKey]
nSubmissionsForAssignment = objectsIn "submission" SubmissionKey isSubmissionDir

saveAndLinkAssignment :: (ForeignKey k) => FilePath -> k -> Assignment -> TIO AssignmentKey
saveAndLinkAssignment subdir k a = do
  ak <- nSaveAssignment a
  link ak k "assignments"
  link k ak subdir
  return ak

nSaveCourseAssignment :: CourseKey -> Assignment -> TIO AssignmentKey
nSaveCourseAssignment = saveAndLinkAssignment "course"

nSaveGroupAssignment :: GroupKey  -> Assignment -> TIO AssignmentKey
nSaveGroupAssignment = saveAndLinkAssignment "group"

isAssignmentDir :: FilePath -> TIO Bool
isAssignmentDir = isCorrectDirStructure assignmentDirStructure

assignmentsFor :: (a -> FilePath) -> a -> TIO [AssignmentKey]
assignmentsFor dirPath k = do
  fp <- (selectValidDirsFrom (joinPath [dirPath k, "assignments"]) isAssignmentDir)
  return ((AssignmentKey . takeBaseName) <$> fp)

nCourseAssignments :: CourseKey -> TIO [AssignmentKey]
nCourseAssignments = assignmentsFor courseDirPath

nGroupAssignments :: GroupKey -> TIO [AssignmentKey]
nGroupAssignments = assignmentsFor groupDirPath

nCourseKeys :: TIO [CourseKey]
nCourseKeys =
  (selectValidDirsFrom courseDataDir isCourseDir) >>=
  calcCourseKeys
    where
      calcCourseKeys = return . map (CourseKey . takeBaseName)

isCourseDir :: FilePath -> TIO Bool
isCourseDir = isCorrectDirStructure courseDirStructure

isUserRegDir :: FilePath -> TIO Bool
isUserRegDir = isCorrectDirStructure userRegDirStructure

nFilterCourses :: (CourseKey -> Course -> Bool) -> TIO [(CourseKey, Course)]
nFilterCourses f = filterDirectory courseDataDir isCourseDir tLoadCourse (filter (uncurry f))

-- * Submission

isSubmissionDir :: FilePath -> TIO Bool
isSubmissionDir = isCorrectDirStructure submissionDirStructure


nSaveSubmission :: AssignmentKey -> Username -> Submission -> TIO SubmissionKey
nSaveSubmission ak u s = do
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
      linkUserSubmission :: SubmissionKey -> TIO ()
      linkUserSubmission sk = do
        let dirName = userAsgSubmissionDir u ak
        createDirIfMissing dirName
        createLink
          (joinPath ["..","..","..","..","..",(referredPath sk)])
          (joinPath [dirName, (baseName sk)])

userAsgSubmissionDir :: Username -> AssignmentKey -> FilePath
userAsgSubmissionDir u ak = joinPath [referredPath u, "submissions", baseName ak]

nLastSubmission :: AssignmentKey -> Username -> TIO (Maybe SubmissionKey)
nLastSubmission ak u = do
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
nLoadSubmission :: SubmissionKey -> TIO Submission
nLoadSubmission = load . dirName

nAssignmentOfSubmission :: SubmissionKey -> TIO AssignmentKey
nAssignmentOfSubmission sk =
  objectIn' "No assignment was found for the " "assignment" AssignmentKey isAssignmentDir sk

nUsernameOfSubmission :: SubmissionKey -> TIO Username
nUsernameOfSubmission sk =
  objectIn' "No assignment was found for the " "user" Username isUserDir sk

nEvaluationOfSubmission :: SubmissionKey -> TIO (Maybe EvaluationKey)
nEvaluationOfSubmission =
  objectIn "evaluation" EvaluationKey isEvaluationDir

nFilterSubmissions :: (SubmissionKey -> Submission -> Bool) -> TIO [(SubmissionKey, Submission)]
nFilterSubmissions f = filterDirectory submissionDataDir isSubmissionDir tLoadSubmission (filter (uncurry f))

tLoadSubmission :: FilePath -> TIO (SubmissionKey, Submission)
tLoadSubmission dirName = do
  s <- load dirName
  return (SubmissionKey . takeBaseName $ dirName, s)

openedSubmissionDataDirPath :: AssignmentKey -> Username -> FilePath
openedSubmissionDataDirPath ak u =
  joinPath [openSubmissionDataDir, "assignment", baseName ak, baseName u]

nPlaceToOpened :: AssignmentKey -> Username -> SubmissionKey -> TIO ()
nPlaceToOpened ak u sk = do
  let lookupPath = openedSubmissionDataDirPath ak u
  createLink
    (joinPath ["..", "..", "..", (referredPath sk)])
    (joinPath [openSubmissionAllDataDir, baseName sk])
  createDirIfMissing lookupPath
  createLink
    (joinPath ["..", "..", "..", "..", "..", (referredPath sk)])
    (joinPath [lookupPath, baseName sk])

nRemoveFromOpened :: AssignmentKey -> Username -> SubmissionKey -> TIO ()
nRemoveFromOpened ak u sk = do
  removeSymLink (joinPath [openSubmissionAllDataDir, baseName sk])
  removeSymLink (joinPath [openedSubmissionDataDirPath ak u, baseName sk])

nUsersOpenedSubmissions :: AssignmentKey -> Username -> TIO [SubmissionKey]
nUsersOpenedSubmissions ak u = do
  let path = openedSubmissionDataDirPath ak u
  exists <- doesDirExist path
  if exists
    then filterDirectory path isSubmissionDir tLoadSubmission (map fst)
    else return []

filterDirectory :: FilePath -> (FilePath -> TIO Bool) -> (FilePath -> TIO a) -> ([a] -> [b]) -> TIO [b]
filterDirectory dir isValid loader f = f <$> ((selectValidDirsFrom dir isValid) >>= (mapM loader))

-- | First it checks if the directory exist, if not the result is an empty list
--   else, the result is the original filtered data
safeFilterDirectory :: FilePath -> (FilePath -> TIO Bool) -> (FilePath -> TIO a) -> ([a] -> [b]) -> TIO [b]
safeFilterDirectory dir isValid loader f = do
  e <- doesDirExist dir
  case e of
    False -> return []
    True  -> filterDirectory dir isValid loader f

nOpenedSubmission :: TIO [SubmissionKey]
nOpenedSubmission = filterDirectory openSubmissionAllDataDir isSubmissionDir tLoadSubmission (map fst)

userDirPath :: Username -> FilePath
userDirPath (Username u) = joinPath [userDataDir, u]

submissionDirPath :: SubmissionKey -> FilePath
submissionDirPath (SubmissionKey sk) = joinPath [submissionDataDir, sk]

nUserSubmissions :: Username -> AssignmentKey -> TIO [SubmissionKey]
nUserSubmissions u ak =
  safeFilterDirectory
    (joinPath [userDirPath u, "submissions", baseName ak])
    isSubmissionDir
    (return . takeBaseName)
    (map SubmissionKey)

nCommentsOfSubmission :: SubmissionKey -> TIO [CommentKey]
nCommentsOfSubmission sk =
  filterDirectory
    (joinPath [submissionDirPath sk, "comment"])
    isCommentDir
    (return . takeBaseName)
    (map CommentKey)

-- * Evaluation

instance ForeignKey EvaluationKey where
  referredPath (EvaluationKey e) = joinPath [evaluationDataDir, e]
  baseName     (EvaluationKey e) = e

isEvaluationDir :: FilePath -> TIO Bool
isEvaluationDir = isCorrectDirStructure evaluationDirStructure

nSaveEvaluation :: SubmissionKey -> Evaluation -> TIO EvaluationKey
nSaveEvaluation sk e = do
  dirName <- createTmpDir evaluationDataDir "ev"
  let evKey = EvaluationKey . takeBaseName $ dirName
  save dirName e
  link evKey sk "evaluation"
  link sk evKey "submission"
  return evKey

evaluationDirPath :: EvaluationKey -> FilePath
evaluationDirPath (EvaluationKey e) = joinPath [evaluationDataDir, e]

nLoadEvaluation :: EvaluationKey -> TIO Evaluation
nLoadEvaluation e = do
  let p = evaluationDirPath e
  isE <- isEvaluationDir p
  unless isE . throwEx . userError . join $ ["Evaluation does not exist."]
  liftM snd . tLoadPersistenceObject EvaluationKey $ p

nModifyEvaluation :: EvaluationKey -> Evaluation -> TIO ()
nModifyEvaluation ek e = do
  let p = evaluationDirPath ek
  isE <- isEvaluationDir p
  unless isE . throwEx . userError . join $ ["Evaluation does not exist."]
  update p e

nSubmissionOfEvaluation :: EvaluationKey -> TIO SubmissionKey
nSubmissionOfEvaluation =
  objectIn' "No submission was found for " "submission" SubmissionKey isSubmissionDir

-- * Comment

instance ForeignKey CommentKey where
  referredPath (CommentKey c) = joinPath [commentDataDir, c]
  baseName     (CommentKey c) = c

commentDirPath :: CommentKey -> FilePath
commentDirPath (CommentKey c) = joinPath [commentDataDir, c]

isCommentDir :: FilePath -> TIO Bool
isCommentDir = isCorrectDirStructure commentDirStructure

nSaveComment :: SubmissionKey -> Comment -> TIO CommentKey
nSaveComment sk c = do
  dirName <- createTmpDir commentDataDir "cm"
  let key = CommentKey . takeBaseName $ dirName
  save dirName c
  link key sk "comment"
  link sk key "submission"
  return key

nLoadComment :: CommentKey -> TIO Comment
nLoadComment ck = do
  let p = commentDirPath ck
  isC <- isCommentDir p
  unless isC . throwEx $ userError "Comment does not exist."
  liftM snd . tLoadPersistenceObject CommentKey $ p

nSubmissionOfComment :: CommentKey -> TIO SubmissionKey
nSubmissionOfComment =
  objectIn' "No submission was found for " "submission" SubmissionKey isSubmissionDir

-- * Test Script

testScriptDirPath :: TestScriptKey -> FilePath
testScriptDirPath = testScriptKeyCata $ \k -> joinPath [testScriptDataDir, k]

isTestScriptDir :: FilePath -> TIO Bool
isTestScriptDir = isCorrectDirStructure testScriptDirStructure

instance ForeignKey TestScriptKey where
  referredPath = testScriptDirPath
  baseName     (TestScriptKey k) = k

nSaveTestScript :: CourseKey -> TestScript -> TIO TestScriptKey
nSaveTestScript ck ts = do
  dirName <- createTmpDir testScriptDataDir "ts"
  let key = TestScriptKey $ takeBaseName dirName
  save dirName ts
  link key ck "test-script"
  link ck key "course"
  return key

nLoadTestScript :: TestScriptKey -> TIO TestScript
nLoadTestScript tk = do
  let p = testScriptDirPath tk
  isTS <- isTestScriptDir p
  unless isTS . throwEx $ userError "Not a test script directory"
  snd <$> tLoadPersistenceObject TestScriptKey p

nCourseOfTestScript :: TestScriptKey -> TIO CourseKey
nCourseOfTestScript =
  objectIn' "No course was found for " "course" CourseKey isCourseDir

nModifyTestScript :: TestScriptKey -> TestScript -> TIO ()
nModifyTestScript tk ts = do
  let p = testScriptDirPath tk
  isTS <- isTestScriptDir p
  unless isTS . throwEx $ userError "Test Script does not exist"
  update p ts

-- * Test Case

testCaseDirPath :: TestCaseKey -> FilePath
testCaseDirPath = testCaseKeyCata $ \k -> joinPath [testCaseDataDir, k]

isTestCaseDir :: FilePath -> TIO Bool
isTestCaseDir = isCorrectDirStructure testCaseDirStructure

instance ForeignKey TestCaseKey where
  referredPath = testCaseDirPath
  baseName (TestCaseKey k) = k

nSaveTestCase :: TestScriptKey -> AssignmentKey -> TestCase -> TIO TestCaseKey
nSaveTestCase tk ak tc = do
  dirName <- createTmpDir testCaseDataDir "tc"
  let key = TestCaseKey $ takeBaseName dirName
  save dirName tc
  link key ak "test-case"
  link ak key "assignment"
  link tk key "test-script"
  return key

nLoadTestCase :: TestCaseKey -> TIO TestCase
nLoadTestCase tk = do
  let p = testCaseDirPath tk
  isTC <- isTestCaseDir p
  unless isTC . throwEx $ userError "Not a test case directory"
  snd <$> tLoadPersistenceObject TestCaseKey p

nAssignmentOfTestCase :: TestCaseKey -> TIO AssignmentKey
nAssignmentOfTestCase =
  objectIn' "No assignment was found for " "assignment" AssignmentKey isAssignmentDir

nTestScriptOfTestCase :: TestCaseKey -> TIO TestScriptKey
nTestScriptOfTestCase =
  objectIn' "No Test Script was found for " "test-script" TestScriptKey isTestScriptDir

nModifyTestCase :: TestCaseKey -> TestCase -> TIO ()
nModifyTestCase tk tc = do
  let p = testCaseDirPath tk
  isTC <- isTestCaseDir p
  unless isTC . throwEx $ userError "Test Case does not exist"
  update p tc

-- * Tools

encodePwd :: String -> String
encodePwd = ordEncode
