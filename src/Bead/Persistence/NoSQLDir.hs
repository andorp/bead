module Bead.Persistence.NoSQLDir (
    noSqlDirPersist
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.NoSQL.Loader
import Control.Monad.Transaction.TIO

import Control.Applicative ((<$>))
import Control.Monad (join, mapM, liftM, filterM, when, unless)
import Control.Exception (IOException, throwIO)
import System.FilePath (joinPath, takeBaseName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)

-- | Simple directory and file based NoSQL persistence implementation
noSqlDirPersist = Persist {
    saveUser      = nSaveUser
  , canUserLogin  = nCanUserLogin
  , personalInfo  = nPersonalInfo
  , updatePwd     = nUpdatePwd
  , filterUsers   = nFilterUsers
  , loadUser      = nLoadUser
  , updateUser    = nUpdateUser
  , doesUserExist = nDoesUserExist
  , administratedCourses = nAdministratedCourses
  , administratedGroups = nAdministratedGroups

  , saveCourse    = nSaveCourse
  , courseKeys    = nCourseKeys
  , filterCourses = nFilterCourses
  , loadCourse    = nLoadCourse
  , groupKeysOfCourse = nGroupKeysOfCourse
  , isUserInCourse = nIsUserInCourse
  , userCourses = nUserCourses
  , createCourseAdmin = nCreateCourseAdmin

  , saveGroup     = nSaveGroup
  , loadGroup     = nLoadGroup
  , isUserInGroup = nIsUserInGroup
  , userGroups    = nUserGroups
  , subscribe     = nSubscribe
  , createGroupProfessor = nCreateGroupProfessor

  , filterAssignment     = nFilterAssignment
  , assignmentKeys       = nAssignmentKeys
  , saveAssignment       = nSaveAssignment
  , loadAssignment       = nLoadAssignment
  , saveCourseAssignment = nSaveCourseAssignment
  , saveGroupAssignment  = nSaveGroupAssignment
  , courseAssignments    = nCourseAssignments
  , groupAssignments     = nGroupAssignments
--  , courseOfAssignment   = nCourseOfAssignment
--  , groupOfAssignment    = nGroupOfAssignemnt
  
  , saveSubmission = nSaveSubmission
  , loadSubmission = nLoadSubmission

  , isPersistenceSetUp = nIsPersistenceSetUp
  , initPersistence    = nInitPersistence
  }

nIsPersistenceSetUp :: IO Bool
nIsPersistenceSetUp = do
  dirsExist <- mapM doesDirectoryExist persistenceDirs
  return $ and dirsExist

nInitPersistence :: IO ()
nInitPersistence = mapM_ createDirWhenDoesNotExist persistenceDirs
  where
    createDirWhenDoesNotExist d = do
      existDir <- doesDirectoryExist d
      unless existDir . createDirectory $ d

nSaveUser :: User -> Password -> TIO ()
nSaveUser usr pwd = do
  userExist <- isThereAUser (u_username usr)
  case userExist of
    True -> throwEx $ userError $ "The user already exists: " ++ show (u_username usr)
    False -> do
      let ePwd = encodePwd pwd
          dirname = dirName usr
      createDir dirname
      save    dirname usr
      savePwd dirname ePwd

isThereAUser :: Username -> TIO Bool
isThereAUser uname = hasNoRollback $ do
  let dirname = dirName uname
  exist <- doesDirectoryExist dirname
  case exist of
    False -> return False
    True  -> isCorrectStructure dirname userDirStructure

nCanUserLogin :: Username -> Password -> TIO Bool
nCanUserLogin u p = tCanUserLogin u p

tCanUserLogin :: Username -> Password -> TIO Bool
tCanUserLogin uname pwd = do
  exists <- tDoesUserExist uname
  case exists of
    False -> return False
    True  -> do
      ePwd <- loadPwd . dirName $ uname
      return (ePwd == (encodePwd pwd))

nDoesUserExist :: Username -> TIO Bool
nDoesUserExist = tDoesUserExist

tDoesUserExist :: Username -> TIO Bool
tDoesUserExist = hasNoRollback . doesDirectoryExist . dirName

nPersonalInfo :: Username -> Password -> TIO (Role, String)
nPersonalInfo uname pwd = do
  userExist <- isThereAUser uname
  case userExist of
    False -> throwEx . userError $ "User doesn't exist: " ++ show uname
    True -> do
      let ePwd = encodePwd pwd
          dirname = dirName uname
      role       <- load dirname
      familyName <- loadName dirname
      return (role, familyName)

isUserDir :: FilePath -> TIO Bool
isUserDir = isCorrectDirStructure userDirStructure

nFilterUsers :: (User -> Bool) -> TIO [User]
nFilterUsers f =
  (selectValidDirsFrom userDataDir isUserDir) >>=
  (mapM load)                                 >>=
  (return . filter f)

tLoadUser :: Username -> TIO User
tLoadUser = load . dirName

nLoadUser :: Username -> TIO User
nLoadUser = tLoadUser

nUpdateUser :: User -> TIO ()
nUpdateUser user = update (dirName . u_username $ user) user

nUpdatePwd :: Username -> Password -> Password -> TIO ()
nUpdatePwd uname oldPwd newPwd = do
  userExist <- tCanUserLogin uname oldPwd
  case userExist of
    False -> throwEx $ userError $ "Invalid user and/or password combination: " ++ show uname
    True -> do
      let ePwd = encodePwd oldPwd
          dirname = dirName uname
      oldEPwd <- loadPwd dirname
      case ePwd == oldEPwd of
        False -> throwEx . userError $ "Invalid password"
        True  -> savePwd dirname $ encodePwd newPwd

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

nGroupKeysOfCourse :: CourseKey -> TIO [GroupKey]
nGroupKeysOfCourse c = do
  let p = courseDirPath c
      g = joinPath [p, "groups"]
  subdirs <- getSubDirectories g
  return . map (GroupKey . takeBaseName) $ subdirs

nCreateCourseAdmin :: Username -> CourseKey -> TIO ()
nCreateCourseAdmin u ck = do
  usr <- tLoadUser u
  case atLeastCourseAdmin . u_role $ usr of
    False -> throwEx . userError . join $ [str u, " is not course admin"]
    True  -> do
      link u ck "admins"
      link ck u "courseadmin"

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

nIsUserInGroup :: Username -> GroupKey -> TIO Bool
nIsUserInGroup u gk = isLinkedIn gk u "users"

nIsUserInCourse :: Username -> CourseKey -> TIO Bool
nIsUserInCourse u ck = isLinkedIn ck u "users"

nSubscribe :: Username -> CourseKey -> GroupKey -> TIO ()
nSubscribe username ck gk = do
  link username gk "users"
  link username ck "users"
  link gk username "group"
  link ck username "course"

nCreateGroupProfessor :: Username -> GroupKey -> TIO ()
nCreateGroupProfessor u gk = do
  link u gk "admins"
  link gk u "groupadmin"

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

isLinkedIn :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO Bool
isLinkedIn object linkto subdir =
  hasNoRollback . doesDirectoryExist . joinPath $ [referredPath object, subdir, baseName linkto]

link :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
link object linkto subdir = do
  exist <- isLinkedIn object linkto subdir
  unless exist $ foreignKey object linkto subdir


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

nSaveAssignment :: Assignment -> TIO AssignmentKey
nSaveAssignment = tSaveAssignment

tSaveAssignment :: Assignment -> TIO AssignmentKey
tSaveAssignment a = do
  dirName <- createTmpDir assignmentDataDir "a"
  let assignmentKey = takeBaseName dirName
  save dirName a
  return . AssignmentKey $ assignmentKey
  
selectValidDirsFrom :: FilePath -> (FilePath -> TIO Bool) -> TIO [FilePath]
selectValidDirsFrom dir isValidDir = getSubDirectories dir >>= filterM isValidDir

nAssignmentKeys :: TIO [AssignmentKey]
nAssignmentKeys =
  (selectValidDirsFrom assignmentDataDir isAssignmentDir) >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (AssignmentKey . takeBaseName)

nFilterAssignment :: (AssignmentKey -> Assignment -> Bool) -> TIO [(AssignmentKey, Assignment)]
nFilterAssignment f =
  (selectValidDirsFrom assignmentDataDir isAssignmentDir) >>=
  (mapM tLoadAssignment)                                  >>=
  (return . filter (uncurry f))

nLoadAssignment :: AssignmentKey -> TIO Assignment
nLoadAssignment e = do
  let p = exerciseDirPath e
  isEx <- isAssignmentDir p
  case isEx of
    False -> throwEx $ userError $ join [str e, " exercise does not exist."]
    True  -> liftM snd $ tLoadAssignment p
  where
    exerciseDirPath :: AssignmentKey -> FilePath
    exerciseDirPath (AssignmentKey e) = joinPath [assignmentDataDir, e]

tLoadAssignment :: FilePath -> TIO (AssignmentKey, Assignment)
tLoadAssignment dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (AssignmentKey exerciseKey, e)

saveAndLinkAssignment :: (ForeignKey k) => FilePath -> k -> Assignment -> TIO AssignmentKey
saveAndLinkAssignment subdir k a = do
  ak <- tSaveAssignment a
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

nFilterCourses :: (CourseKey -> Course -> Bool) -> TIO [(CourseKey, Course)]
nFilterCourses f =
  (selectValidDirsFrom courseDataDir isCourseDir) >>=
  (mapM tLoadCourse)             >>=
  (return . filter (uncurry f))

-- * Submission

isSubmissionDir :: FilePath -> TIO Bool
isSubmissionDir = isCorrectDirStructure submissionDirStructure

nSaveSubmission :: AssignmentKey -> Username -> Submission -> TIO SubmissionKey
nSaveSubmission ek u s = do
  dirName <- createTmpDir submissionDataDir "s"
  let submissionKey = SubmissionKey . takeBaseName $ dirName
  save dirName s
  link ek submissionKey "exercise"
  link u  submissionKey "user"
  link submissionKey  u "submissions"
  return submissionKey

nLoadSubmission :: SubmissionKey -> TIO Submission
nLoadSubmission sk = undefined

-- * Tools

encodePwd :: String -> String
encodePwd = ordEncode
