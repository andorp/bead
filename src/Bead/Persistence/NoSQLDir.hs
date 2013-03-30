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
    saveUser      = nSaveUser      -- :: User -> Password -> IO (Erroneous ())
  , canUserLogin  = nCanUserLogin  -- :: Username -> Password -> IO (Erroneous Bool)
  , personalInfo  = nPersonalInfo  -- :: Username -> Password -> IO (Erroneous (Role, String))
  , updatePwd     = nUpdatePwd     -- :: Username -> Password -> Password -> IO (Erroneous ())
  , filterUsers   = nFilterUsers   -- :: (User -> Bool) -> IO (Erroneous [User])
  , loadUser      = nLoadUser      -- :: Username -> IO (Erroneous User)
  , updateUser    = nUpdateUser    -- :: Username -> User -> IO (Erroneous ())
  , doesUserExist = nDoesUserExist -- :: Username -> IO (Erroneous Bool)
  , administratedCourses = nAdministratedCourses -- :: Username -> IO (Erroneous [CourseKey])
  , administratedGroups = nAdministratedGroups -- :: Username -> IO (Erroneous [GroupKey])

  , saveCourse    = nSaveCourse    -- :: Course -> IO (Erroneous ())
  , courseKeys    = nCourseKeys    -- :: IO (Erroneous [CourseKey])
  , filterCourses = nFilterCourses -- :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
  , loadCourse    = nLoadCourse    -- :: CourseKey -> IO (Erroneous Course)
  , groupKeysOfCourse = nGroupKeysOfCourse -- :: CourseKey -> IO (Erroneous [GroupKey])
  , isUserInCourse = nIsUserInCourse -- :: Username -> CourseKey -> IO (Erroneous Bool)
  , createCourseAdmin = nCreateCourseAdmin -- :: Username -> CourseKey -> IO (Erroneous ())

  , saveGroup     = nSaveGroup     -- :: CourseKey -> Group -> IO (Erroneous GroupKey)
  , loadGroup     = nLoadGroup     -- :: GroupKey -> IO (Erroneous Group)
  , isUserInGroup = nIsUserInGroup -- :: Username -> GroupKey -> IO (Erroneous Bool)
  , subscribe     = nSubscribe     -- :: Username -> CourseKey -> GroupKey -> IO (Erroneous ())
  , createGroupProfessor = nCreateGroupProfessor -- :: Username -> GroupKey -> IO (Erroneous ())

  , filterExercises = nFilterExercises -- :: (AssignmentKey -> Assignment -> Bool) -> IO (Erroneous [(AssignmentKey,Assignment)])
  , exerciseKeys  = nExerciseKeys  -- :: IO (Erroneous [AssignmentKey])
  , saveExercise  = nSaveExercise  -- :: Assignment -> IO (Erroneous AssignmentKey)
  , loadExercise    = nLoadExercise  -- :: AssignmentKey -> IO (Erroneous Assignment)
  , saveCourseAssignment = nSaveCourseAssignment -- :: CourseKey -> Assignment -> IO (Erroneous AssignmentKey)
  , saveGroupAssignment = nSaveGroupAssignment   -- :: GroupKey  -> Assignment -> IO (Erroneous AssignmentKey)
  , courseAssignments = nCourseAssignments -- :: CourseKey -> IO (Erroneous [AssignmentKey])
  , groupAssignments = nGroupAssignments -- :: GroupKey -> IO (Erroneous [AssignmentKey])
  
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

nSaveUser :: User -> Password -> IO (Erroneous ())
nSaveUser usr pwd = runAtomically $ do
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

nCanUserLogin :: Username -> Password -> IO (Erroneous Bool)
nCanUserLogin u p = runAtomically $ tCanUserLogin u p

tCanUserLogin :: Username -> Password -> TIO Bool
tCanUserLogin uname pwd = do
  exists <- tDoesUserExist uname
  case exists of
    False -> return False
    True  -> do
      ePwd <- loadPwd . dirName $ uname
      return (ePwd == (encodePwd pwd))

nDoesUserExist :: Username -> IO (Erroneous Bool)
nDoesUserExist = runAtomically . tDoesUserExist

tDoesUserExist :: Username -> TIO Bool
tDoesUserExist = hasNoRollback . doesDirectoryExist . dirName

nPersonalInfo :: Username -> Password -> IO (Erroneous (Role, String))
nPersonalInfo uname pwd = runAtomically $ do
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

nFilterUsers :: (User -> Bool) -> IO (Erroneous [User])
nFilterUsers f = runAtomically $
  (selectValidDirsFrom userDataDir isUserDir) >>=
  (mapM load)                                 >>=
  (return . filter f)

tLoadUser :: Username -> TIO User
tLoadUser = load . dirName

nLoadUser :: Username -> IO (Erroneous User)
nLoadUser = runAtomically . tLoadUser

nUpdateUser :: User -> IO (Erroneous ())
nUpdateUser user = runAtomically $ update (dirName . u_username $ user) user

nUpdatePwd :: Username -> Password -> Password -> IO (Erroneous ())
nUpdatePwd uname oldPwd newPwd = runAtomically $ do
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

nAdministratedCourses :: Username -> IO (Erroneous [(CourseKey, Course)])
nAdministratedCourses u = runAtomically $ do
  let dirname = joinPath [dirName u, "courseadmin"]
  (selectValidDirsFrom dirname isCourseDir) >>= (mapM tLoadCourse)

nAdministratedGroups :: Username -> IO (Erroneous [(GroupKey, Group)])
nAdministratedGroups u = runAtomically $ do
  let dirname = joinPath [dirName u, "groupadmin"]
  (selectValidDirsFrom dirname isGroupDir) >>= (mapM tLoadGroup)

courseDirPath :: CourseKey -> FilePath
courseDirPath (CourseKey c) = joinPath [courseDataDir, c]

groupDirPath :: GroupKey -> FilePath
groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

nLoadCourse :: CourseKey -> IO (Erroneous Course)
nLoadCourse c = runAtomically $ do
  let p = courseDirPath c
  isC <- isCourseDir p
  -- GUARD: Course dir does not exist
  unless isC . throwEx . userError . join $ [str c, " course does not exist."]
  -- Course found
  liftM snd $ tLoadCourse p

tLoadCourse :: FilePath -> TIO (CourseKey, Course)
tLoadCourse = tLoadPersistenceObject CourseKey

nGroupKeysOfCourse :: CourseKey -> IO (Erroneous [GroupKey])
nGroupKeysOfCourse c = runAtomically $ do
  let p = courseDirPath c
      g = joinPath [p, "groups"]
  subdirs <- getSubDirectories g
  return . map (GroupKey . takeBaseName) $ subdirs

nCreateCourseAdmin :: Username -> CourseKey -> IO (Erroneous ())
nCreateCourseAdmin u ck = runAtomically $ do
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

nLoadGroup :: GroupKey -> IO (Erroneous Group)
nLoadGroup g = runAtomically $ do
  let p = groupDirPath g
  isG <- isGroupDir p
  -- GUARD: Group id does not exist
  unless isG . throwEx . userError . join $ [str g, " group does not exist."]
  liftM snd $ tLoadGroup p
  where
    groupDirPath :: GroupKey -> FilePath
    groupDirPath (GroupKey g) = joinPath [groupDataDir, g]

nIsUserInGroup :: Username -> GroupKey -> IO (Erroneous Bool)
nIsUserInGroup u gk = runAtomically $ isLinkedIn gk u "users"

nIsUserInCourse :: Username -> CourseKey -> IO (Erroneous Bool)
nIsUserInCourse u ck = runAtomically $ isLinkedIn ck u "users"

nSubscribe :: Username -> CourseKey -> GroupKey -> IO (Erroneous ())
nSubscribe username ck gk = runAtomically $ do
  link username gk "users"
  link username ck "users"
  link gk username "group"
  link ck username "course"

nCreateGroupProfessor :: Username -> GroupKey -> IO (Erroneous ())
nCreateGroupProfessor u gk = runAtomically $ do
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

nSaveCourse :: Course -> IO (Erroneous CourseKey)
nSaveCourse c = runAtomically $ do
  dirName <- createTmpDir courseDataDir "cr"
  let courseKey = CourseKey . takeBaseName $ dirName
  save dirName c
  return courseKey

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

{- * One primitve value is stored in the file with the same name as the row.
   * One combined value is stored in the given directory into many files. The name
     of the directory is the primary key for the record.
   * The foreign keys are the symlinks for the other row of the given combined object.
-}
nSaveGroup :: CourseKey -> Group -> IO (Erroneous GroupKey)
nSaveGroup ck group = runAtomically $ do
  dirName <- createTmpDir groupDataDir "gr"
  let groupKey = GroupKey . takeBaseName $ dirName
  save dirName group
  link groupKey ck "groups"
  link ck groupKey "course"
  return groupKey

nSaveExercise :: Assignment -> IO (Erroneous AssignmentKey)
nSaveExercise = runAtomically . tSaveAssignment

tSaveAssignment :: Assignment -> TIO AssignmentKey
tSaveAssignment a = do
  dirName <- createTmpDir assignmentDataDir "a"
  let assignmentKey = takeBaseName dirName
  save dirName a
  return . AssignmentKey $ assignmentKey
  
selectValidDirsFrom :: FilePath -> (FilePath -> TIO Bool) -> TIO [FilePath]
selectValidDirsFrom dir isValidDir = getSubDirectories dir >>= filterM isValidDir

nExerciseKeys :: IO (Erroneous [AssignmentKey])
nExerciseKeys = runAtomically $
  (selectValidDirsFrom assignmentDataDir isExerciseDir) >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (AssignmentKey . takeBaseName)

nFilterExercises :: (AssignmentKey -> Assignment -> Bool) -> IO (Erroneous [(AssignmentKey, Assignment)])
nFilterExercises f = runAtomically $
  (selectValidDirsFrom assignmentDataDir isExerciseDir) >>=
  (mapM tLoadExercise)             >>=
  (return . filter (uncurry f))

isExerciseDir :: FilePath -> TIO Bool
isExerciseDir f = hasNoRollback $ do
  d <- doesDirectoryExist f
  e <- doesFileExist $ joinPath [f,"description"]
  t <- doesFileExist $ joinPath [f,"testcases"]
  return $ and [d,e,t]

nLoadExercise :: AssignmentKey -> IO (Erroneous Assignment)
nLoadExercise e = runAtomically $ do
  let p = exerciseDirPath e
  isEx <- isExerciseDir p
  case isEx of
    False -> throwEx $ userError $ join [str e, " exercise does not exist."]
    True  -> liftM snd $ tLoadExercise p
  where
    exerciseDirPath :: AssignmentKey -> FilePath
    exerciseDirPath (AssignmentKey e) = joinPath [assignmentDataDir, e]

tLoadExercise :: FilePath -> TIO (AssignmentKey, Assignment)
tLoadExercise dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (AssignmentKey exerciseKey, e)

saveAndLinkAssignment :: (ForeignKey k) => k -> Assignment -> IO (Erroneous AssignmentKey)
saveAndLinkAssignment k a = runAtomically $ do
  ak <- tSaveAssignment a
  link ak k "assignments"
  return ak

nSaveCourseAssignment :: CourseKey -> Assignment -> IO (Erroneous AssignmentKey)
nSaveCourseAssignment = saveAndLinkAssignment

nSaveGroupAssignment :: GroupKey  -> Assignment -> IO (Erroneous AssignmentKey)
nSaveGroupAssignment = saveAndLinkAssignment

isAssignmentDir :: FilePath -> TIO Bool
isAssignmentDir = isCorrectDirStructure assignmentDirStructure

assignmentsFor :: (a -> FilePath) -> a -> IO (Erroneous [AssignmentKey])
assignmentsFor dirPath k = runAtomically $ do
  fp <- (selectValidDirsFrom (joinPath [dirPath k, "assignments"]) isAssignmentDir)
  return ((AssignmentKey . takeBaseName) <$> fp)

nCourseAssignments :: CourseKey -> IO (Erroneous [AssignmentKey])
nCourseAssignments = assignmentsFor courseDirPath

nGroupAssignments :: GroupKey -> IO (Erroneous [AssignmentKey])
nGroupAssignments = assignmentsFor groupDirPath

nCourseKeys :: IO (Erroneous [CourseKey])
nCourseKeys = runAtomically $
  (selectValidDirsFrom courseDataDir isCourseDir) >>=
  calcCourseKeys
    where
      calcCourseKeys = return . map (CourseKey . takeBaseName)

isCourseDir :: FilePath -> TIO Bool
isCourseDir = isCorrectDirStructure courseDirStructure

nFilterCourses :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
nFilterCourses f = runAtomically $
  (selectValidDirsFrom courseDataDir isCourseDir) >>=
  (mapM tLoadCourse)             >>=
  (return . filter (uncurry f))

-- * Tools

nError :: String -> Erroneous a
nError = Left

encodePwd :: String -> String
encodePwd = ordEncode

reason :: Either IOException a -> Either String a
reason (Left e)  = Left $ show e
reason (Right x) = Right x

-- | Run a TIO transaction and convert the exception to a String message
runAtomically :: TIO a -> IO (Erroneous a)
runAtomically = liftM reason . atomically
