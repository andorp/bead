module Bead.Persistence.NoSQLDir (
    noSqlDirPersist
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.NoSQL.Loader
import Control.Monad.Transaction.TIO

import Control.Monad (join, mapM, liftM, filterM, when, unless)
import Control.Exception (IOException, throwIO)
import System.FilePath (joinPath, takeBaseName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)

-- | Simple directory and file based NoSQL persistence implementation
noSqlDirPersist = Persist {
    saveUser      = nSaveUser      -- :: User -> Password -> IO (Erroneous ())
  , doesUserExist = nDoesUserExist -- :: Username -> Password -> IO (Erroneous Bool)
  , personalInfo  = nPersonalInfo  -- :: Username -> Password -> IO (Erroneous (Role, String))
  , updatePwd     = nUpdatePwd     -- :: Username -> Password -> Password -> IO (Erroneous ())
  , filterUsers   = nFilterUsers   -- :: (User -> Bool) -> IO (Erroneous [User])
  , loadUser      = nLoadUser      -- :: Username -> IO (Erroneous User)
  , updateUser    = nUpdateUser    -- :: Username -> User -> IO (Erroneous ())

  , saveCourse    = nSaveCourse    -- :: Course -> IO (Erroneous ())
  , courseKeys    = nCourseKeys    -- :: IO (Erroneous [CourseKey])
  , filterCourses = nFilterCourses -- :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
  , loadCourse    = nLoadCourse    -- :: CourseKey -> IO (Erroneous Course)
  , groupKeysOfCourse = nGroupKeysOfCourse -- :: CourseKey -> IO (Erroneous [GroupKey])
  , isUserInCourse = nIsUserInCourse -- :: Username -> CourseKey -> IO (Erroneous Bool)

  , saveGroup     = nSaveGroup     -- :: CourseKey -> Group -> IO (Erroneous GroupKey)
  , loadGroup     = nLoadGroup     -- :: GroupKey -> IO (Erroneous Group)
  , isUserInGroup = nIsUserInGroup -- :: Username -> GroupKey -> IO (Erroneous Bool)
  , subscribe     = nSubscribe     -- :: Username -> CourseKey -> GroupKey -> IO (Erroneous ())

  , filterExercises = nFilterExercises -- :: (ExerciseKey -> Exercise -> Bool) -> IO (Erroneous [(ExerciseKey,Exercise)])
  , exerciseKeys  = nExerciseKeys  -- :: IO (Erroneous [ExerciseKey])
  , saveExercise  = nSaveExercise  -- :: Exercise -> IO (Erroneous ExerciseKey)
  , loadExercise    = nLoadExercise  -- :: ExerciseKey -> IO (Erroneous Exercise)

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

nDoesUserExist :: Username -> Password -> IO (Erroneous Bool)
nDoesUserExist u p = runAtomically $ tDoesUserExist u p

tDoesUserExist :: Username -> Password -> TIO Bool
tDoesUserExist uname pwd = do
  let dirname = dirName uname
      ePwd = encodePwd pwd
  exists <- hasNoRollback . doesDirectoryExist $ dirname
  case exists of
    False -> return False
    True  -> do
      ePwd' <- loadPwd dirname
      return (ePwd == ePwd')

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

nLoadUser :: Username -> IO (Erroneous User)
nLoadUser = runAtomically . load . dirName

nUpdateUser :: User -> IO (Erroneous ())
nUpdateUser user = runAtomically $ update (dirName . u_username $ user) user

nUpdatePwd :: Username -> Password -> Password -> IO (Erroneous ())
nUpdatePwd uname oldPwd newPwd = runAtomically $ do
  userExist <- tDoesUserExist uname oldPwd
  case userExist of
    False -> throwEx $ userError $ "Invalid user and/or password combination: " ++ show uname
    True -> do
      let ePwd = encodePwd oldPwd
          dirname = dirName uname
      oldEPwd <- loadPwd dirname
      case ePwd == oldEPwd of
        False -> throwEx . userError $ "Invalid password"
        True  -> savePwd dirname $ encodePwd newPwd

courseDirPath :: CourseKey -> FilePath
courseDirPath (CourseKey c) = joinPath [courseDataDir, c]

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

userIsLinkedInDir username key = runAtomically $
  hasNoRollback . doesDirectoryExist . joinPath $
    [referredPath key, "users", baseName username]

nIsUserInGroup :: Username -> GroupKey -> IO (Erroneous Bool)
nIsUserInGroup = userIsLinkedInDir

nIsUserInCourse :: Username -> CourseKey -> IO (Erroneous Bool)
nIsUserInCourse = userIsLinkedInDir

nSubscribe :: Username -> CourseKey -> GroupKey -> IO (Erroneous ())
nSubscribe username ck gk = runAtomically $ do
  let pg = joinPath [referredPath gk, "users"]
      pc = joinPath [referredPath ck, "users"]
      b = baseName username
  existsInGroup  <- hasNoRollback . doesDirectoryExist . joinPath $ [pg, b]
  existsInCourse <- hasNoRollback . doesDirectoryExist . joinPath $ [pc, b]
  unless existsInGroup  $ foreignKey username gk "users"
  unless existsInCourse $ foreignKey username ck "users"

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
  let courseDir = dirName c
      courseKey = keyString c
  exist <- hasNoRollback $ doesDirectoryExist courseDir
  -- GUARD: Course already exists on the disk
  when exist . throwEx . userError . join $ [
      "Course already exist: "
    , courseName c
    , " (", show $ courseCode c, ")"
    ]

  -- New course
  createDir courseDir
  save courseDir c
  return . CourseKey $ courseKey

class ForeignKey k where
  referredPath :: k -> DirPath
  baseName     :: k -> String

foreignKey :: (ForeignKey k1, ForeignKey k2) => k1 -> k2 -> FilePath -> TIO ()
foreignKey object linkto subdir =
  createLink
    (joinPath ["..", "..", "..", "..", (referredPath object)])
    (joinPath [(referredPath linkto), subdir, baseName object])

instance ForeignKey Username where
  referredPath = dirName
  baseName     = takeBaseName . dirName

instance ForeignKey GroupKey where
  referredPath (GroupKey g) = joinPath [groupDataDir, g]
  baseName     (GroupKey g) = g

instance ForeignKey CourseKey where
  referredPath (CourseKey c) = joinPath [courseDataDir, c]
  baseName     (CourseKey c) = c

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
  foreignKey groupKey ck "groups"
  foreignKey ck groupKey "course"
  return groupKey

nSaveExercise :: Exercise -> IO (Erroneous ExerciseKey)
nSaveExercise exercise = runAtomically $ do
  dirName <- createTmpDir exerciseDataDir "ex"
  let exerciseKey = takeBaseName dirName
  save dirName exercise
  return . ExerciseKey $ exerciseKey

selectValidDirsFrom :: FilePath -> (FilePath -> TIO Bool) -> TIO [FilePath]
selectValidDirsFrom dir isValidDir = getSubDirectories dir >>= filterM isValidDir

nExerciseKeys :: IO (Erroneous [ExerciseKey])
nExerciseKeys = runAtomically $
  (selectValidDirsFrom exerciseDataDir isExerciseDir) >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (ExerciseKey . takeBaseName)

nFilterExercises :: (ExerciseKey -> Exercise -> Bool) -> IO (Erroneous [(ExerciseKey, Exercise)])
nFilterExercises f = runAtomically $
  (selectValidDirsFrom exerciseDataDir isExerciseDir) >>=
  (mapM tLoadExercise)             >>=
  (return . filter (uncurry f))

isExerciseDir :: FilePath -> TIO Bool
isExerciseDir f = hasNoRollback $ do
  d <- doesDirectoryExist f
  e <- doesFileExist $ joinPath [f,"exercise"]
  return $ and [d,e]

nLoadExercise :: ExerciseKey -> IO (Erroneous Exercise)
nLoadExercise e = runAtomically $ do
  let p = exerciseDirPath e
  isEx <- isExerciseDir p
  case isEx of
    False -> throwEx $ userError $ join [str e, " exercise does not exist."]
    True  -> liftM snd $ tLoadExercise p
  where
    exerciseDirPath :: ExerciseKey -> FilePath
    exerciseDirPath (ExerciseKey e) = joinPath [exerciseDataDir, e]

tLoadExercise :: FilePath -> TIO (ExerciseKey, Exercise)
tLoadExercise dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (ExerciseKey exerciseKey, e)

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
