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

  , saveCourse    = nSaveCourse    -- :: Course -> IO (Erroneous ())
  , courseKeys    = nCourseKeys    -- :: IO (Erroneous [CourseKey])
  , filterCourses = nFilterCourses -- :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
  , loadCourse    = nLoadCourse    -- :: CourseKey -> IO (Erroneous Course)

  , saveGroup     = nSaveGroup     -- :: CourseKey -> Group -> IO (Erroneous GroupKey)

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
      save     dirname (u_username usr)
      save     dirname (u_role     usr)
      save     dirname (u_email    usr)
      saveName dirname (u_name     usr)
      savePwd  dirname (          ePwd)

isThereAUser :: Username -> TIO Bool
isThereAUser uname = hasNoRollback $ do
  let dirname = dirName uname
  exist <- doesDirectoryExist dirname
  case exist of
    False -> return False
    True  -> isCorrectStructure dirname usersStructure

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


nLoadCourse :: CourseKey -> IO (Erroneous Course)
nLoadCourse c = runAtomically $ do
  let p = courseDirPath c
  isC <- isCourseDir p
  -- GUARD: Course dir does not exist
  unless isC . throwEx . userError . join $ [str c, " course does not exist."]
  -- Course found
  liftM snd $ tLoadCourse p
  where
    courseDirPath :: CourseKey -> FilePath
    courseDirPath (CourseKey e) = joinPath [courseDataDir, e]


tLoadCourse :: FilePath -> TIO (CourseKey, Course)
tLoadCourse d = do
  let courseKey = takeBaseName d
  course <- load d
  return (CourseKey courseKey, course)

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
  return groupKey

nSaveExercise :: Exercise -> IO (Erroneous ExerciseKey)
nSaveExercise exercise = runAtomically $ do
  dirName <- createTmpDir exerciseDataDir "ex"
  let exerciseKey = takeBaseName dirName
  save dirName exercise
  return . ExerciseKey $ exerciseKey

addDataDirPath p =
  return .
    map (\f -> joinPath [p, f]) .
    filter (not . flip elem [".", ".."])

nExerciseKeys :: IO (Erroneous [ExerciseKey])
nExerciseKeys = runAtomically $
  (getDirContents exerciseDataDir) >>=
  (addDataDirPath exerciseDataDir) >>=
  (filterM isExerciseDir)          >>=
  calcExerciseKeys
    where
      calcExerciseKeys = return . map (ExerciseKey . takeBaseName)

nFilterExercises :: (ExerciseKey -> Exercise -> Bool) -> IO (Erroneous [(ExerciseKey, Exercise)])
nFilterExercises f = runAtomically $
  (getDirContents exerciseDataDir) >>=
  (addDataDirPath exerciseDataDir) >>=
  (filterM isExerciseDir)          >>=
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


-- TODO: implement
tLoadExercise :: FilePath -> TIO (ExerciseKey, Exercise)
tLoadExercise dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (ExerciseKey exerciseKey, e)

nCourseKeys :: IO (Erroneous [CourseKey])
nCourseKeys = runAtomically $
  (getDirContents courseDataDir) >>=
  (addDataDirPath courseDataDir) >>=
  (filterM isCourseDir)          >>=
  calcCourseKeys
    where
      calcCourseKeys = return . map (CourseKey . takeBaseName)

isCourseDir :: FilePath -> TIO Bool
isCourseDir p = hasNoRollback $ isCorrectStructure p courseDirStructure

nFilterCourses :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
nFilterCourses f = runAtomically $
  (getDirContents courseDataDir) >>=
  (addDataDirPath courseDataDir) >>=
  (filterM isCourseDir)          >>=
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
