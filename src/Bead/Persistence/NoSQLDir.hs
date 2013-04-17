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
  , userDescription = nUserDescription
  , userSubmissions = nUserSubmissions
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
  , courseAdmins = nCourseAdmins

  , saveGroup     = nSaveGroup
  , loadGroup     = nLoadGroup
  , courseOfGroup = nCourseOfGroup
  , filterGroups  = nFilterGroups
  , isUserInGroup = nIsUserInGroup
  , userGroups    = nUserGroups
  , subscribe     = nSubscribe
  , groupAdmins   = nGroupAdmins
  , createGroupProfessor = nCreateGroupProfessor

  , filterAssignment     = nFilterAssignment
  , assignmentKeys       = nAssignmentKeys
  , saveAssignment       = nSaveAssignment
  , loadAssignment       = nLoadAssignment
  , saveCourseAssignment = nSaveCourseAssignment
  , saveGroupAssignment  = nSaveGroupAssignment
  , courseAssignments    = nCourseAssignments
  , groupAssignments     = nGroupAssignments
  , courseOfAssignment   = nCourseOfAssignment
  , groupOfAssignment    = nGroupOfAssignment

  , saveSubmission = nSaveSubmission
  , loadSubmission = nLoadSubmission
  , assignmentOfSubmission = nAssignmentOfSubmission
  , usernameOfSubmission   = nUsernameOfSubmission
  , filterSubmissions      = nFilterSubmissions
  , evaulationOfSubmission = nEvaulationOfSubmission
  , commentsOfSubmission   = nCommentsOfSubmission

  , placeToOpened     = nPlaceToOpened
  , removeFromOpened  = nRemoveFromOpened
  , openedSubmissions = nOpenedSubmission

  , saveEvaulation = nSaveEvaulation
  , loadEvaulation = nLoadEvaulation
  , modifyEvaulation = nModifyEvaulation
  , submissionOfEvaulation = nSubmissionOfEvaulation

  , saveComment = nSaveComment
  , loadComment = nLoadComment
  , submissionOfComment = nSubmissionOfComment

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
nCanUserLogin uname pwd = do
  exists <- nDoesUserExist uname
  case exists of
    False -> return False
    True  -> do
      ePwd <- loadPwd . dirName $ uname
      return (ePwd == (encodePwd pwd))

nDoesUserExist :: Username -> TIO Bool
nDoesUserExist = hasNoRollback . doesDirectoryExist . dirName

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
nFilterUsers f = filterDirectory userDataDir isUserDir load (filter f)

nLoadUser :: Username -> TIO User
nLoadUser = load . dirName

nUserDescription :: Username -> TIO UserDesc
nUserDescription = liftM mkUserDescription . nLoadUser

nUpdateUser :: User -> TIO ()
nUpdateUser user = update (dirName . u_username $ user) user

nUpdatePwd :: Username -> Password -> Password -> TIO ()
nUpdatePwd uname oldPwd newPwd = do
  userExist <- nCanUserLogin uname oldPwd
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
  usr <- nLoadUser u
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

admins :: (DirName k) => k -> TIO [Username]
admins k = do
  let dirname = joinPath [dirName k, "admins"]
  mapM (liftM u_username . load) =<< (selectValidDirsFrom dirname isUserDir)

nGroupAdmins :: GroupKey -> TIO [Username]
nGroupAdmins = admins

nCourseAdmins :: CourseKey -> TIO [Username]
nCourseAdmins = admins

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

nFilterGroups :: (GroupKey -> Group -> Bool) -> TIO [(GroupKey, Group)]
nFilterGroups f = filterDirectory groupDataDir isGroupDir tLoadGroup (filter (uncurry f))

nSaveAssignment :: Assignment -> TIO AssignmentKey
nSaveAssignment a = do
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
nFilterAssignment f = filterDirectory assignmentDataDir isAssignmentDir tLoadAssignment (filter (uncurry f))

nLoadAssignment :: AssignmentKey -> TIO Assignment
nLoadAssignment a = do
  let p = assignmentDirPath a
  isEx <- isAssignmentDir p
  case isEx of
    False -> throwEx $ userError $ join [str a, " assignment does not exist."]
    True  -> liftM snd $ tLoadAssignment p
  where
    assignmentDirPath :: AssignmentKey -> FilePath
    assignmentDirPath (AssignmentKey e) = joinPath [assignmentDataDir, e]

tLoadAssignment :: FilePath -> TIO (AssignmentKey, Assignment)
tLoadAssignment dirName = do
  let exerciseKey = takeBaseName dirName
  e <- load dirName
  return (AssignmentKey exerciseKey, e)

objectIn
  :: (Show sk, DirName sk)
  => FilePath               -- Subdir where to look at
  -> (String -> rk)         -- Result's key constructor
  -> (FilePath -> TIO Bool) -- Checks if the given subdir is appropiate
  -> sk                     -- The source's key
  -> TIO (Maybe rk)         -- Returns (Just rk) if only one key was found in the given dictionary,
                            --   otherwise Nothing
objectIn subdir keyConstructor isValidDir sourceKey = do
  let dirname = joinPath [dirName sourceKey, subdir]
  ks <- map (keyConstructor . takeBaseName) <$> (selectValidDirsFrom dirname isValidDir)
  return $ case ks of
    []  -> Nothing
    [k] -> Just k
    _   -> error $ "Impossible: found more than one object found for: " ++ show sourceKey

objectIn' msg subdir keyConstructor isValidDir sourceKey = do
  m <- objectIn subdir keyConstructor isValidDir sourceKey
  case m of
    Nothing -> error $ msg ++ show sourceKey
    Just  x -> return x

nCourseOfAssignment :: AssignmentKey -> TIO (Maybe CourseKey)
nCourseOfAssignment = objectIn "course" CourseKey isCourseDir

nGroupOfAssignment :: AssignmentKey -> TIO (Maybe GroupKey)
nGroupOfAssignment = objectIn  "group" GroupKey isGroupDir

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
  linkUserSubmission submissionKey
  nPlaceToOpened submissionKey
  return submissionKey
    where
      linkUserSubmission :: SubmissionKey -> TIO ()
      linkUserSubmission sk = do
        let dirName = joinPath [referredPath u, "submissions", baseName ak]
        createDirIfMissing dirName
        createLink
          (joinPath ["..","..","..","..","..",(referredPath sk)])
          (joinPath [dirName, (baseName sk)])

nLoadSubmission :: SubmissionKey -> TIO Submission
nLoadSubmission = load . dirName

nAssignmentOfSubmission :: SubmissionKey -> TIO AssignmentKey
nAssignmentOfSubmission sk =
  objectIn' "No assignment was found for the " "assignment" AssignmentKey isAssignmentDir sk

nUsernameOfSubmission :: SubmissionKey -> TIO Username
nUsernameOfSubmission sk =
  objectIn' "No assignment was found for the " "user" Username isUserDir sk

nEvaulationOfSubmission :: SubmissionKey -> TIO (Maybe EvaulationKey)
nEvaulationOfSubmission =
  objectIn "evaulation" EvaulationKey isEvaulationDir

nFilterSubmissions :: (SubmissionKey -> Submission -> Bool) -> TIO [(SubmissionKey, Submission)]
nFilterSubmissions f = filterDirectory submissionDataDir isSubmissionDir tLoadSubmission (filter (uncurry f))

tLoadSubmission :: FilePath -> TIO (SubmissionKey, Submission)
tLoadSubmission dirName = do
  s <- load dirName
  return (SubmissionKey . takeBaseName $ dirName, s)

nPlaceToOpened :: SubmissionKey -> TIO ()
nPlaceToOpened sk = do
  createLink
    (joinPath ["..", "..", (referredPath sk)])
    (joinPath [openSubmissionDataDir, baseName sk])

nRemoveFromOpened :: SubmissionKey -> TIO ()
nRemoveFromOpened sk = removeSymLink (joinPath [openSubmissionDataDir, baseName sk])

filterDirectory :: FilePath -> (FilePath -> TIO Bool) -> (FilePath -> TIO a) -> ([a] -> [b]) -> TIO [b]
filterDirectory dir isValid loader f = f <$> ((selectValidDirsFrom dir isValid) >>= (mapM loader))

nOpenedSubmission :: TIO [SubmissionKey]
nOpenedSubmission = filterDirectory openSubmissionDataDir isSubmissionDir tLoadSubmission (map fst)

userDirPath :: Username -> FilePath
userDirPath (Username u) = joinPath [userDataDir, u]

submissionDirPath :: SubmissionKey -> FilePath
submissionDirPath (SubmissionKey sk) = joinPath [submissionDataDir, sk]

nUserSubmissions :: Username -> AssignmentKey -> TIO [SubmissionKey]
nUserSubmissions u ak =
  filterDirectory
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

-- * Evaulation

instance ForeignKey EvaulationKey where
  referredPath (EvaulationKey e) = joinPath [evaulationDataDir, e]
  baseName     (EvaulationKey e) = e

isEvaulationDir :: FilePath -> TIO Bool
isEvaulationDir = isCorrectDirStructure evaulationDirStructure

nSaveEvaulation :: SubmissionKey -> Evaulation -> TIO EvaulationKey
nSaveEvaulation sk e = do
  dirName <- createTmpDir evaulationDataDir "ev"
  let evKey = EvaulationKey . takeBaseName $ dirName
  save dirName e
  link evKey sk "evaulation"
  link sk evKey "submission"
  return evKey

evaulationDirPath :: EvaulationKey -> FilePath
evaulationDirPath (EvaulationKey e) = joinPath [evaulationDataDir, e]

nLoadEvaulation :: EvaulationKey -> TIO Evaulation
nLoadEvaulation e = do
  let p = evaulationDirPath e
  isE <- isEvaulationDir p
  unless isE . throwEx . userError . join $ ["Evaulation does not exist."]
  liftM snd . tLoadPersistenceObject EvaulationKey $ p

nModifyEvaulation :: EvaulationKey -> Evaulation -> TIO ()
nModifyEvaulation ek e = do
  let p = evaulationDirPath ek
  isE <- isEvaulationDir p
  unless isE . throwEx . userError . join $ ["Evaulation does not exist."]
  update p e

nSubmissionOfEvaulation :: EvaulationKey -> TIO SubmissionKey
nSubmissionOfEvaulation =
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
  unless isC . throwEx . userError . join $ ["Comment does not exist."]
  liftM snd . tLoadPersistenceObject CommentKey $ p

nSubmissionOfComment :: CommentKey -> TIO SubmissionKey
nSubmissionOfComment =
  objectIn' "No evaulation was found for " "submission" SubmissionKey isSubmissionDir

-- * Tools

encodePwd :: String -> String
encodePwd = ordEncode
