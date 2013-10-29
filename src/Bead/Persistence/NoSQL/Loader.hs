{-# LANGUAGE CPP #-}
module Bead.Persistence.NoSQL.Loader where

import Bead.Domain.Types hiding (FileName(..), fileName)
import qualified Bead.Domain.Types as T (FileName(..), fileName)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Control.Monad.Transaction.TIO

import Control.DeepSeq (deepseq)
import Control.Monad (liftM, filterM, unless)

import Data.Char (ord)
import Data.Maybe (maybe)
import Data.Time (UTCTime)
import Data.List (nub)
import System.FilePath (joinPath)
import System.IO
import System.IO.Temp (createTempDirectory, openTempFile)
import System.Directory
import System.Posix.Files (createSymbolicLink, removeLink, readSymbolicLink, fileExist)
import Control.Exception as E
import Control.Monad (join, when)

#ifdef TEST
import Bead.Invariants (UnitTests(..), InvariantsM2(..))
#endif

type DirPath = FilePath

-- * Type classes

dataDir = "data"
userDir = "user"
courseDir   = "course"
assignmentDir = "assignment"
groupDir    = "group"
submissionDir = "submission"
evaulationDir = "evaulation"
commentDir    = "comment"
openSubmissionDir = "open-submission"
userRegDir = "user-registration"

courseDataDir   = joinPath [dataDir, courseDir]
userDataDir     = joinPath [dataDir, userDir]
groupDataDir    = joinPath [dataDir, groupDir]
assignmentDataDir = joinPath [dataDir, assignmentDir]
submissionDataDir = joinPath [dataDir, submissionDir]
evaulationDataDir = joinPath [dataDir, evaulationDir]
commentDataDir    = joinPath [dataDir, commentDir]
openSubmissionDataDir = joinPath [dataDir, openSubmissionDir]
userRegDataDir = joinPath [dataDir, userRegDir]

persistenceDirs :: [FilePath]
persistenceDirs = [
    dataDir
  , userDataDir
  , courseDataDir
  , assignmentDataDir
  , groupDataDir
  , submissionDataDir
  , evaulationDataDir
  , commentDataDir
  , openSubmissionDataDir
  , userRegDataDir
  ]

class DirName d where
  dirName :: d -> DirPath

class FileName f where
  fileName :: f -> String

class KeyString k where
  keyString :: k -> String

-- | Loading data from file persistence
class Load l where
  load   :: DirPath -> TIO l

-- | Saving data to file persistence
class Save s where
  save :: DirPath -> s -> TIO ()

class Update s where
  update :: DirPath -> s -> TIO ()

-- * DirName and KeyString instances

instance DirName Username where
  dirName u = joinPath [dataDir, userDir, str u]

instance DirName User where
  dirName = dirName . u_username

instance KeyString CourseKey where
  keyString (CourseKey k) = k

instance DirName AssignmentKey where
  dirName (AssignmentKey c) = joinPath [assignmentDataDir, c]

instance DirName GroupKey where
  dirName (GroupKey g) = joinPath [groupDataDir, g]

instance DirName CourseKey where
  dirName (CourseKey c) = joinPath [courseDataDir, c]

instance DirName SubmissionKey where
  dirName (SubmissionKey sk) = joinPath [submissionDataDir, sk]

instance DirName EvaulationKey where
  dirName (EvaulationKey ek) = joinPath [evaulationDataDir, ek]

instance DirName CommentKey where
  dirName (CommentKey ck) = joinPath [commentDataDir, ck]

-- * Load and save aux functions

fileSave :: DirPath -> FilePath -> String -> TIO ()
fileSave d f s = step
    (do let fname = joinPath [d,f]
        handler <- openFile fname WriteMode
        hSetEncoding handler utf8
        hPutStr handler s
        hClose handler
        return ())
    (do let fname = joinPath [d,f]
        exists <- doesFileExist fname
        when exists $ removeFile fname)

fileLoad :: DirPath -> FilePath -> (String -> Maybe a) -> TIO a
fileLoad d f reader = step
    (do let fname = joinPath [d,f]
        exist <- doesFileExist fname
        h <- openFile fname ReadMode
        hSetEncoding h utf8
        s <- hGetContents h
        s `deepseq` hClose h
        case reader s of
          Nothing -> throwIO . userError . join $
            ["Non parseable data in file: ", fname]
          Just x  -> return x)
    (return ())

same :: a -> Maybe a
same = Just

fileUpdate :: DirPath -> FilePath -> String -> TIO ()
fileUpdate d f c = do
  stepM action before after
  return ()
    where
      before = return ()

      action = do
        let fname = joinPath [d,f]
        h <- openFile fname ReadMode
        hSetEncoding h utf8
        s <- hGetContents h
        s `deepseq` hClose h
        writeFile fname c
        return s

      after original = do
        let fname = joinPath [d,f]
        writeFile fname original
        return ()

-- * Directories

createDir :: FilePath -> TIO ()
createDir d = step (createDirectory d) (removeDirectory d)

createDirIfMissing :: FilePath -> TIO ()
createDirIfMissing d = step (createDirectoryIfMissing True d) (removeDirectory d)

createTmpDir :: FilePath -> String -> TIO FilePath
createTmpDir f t = stepM (createTempDirectory f t) (return ()) removeDirectory

createLink :: FilePath -> FilePath -> TIO ()
createLink exist link = step (createSymbolicLinkSafely exist link) (removeLink link)

-- | Create a symbolic link 'l' if the link does not exist already.
createSymbolicLinkSafely f l = do
  exist <- fileExist l
  unless exist $ createSymbolicLink f l

removeSymLink :: FilePath -> TIO ()
removeSymLink link = do
  stepM (do f <- readSymbolicLink link
            removeLink link
            return f)
        (return ())
        (\f -> createSymbolicLinkSafely f link >> return ())
  return ()

removeDir :: FilePath -> TIO ()
removeDir d = step (removeDirectory d) (createDirectory d)

getDirContents :: FilePath -> TIO [FilePath]
getDirContents f = hasNoRollback (getDirectoryContents f)

filterDirContents :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
filterDirContents f p = do
  content <- liftM (filter (\d -> not $ or [d == ".", d == ".."])) $ getDirectoryContents p
  filterM f $ map jp content
    where
      jp x = joinPath [p, x]

getSubDirectories :: FilePath -> TIO [FilePath]
getSubDirectories = hasNoRollback . filterDirContents doesDirectoryExist

getFilesInFolder :: FilePath -> TIO [FilePath]
getFilesInFolder = hasNoRollback . filterDirContents doesFileExist

doesDirExist :: FilePath -> TIO Bool
doesDirExist = hasNoRollback . doesDirectoryExist

openTmpFile :: FilePath -> String -> TIO (FilePath, Handle)
openTmpFile f t = stepM
  (openTempFile f t)
  (return ())
  (\(p,h) -> do
    hClose h
    removeFile p)


saveString :: DirPath -> FilePath -> String -> TIO ()
saveString = fileSave

loadString :: DirPath -> FilePath -> TIO String
loadString d f = fileLoad d f same

updateString :: DirPath -> FilePath -> String -> TIO ()
updateString d f c = fileUpdate d f c

-- * Save instances

instance Save Role where
  save d r = fileSave d "role" (show r)

instance Save Username where
  save d (Username s) = fileSave d "username" s

instance Save Email where
  save d (Email e) = fileSave d "email" e

instance Save TimeZone where
  save d = fileSave d "timezone" . show

instance Save Assignment where
  save d e = do
    createStructureDirs d assignmentDirStructure
    saveName d (assignmentName e)
    fileSave d "description" (assignmentDesc e)
    fileSave d "testcases"   (assignmentTCs  e)
    fileSave d "type"        (show . assignmentType $ e)
    fileSave d "start"       (show . assignmentStart $ e)
    fileSave d "end"         (show . assignmentEnd $ e)

instance Save Submission where
  save d s = do
    createStructureDirs d submissionDirStructure
    fileSave d "solution" (solution s)
    fileSave d "date"     (show . solutionPostDate $ s)

instance Save Evaulation where
  save d e = do
    createStructureDirs d evaulationDirStructure
    fileSave d "evaulation" (writtenEvaulation e)
    fileSave d "result" (show . evaulationResult $ e)

instance Save Comment where
  save d c = do
    createStructureDirs d commentDirStructure
    fileSave d "comment" (comment c)
    fileSave d "date" (show . commentDate $ c)

instance Save Course where
  save d c = do createStructureDirs d courseDirStructure
                saveDesc d (courseDesc c)
                saveName d (courseName c)
                fileSave d "evalcfg" (show . courseEvalConfig $ c)

instance Save Group where
  save d g = do createStructureDirs d groupDirStructure
                saveDesc d (groupDesc g)
                saveName d (groupName g)
                fileSave d "evalcfg" (show . groupEvalConfig $ g)

instance Save User where
  save d = userCata $ \role username email name timezone -> do
    createStructureDirs d userDirStructure
    save d role
    save d username
    save d email
    saveName d name
    save d timezone

instance Save UserRegistration where
  save d u = do createStructureDirs d userRegDirStructure
                fileSave d "userreg" (show u)

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" (maybe (error "Role parsing was failed") same . parseRole)

instance Load Username where
  load d = fileLoad d "username" (same . username)

instance Load Email where
  load d = fileLoad d "email" (same . email')

instance Load TimeZone where
  load d = fileLoad d "timezone" readMaybe

instance Load Assignment where
  load d = do
    name <- loadName d
    desc <- fileLoad d "description" same
    tcs  <- fileLoad d "testcases" same
    t    <- fileLoad d "type"  readMaybe
    s    <- fileLoad d "start" readMaybe
    e    <- fileLoad d "end"   readMaybe
    return $ Assignment {
        assignmentName = name
      , assignmentDesc = desc
      , assignmentTCs  = tcs
      , assignmentType = t
      , assignmentStart = s
      , assignmentEnd   = e
      }

instance Load Submission where
  load d = do
    s <- fileLoad d "solution" same
    p <- fileLoad d "date" readMaybe
    return $ Submission {
        solution = s
      , solutionPostDate = p
      }

instance Load Evaulation where
  load d = do
    e <- fileLoad d "evaulation" same
    r <- fileLoad d "result" readMaybe
    return Evaulation {
      writtenEvaulation = e
    , evaulationResult  = r
    }

instance Load Comment where
  load d = do
    c <- fileLoad d "comment" same
    e <- fileLoad d "date" readMaybe
    return Comment {
      comment     = c
    , commentDate = e
    }

instance Load Course where
  load d = do desc <- loadDesc d
              name <- loadName d
              eval <- fileLoad d "evalcfg" readMaybe
              return $ Course {
                  courseDesc = desc
                , courseName = name
                , courseEvalConfig = eval
                }

instance Load Group where
  load d = do desc <- loadDesc d
              name <- loadName d
              eval <- fileLoad d "evalcfg" readMaybe
              return $ Group {
                  groupDesc = desc
                , groupName = name
                , groupEvalConfig = eval
                }

instance Load User where
  load d = do
    role  <- load d
    uname <- load d
    email <- load d
    name  <- loadName d
    zone  <- load d
    return $ User {
        u_role = role
      , u_username = uname
      , u_email = email
      , u_name = name
      , u_timezone = zone
      }

instance Load UserRegistration where
  load d = fileLoad d "userreg" readMaybe

-- * Update instances

instance Update TimeZone where
  update d = fileUpdate d "timezone" . show

instance Update Role where
  update d r = fileUpdate d "role" (show r)

instance Update Username where
  update d (Username s) = fileUpdate d "username" s

instance Update Email where
  update d (Email e) = fileUpdate d "email" e

instance Update User where
  update d = userCata $ \username role email name timezone -> do
    update d username
    update d role
    update d email
    updateName d name
    update d timezone

instance Update Evaulation where
  update d e = do
    fileUpdate d "evaulation" (writtenEvaulation e)
    fileUpdate d "result"      (show . evaulationResult $ e)

instance Update Assignment where
  update d a = do
    updateName d (assignmentName a)
    fileUpdate d "type"  (show . assignmentType  $ a)
    fileUpdate d "start" (show . assignmentStart $ a)
    fileUpdate d "end"   (show . assignmentEnd   $ a)
    fileUpdate d "description" (assignmentDesc a)
    fileUpdate d "testcases"   (assignmentTCs  a)

-- * Dir Structures

data DirStructure = DirStructure {
    directories :: [FilePath]
  , files       :: [FilePath]
  }

isCorrectStructure :: DirPath -> DirStructure -> IO Bool
isCorrectStructure dirname ds = do
  d  <- doesDirectoryExist dirname
  as <- mapM (doesDirectoryExist . joinPath . f) . directories $ ds
  bs <- mapM (doesFileExist      . joinPath . f) . files       $ ds
  return . and $ as ++ bs
  where
    f x = [dirname, x]

createStructureDirs :: DirPath -> DirStructure -> TIO ()
createStructureDirs p = mapM_ (\x -> createDir (joinPath [p,x])) . directories

created = "created"

saveCreatedTime :: DirPath -> UTCTime -> TIO ()
saveCreatedTime d = fileSave d "created" . show

getCreatedTime :: DirPath -> TIO UTCTime
getCreatedTime d = fileLoad d "created" readMaybe

userDirStructure = DirStructure {
    files       = ["email", "name", "role", "username"]
  , directories = ["course", "group" ,"courseadmin" ,"groupadmin", "submissions"]
  }

assignmentDirStructure = DirStructure {
    files = [ "name", "description", "testcases"
            , "type", "start", "end", created ]
  , directories = ["group", "course", "submission"]
  }

submissionDirStructure = DirStructure {
    files = ["solution", "date"]
  , directories = ["assignment", "user", "evaulation", "comment"]
  }

courseDirStructure = DirStructure {
    files       = ["description", "name", "evalcfg"]
  , directories = ["groups", "assignments", "users", "admins"]
  }

groupDirStructure = DirStructure {
    files       = ["description", "name", "evalcfg"]
  , directories = ["users", "course", "admins", "assignments"]
  }

evaulationDirStructure = DirStructure {
    files       = ["result", "evaulation"]
  , directories = ["submission"]
  }

commentDirStructure = DirStructure {
    files = ["comment", "date"]
  , directories = ["submission"]
  }

userRegDirStructure = DirStructure {
    files = ["userreg"]
  , directories = []
  }

-- * Encoding

ordEncode :: String -> String
ordEncode txt = concatMap code txt
  where
    code :: Char -> String
    code = show . ord

saveName d = saveString d "name"
loadName d = loadString d "name"
updateName d = updateString d "name"

saveDesc d = saveString d "description"
loadDesc d = loadString d "description"

savePwd d = saveString d "password"
loadPwd d = loadString d "password"

#ifdef TEST

-- * Invariants

isValidDirStructure :: DirStructure -> Bool
isValidDirStructure s =
  let names = join [files s, directories s]
  in and $ join [
         (map (not . null) names)
       , [(length (nub names) == length names)]
       ]

dirStructures = [
    submissionDirStructure
  , courseDirStructure
  , groupDirStructure
  , userDirStructure
  , assignmentDirStructure
  , evaulationDirStructure
  , commentDirStructure
  , userRegDirStructure
  ]

unitTests = UnitTests [
    ("Dir structures must has different and not empty names", and . map isValidDirStructure $ dirStructures)
  ]

invariants :: (Eq a, Load a, Save a) => InvariantsM2 IO T.FileName a
invariants = InvariantsM2 [
    ("Load and save must be transparent", \f x -> (runBoolTransaction (loadAndSave f x)))
  ] where
      runBoolTransaction :: TIO Bool -> IO Bool
      runBoolTransaction t = do
        b <- atomically t
        return $ case b of
          Left _   -> False
          Right b' -> b'

      loadAndSave :: (Eq a, Load a, Save a) => T.FileName -> a -> TIO Bool
      loadAndSave d x = do
        let d' = T.fileName d
        save d' x
        y <- load d'
        hasNoRollback $ removeDirectory d'
        return (x==y)

{- TODO:
  * All save instances must save the directory structure correctly
  * All save and load instances must be identical relation
-}
#endif
