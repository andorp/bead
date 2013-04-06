module Bead.Persistence.NoSQL.Loader where

import Bead.Domain.Types hiding (FileName(..), fileName)
import qualified Bead.Domain.Types as T (FileName(..), fileName)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Invariants (UnitTests(..), InvariantsM2(..))
import Control.Monad.Transaction.TIO

import Control.DeepSeq (deepseq)
import Control.Monad (liftM, filterM)

import Data.Char (ord)
import Data.Maybe (maybe)
import Data.Time (UTCTime)
import Data.List (nub)
import System.FilePath (joinPath)
import System.IO
import System.IO.Temp (createTempDirectory, openTempFile)
import System.Directory
import System.Posix.Files (createSymbolicLink, removeLink)
import Control.Exception as E
import Control.Monad (join, when)

type DirPath = FilePath

-- * Type classes

dataDir = "data"
userDir = "user"
courseDir   = "course"
assignmentDir = "assignment"
groupDir    = "group"
submissionDir = "submission"

courseDataDir   = joinPath [dataDir, courseDir]
userDataDir     = joinPath [dataDir, userDir]
groupDataDir    = joinPath [dataDir, groupDir]
assignmentDataDir = joinPath [dataDir, assignmentDir]
submissionDataDir = joinPath [dataDir, submissionDir]

persistenceDirs :: [FilePath]
persistenceDirs = [
    dataDir
  , userDataDir
  , courseDataDir
  , assignmentDataDir
  , groupDataDir
  , submissionDataDir
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
  dirName u = joinPath [dataDir, userDir, ordEncode $ str u]

instance DirName User where
  dirName = dirName . u_username

instance KeyString CourseKey where
  keyString (CourseKey k) = k

instance DirName AssignmentKey where
  dirName (AssignmentKey c) = joinPath [assignmentDataDir, c]

instance DirName GroupKey where
  dirName (GroupKey g) = joinPath [assignmentDataDir, g]

-- * Load and save aux functions

fileSave :: DirPath -> FilePath -> String -> TIO ()
fileSave d f s = step
    (do let fname = joinPath [d,f]
        handler <- openFile fname WriteMode
        hPutStr handler s
        hClose handler
        return ())
    (do let fname = joinPath [d,f]
        exists <- doesFileExist fname
        when exists $ removeFile fname)

fileLoad :: DirPath -> FilePath -> (String -> a) -> TIO a
fileLoad d f l = step
    (do let fname = joinPath [d,f]
        exist <- doesFileExist fname
        h <- openFile fname ReadMode
        s <- hGetContents h
        s `deepseq` hClose h
        return . l $ s)
    (return ())

fileUpdate :: DirPath -> FilePath -> String -> TIO ()
fileUpdate d f c = do
  stepM action before after
  return ()
    where
      before = return ()

      action = do
        let fname = joinPath [d,f]
        h <- openFile fname ReadMode
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

createTmpDir :: FilePath -> String -> TIO FilePath
createTmpDir f t = stepM (createTempDirectory f t) (return ()) removeDirectory

createLink :: FilePath -> FilePath -> TIO ()
createLink exist link = step (createSymbolicLink exist link) (removeLink link)

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
loadString d f = fileLoad d f id

updateString :: DirPath -> FilePath -> String -> TIO ()
updateString d f c = fileUpdate d f c

-- * Save instances

instance Save Role where
  save d r = fileSave d "role" (show r)

instance Save Username where
  save d (Username s) = fileSave d "username" s

instance Save Email where
  save d (Email e) = fileSave d "email" e

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

instance Save Course where
  save d c = do createStructureDirs d courseDirStructure
                saveDesc d (courseDesc c)
                saveName d (courseName c)

instance Save Group where
  save d g = do createStructureDirs d groupDirStructure
                saveDesc d (groupDesc g)
                saveName d (groupName g)

instance Save User where
  save d u = do createStructureDirs d userDirStructure
                save     d (u_role u)
                save     d (u_username u)
                save     d (u_email u)
                saveName d (u_name  u)

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" (maybe (error "Role parsing was failed") id . parseRole)

instance Load Username where
  load d = fileLoad d "username" username

instance Load Email where
  load d = fileLoad d "email" email'

instance Load Assignment where
  load d = do
    name <- loadName d
    desc <- fileLoad d "description" id
    tcs  <- fileLoad d "testcases" id
    t    <- fileLoad d "type"  read
    s    <- fileLoad d "start" read
    e    <- fileLoad d "end"   read
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
    s <- fileLoad d "solution" id
    p <- fileLoad d "date" read
    return $ Submission {
        solution = s
      , solutionPostDate = p
      }

instance Load Course where
  load d = do desc <- loadDesc d
              name <- loadName d
              return $ Course {
                  courseDesc = desc
                , courseName = name
                }

instance Load Group where
  load d = do desc <- loadDesc d
              name <- loadName d
              return $ Group {
                  groupDesc = desc
                , groupName = name
                }

instance Load User where
  load d = do
    role  <- load d
    uname <- load d
    email <- load d
    name  <- loadName d
    return $ User {
        u_role = role
      , u_username = uname
      , u_email = email
      , u_name = name
      }

-- * Update instances

instance Update Role where
  update d r = fileUpdate d "role" (show r)

instance Update Username where
  update d (Username s) = fileUpdate d "username" s

instance Update Email where
  update d (Email e) = fileUpdate d "email" e

instance Update User where
  update d u = do
    update d (u_username u)
    update d (u_role     u)
    update d (u_email    u)
    updateName d (u_name u)

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

userDirStructure = DirStructure {
    files       = ["email", "name", "password", "role", "username"]
  , directories = ["course", "group" ,"courseadmin" ,"groupadmin", "submissions"]
  }

assignmentDirStructure = DirStructure {
    files       = ["name", "description", "testcases", "type", "start", "end"]
  , directories = ["group", "course"]
  }

submissionDirStructure = DirStructure {
    files = ["solution", "date"]
  , directories = ["exercise", "user"]
  }

courseDirStructure = DirStructure {
    files       = ["description", "name"]
  , directories = ["groups", "assignments", "users", "admins"]
  }

groupDirStructure = DirStructure {
    files       = ["description", "name"]
  , directories = ["users", "course", "admins", "assignments"]
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

