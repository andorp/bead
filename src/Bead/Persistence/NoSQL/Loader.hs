module Bead.Persistence.NoSQL.Loader where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Control.Monad.Transaction.TIO

import Control.Monad (liftM, filterM)

import Data.Char (ord)
import System.FilePath (joinPath)
import System.IO
import System.IO.Temp (createTempDirectory, openTempFile)
import System.Directory
import System.Posix.Files (createSymbolicLink, removeLink)
import Control.Exception as E
import Control.Monad (join, when)
import Control.Applicative ((<$>))

type DirPath = FilePath

-- * Type classes

dataDir = "data"
userDir = "user"
courseDir   = "course"
exerciseDir = "exercise"
groupDir    = "group"

exerciseDataDir = joinPath [dataDir, exerciseDir]
courseDataDir   = joinPath [dataDir, courseDir]
userDataDir     = joinPath [dataDir, userDir]
groupDataDir    = joinPath [dataDir, groupDir]

persistenceDirs :: [FilePath]
persistenceDirs = [
    dataDir
  , userDataDir
  , courseDataDir
  , exerciseDataDir
  , groupDataDir
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

-- * DirName and KeyString instances

instance DirName Username where
  dirName u = joinPath [dataDir, userDir, ordEncode $ str u]

instance DirName User where
  dirName = dirName . u_username

instance KeyString Course where
  keyString = ordEncode . str . courseCode

instance KeyString CourseKey where
  keyString (CourseKey k) = k

instance DirName Course where
  dirName c = joinPath [dataDir, courseDir, keyString c]

instance DirName CourseKey where
  dirName (CourseKey c) = joinPath [dataDir, courseDir, c]

instance DirName ExerciseKey where
  dirName (ExerciseKey c) = joinPath [dataDir, exerciseDir, c]

instance DirName GroupKey where
  dirName (GroupKey g) = joinPath [exerciseDataDir, g]

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
        l <$> readFile fname)
    (return ())

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

-- * Save instances

instance Save Role where
  save d r = fileSave d "role" (show r)

instance Save Username where
  save d (Username s) = fileSave d "username" s

instance Save Email where
  save d (Email e) = fileSave d "email" e

instance Save CourseCode where
  save d (CourseCode s) = fileSave d "course_code" s

instance Save GroupCode where
  save d (GroupCode g) = fileSave d "group_code" g

instance Save Exercise where
  save d e = fileSave d "exercise" (exercise e)

instance Save Course where
  save d c = do createStructureDirs d courseDirStructure
                save     d (courseCode c)
                saveDesc d (courseDesc c)
                saveName d (courseName c)

instance Save Group where
  save d g = do createStructureDirs d groupDirStructure
                save     d (groupCode g)
                saveDesc d (groupDesc g)
                saveName d (groupName g)

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" read

instance Load Username where
  load d = fileLoad d "username" username

instance Load Email where
  load d = fileLoad d "email" email'

instance Load CourseCode where
  load d = fileLoad d "course_code" CourseCode

instance Load GroupCode where
  load d = fileLoad d "group_code" GroupCode

instance Load Exercise where
  load d = fileLoad d "exercise" Exercise

instance Load Course where
  load d = do code <- load     d
              desc <- loadDesc d
              name <- loadName d
              return $ Course {
                  courseCode = code
                , courseDesc = desc
                , courseName = name
                }

instance Load Group where
  load d = do code <- load     d
              desc <- loadDesc d
              name <- loadName d
              return $ Group {
                  groupCode = code
                , groupDesc = desc
                , groupName = name
                }

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

usersStructure = DirStructure {
    files       = ["email", "name", "password", "role", "username"]
  , directories = []
  }

exerciseDirStructure = DirStructure {
    files       = ["exercise"]
  , directories = []
  }

courseDirStructure = DirStructure {
    files       = ["course_code", "description", "name"]
  , directories = ["groups", "exams"]
  }

groupDirStructure = DirStructure {
    files       = ["group_code", "description", "name"]
  , directories = ["users"]
  }

-- * Encoding

ordEncode :: String -> String
ordEncode txt = concatMap code txt
  where
    code :: Char -> String
    code = show . ord

saveName d = saveString d "name"
loadName d = loadString d "name"

saveDesc d = saveString d "description"
loadDesc d = loadString d "description"

savePwd d = saveString d "password"
loadPwd d = loadString d "password"

