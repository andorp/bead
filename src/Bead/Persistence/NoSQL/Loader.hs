module Bead.Persistence.NoSQL.Loader where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Control.Monad.Transaction.TIO

import Data.Char (ord)
import System.FilePath (joinPath)
import System.IO
import System.IO.Temp (createTempDirectory, openTempFile)
import System.Directory
import Control.Exception as E
import Control.Monad (join, when)
import Control.Applicative ((<$>))

type DirPath = FilePath

-- * Type classes

dataDir = "data"
userDir = "user"
courseDir   = "course"
exerciseDir = "exercise"

dataExerciseDir = joinPath [dataDir, exerciseDir]
dataCourseDir   = joinPath [dataDir, courseDir]
dataUserDir     = joinPath [dataDir, userDir]

persistenceDirs :: [FilePath]
persistenceDirs = [
    dataDir
  , dataUserDir
  , dataCourseDir
  , dataExerciseDir
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

instance KeyString Group where
  keyString = ordEncode . groupCode

instance DirName GroupKey where
  dirName (GroupKey ck g) = joinPath [dataDir, courseDir, g]

instance FileName GroupKey where
  fileName (GroupKey ck g) = join ["group-", keyString ck, "-", g]

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

removeDir :: FilePath -> TIO ()
removeDir d = step (removeDirectory d) (createDirectory d)

getDirContents :: FilePath -> TIO [FilePath]
getDirContents f = hasNoRollback (getDirectoryContents f)

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

instance Save Exercise where
  save d e = fileSave d "exercise" (exercise e)

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" read

instance Load Username where
  load d = fileLoad d "username" username

instance Load Email where
  load d = fileLoad d "email" email'

instance Load CourseCode where
  load d = fileLoad d "course_code" CourseCode

instance Load Exercise where
  load d = fileLoad d "exercise" Exercise

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

-- * Encoding

ordEncode :: String -> String
ordEncode txt = concatMap code txt
  where
    code :: Char -> String
    code = show . ord


