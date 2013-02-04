module Bead.Persistence.NoSQL.Loader where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Data.Char (ord)
import System.FilePath (joinPath)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception as E
import Control.Monad (join)

type DirPath = FilePath

-- * Type classes

persistenceDirs :: [FilePath]
persistenceDirs = [
    "data"
  , joinPath ["data", "user"]
  , joinPath ["data", "course"]
  ]

class DirName d where
  dirName :: d -> DirPath

class FileName f where
  fileName :: f -> String
  
class KeyString k where
  keyString :: k -> String
  
class Load l where
  load   :: DirPath -> IO (Erroneous l)

class Save s where
  save :: DirPath -> s -> IO (Erroneous ())

-- * DirName and KeyString instances

instance DirName Username where
  dirName u = joinPath ["data", "user", ordEncode $ str u]

instance DirName User where
  dirName = dirName . u_username

instance KeyString Course where
  keyString = ordEncode . str . courseCode

instance KeyString CourseKey where
  keyString (CourseKey k) = k
  
instance DirName Course where
  dirName c = joinPath ["data", "course", keyString c]

instance DirName CourseKey where
  dirName (CourseKey c) = joinPath ["data", "course", c]

instance KeyString Group where
  keyString = ordEncode . groupCode
  
instance DirName GroupKey where
  dirName (GroupKey ck g) = joinPath ["data", "course", g]
  
instance FileName GroupKey where
  fileName (GroupKey ck g) = join ["group-", keyString ck, "-", g]
  
-- * Load and save aux functions
  
fileSave :: DirPath -> FilePath -> String -> IO (Erroneous ())
fileSave d f s = 
  catchSE
    (do let fname = joinPath [d,f]
        handler <- openFile fname WriteMode
        hPutStr handler s
        hClose handler
        return $ Right ())
    (\ex -> return $ Left $ "Exception occured: " ++ show ex)

fileLoad :: DirPath -> FilePath -> (String -> a) -> IO (Erroneous a)
fileLoad d f l =
  catchSE
    (do let fname = joinPath [d,f]
        exist <- doesFileExist fname
        case exist of
          False -> return $ Left $ "No role file exist"
          True -> do
            catchSE
              (readFile fname >>= return . Right . l)
              (\innerEx -> return $ Left $ "Exception occured: " ++ show innerEx))
    (\outerEx -> return $ Left $ "Exception occured: " ++ show outerEx)

saveString :: DirPath -> FilePath -> String -> IO (Erroneous ())
saveString = fileSave

loadString :: DirPath -> FilePath -> IO (Erroneous String)
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

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" read

instance Load Username where
  load d = fileLoad d "username" username

instance Load Email where
  load d = fileLoad d "email" email'

instance Load CourseCode where
  load d = fileLoad d "course_code" CourseCode

-- * Dir Structures

newtype DirStructure = DirStructure [FilePath]

isCorrectStructure :: DirPath -> DirStructure -> IO Bool
isCorrectStructure dirname (DirStructure fs) = do
  as <- mapM (doesFileExist . joinPath . f) fs
  return $ and as
  where
    f x = [dirname, x]

usersStructure = DirStructure [
    "email", "name", "password", "role", "username"
  ]

-- * Encoding

ordEncode :: String -> String
ordEncode txt = concatMap code txt
  where
    code :: Char -> String
    code = show . ord

-- * Tool

catchSE :: IO a -> (SomeException -> IO a) -> IO a
catchSE = E.catch

