module Bead.Persistence.NoSQLDir (
    noSqlDirPersist
  ) where

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import Bead.Persistence.NoSQL.Loader

import Control.Monad (join, mapM)
import System.FilePath (joinPath)
import System.Directory (doesDirectoryExist, createDirectory)

-- | Simple directory and file based NoSQL persistence implementation
noSqlDirPersist = Persist {
    saveUser      = nSaveUser      -- :: User -> Password -> IO (Erroneous ())
  , doesUserExist = nDoesUserExist -- :: Username -> Password -> IO Bool
  , personalInfo  = nPersonalInfo  -- :: Username -> Password -> IO (Erroneous (Role, String))
  , updatePwd     = nUpdatePwd     -- :: Username -> Password -> Password -> IO (Erroneous ())

  , saveCourse    = nSaveCourse    -- :: Course -> IO (Erroneous ())

  , saveGroup     = nSaveGroup     -- :: CourseKey -> Group -> IO (Erroneous GroupKey)
  
  , isPersistenceSetUp = nIsPersistenceSetUp
  , initPersistence    = nInitPersistence
  }

nIsPersistenceSetUp :: IO Bool
nIsPersistenceSetUp = do
  dirsExist <- mapM doesDirectoryExist persistenceDirs
  return $ and dirsExist

nInitPersistence :: IO ()
nInitPersistence = mapM_ createDirectory persistenceDirs

nSaveUser :: User -> Password -> IO (Erroneous ())
nSaveUser usr pwd = do
  userExist <- isThereAUser (u_username usr)
  case userExist of
    True -> return $ Left $ "User already exists: " ++ show (u_username usr)
    False -> do
      let ePwd = encodePwd pwd
          dirname = dirName usr
      createDirectory dirname
      e1 <- save dirname (u_username usr)
      e2 <- save dirname (u_role usr)
      e3 <- save dirname (u_email usr)
      e4 <- saveName dirname (u_name usr)
      e5 <- savePwd  dirname ePwd
      return $ firstError [e1,e2,e3,e4,e5]

isThereAUser :: Username -> IO Bool
isThereAUser uname = do
   let dirname = dirName uname
   exist <- doesDirectoryExist dirname
   case exist of
     False -> return False
     True  -> isCorrectStructure dirname usersStructure
      
nDoesUserExist :: Username -> Password -> IO Bool
nDoesUserExist uname pwd = do
  let dirname = dirName uname
      ePwd = encodePwd pwd
  exist <- doesDirectoryExist $ dirname
  case exist of
    False -> return False
    True -> do
      ex <- loadPwd dirname
      case ex of
        Left m -> return False
        Right ePwd' -> do
          putStrLn ePwd
          putStrLn ePwd'
          return (ePwd == ePwd')

nPersonalInfo :: Username -> Password -> IO (Erroneous (Role, String))
nPersonalInfo uname pwd = do
  userExist <- isThereAUser uname
  case userExist of
    False -> return $ Left $ "User doesn't already exist: " ++ show uname
    True -> do
      let ePwd = encodePwd pwd
          dirname = dirName uname
      rl <- load dirname
      fn <- loadName dirname
      case (rl,fn) of
        (Right rl', Right fn') -> return $ Right (rl',fn')
        _ -> return $ unsafeFirstError [forgetVal rl, forgetVal fn]

nUpdatePwd :: Username -> Password -> Password -> IO (Erroneous ())
nUpdatePwd uname oldPwd newPwd = do
  userExist <- nDoesUserExist uname oldPwd
  case userExist of
    False -> return $ Left $ "Invalid user and/or password combination: " ++ show uname
    True -> do
      let ePwd = encodePwd oldPwd
          dirname = dirName uname
      oldEPwd <- loadPwd dirname
      case oldEPwd of
        Left e -> return $ Left e
        Right oldEPwd' -> do
          case ePwd == oldEPwd' of
            False -> return $ Left $ "Invalid password"
            True -> do
              savePwd dirname (encodePwd newPwd)
              return $ Right ()

nSaveCourse :: Course -> IO (Erroneous CourseKey)
nSaveCourse c = do
  let courseDir = dirName c
      courseKey = keyString c
  -- TODO: Check if file exists with a same name
  exist <- doesDirectoryExist courseDir
  case exist of
    -- ERROR: Course already exists on the disk
    True -> return $ nError $ join [
                "Course already exist: "
              , courseName c
              , " (", show $ courseCode c, ")"
              ]
    -- New course
    False -> do
      -- TODO: Check errors creating dirs and files
      -- No space left, etc...
      createDirectory courseDir
      createDirectory $ joinPath [courseDir, "groups"]
      createDirectory $ joinPath [courseDir, "exams"]
      createDirectory $ joinPath [courseDir, "exams", "groups"]
      e <- saveCourseDesc courseDir
      return $ Right $ CourseKey courseKey
  where
    saveCourseDesc :: FilePath -> IO (Erroneous ())
    saveCourseDesc courseDir = do
      e0 <- save courseDir (courseCode c)
      e1 <- saveDesc courseDir (courseDesc c)
      e2 <- saveName courseDir (courseName c)
      return $ firstError [e0,e1,e2]

registerInGroup :: Username -> GroupKey -> IO (Erroneous ())
registerInGroup uname gk = do
  let userDir = dirName uname
  exist <- doesDirectoryExist userDir
  case exist of
    False -> return $ nError $ join [show uname, " does not exist."]
    True -> saveString userDir (fileName gk) "Registered"

nSaveGroup :: CourseKey -> Group -> IO (Erroneous GroupKey)
nSaveGroup ck g = do
  let courseDir   = dirName ck
      groupKeyStr = keyString g
      groupDir    = joinPath [courseDir, "groups", groupKeyStr]
      groupKey    = GroupKey ck groupKeyStr
  exist <- doesDirectoryExist groupDir
  case exist of
    True -> return $ nError $ join ["Group ",groupName g," is already stored"]
    False -> do
      createDirectory groupDir
      e <- saveGroupDesc groupDir groupKey
      return $ Right groupKey
  where
    saveGroupDesc :: FilePath -> GroupKey -> IO (Erroneous ())
    saveGroupDesc groupDir gk = do
      e0 <- saveName groupDir (groupName g)
      e1 <- saveDesc groupDir (groupDesc g)
      e2 <- saveString groupDir "users" $ unlines $ map str $ groupUsers g
      es <- mapM (flip registerInGroup gk) $ groupUsers g
      return $ firstError ([e0,e1,e2] ++ es)

-- * Tools

nError :: String -> Erroneous a
nError = Left

encodePwd :: String -> String
encodePwd = ordEncode

saveName dirName = saveString dirName "name"
loadName dirName = loadString dirName "name"

saveDesc dirName = saveString dirName "description"
loadDesc dirName = loadString dirName "description"

savePwd dirName = saveString dirName "password"
loadPwd dirName = loadString dirName "password"
