module Bead.Persistence.SQL.FileSystem where

import           Control.Applicative ((<$>))
import           Control.DeepSeq (deepseq)
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Directory
import qualified System.Directory as Dir
import           System.FilePath
import           System.IO
import           System.Posix.Types (COff(..))
import           System.Posix.Files (getFileStatus, fileSize, modificationTime)

import Bead.Domain.Entities
import Bead.Domain.Relationships


datadir = "data"

testOutgoingDir = "test-outgoing"
testIncomingDir = "test-incoming"
userDir = "user"

testOutgoingDataDir = joinPath [datadir, testOutgoingDir]
testIncomingDataDir = joinPath [datadir, testIncomingDir]
userDataDir = joinPath [datadir, userDir]

fsDirs = [
    datadir
  , testOutgoingDataDir
  , testIncomingDataDir
  , userDataDir
  ]

class DirName d where
  dirName :: d -> FilePath

instance DirName Username where
  dirName = usernameCata ((datadir </> "user") </>)

instance DirName TestJobKey where
  dirName (TestJobKey k) = joinPath [datadir, testOutgoingDir, k]

fileLoad :: (MonadIO io) => FilePath -> io String
fileLoad fname = liftIO $ do
  h <- openFile fname ReadMode
  hSetEncoding h utf8
  s <- hGetContents h
  s `deepseq` hClose h
  return s

fileSave :: (MonadIO io) => FilePath -> String -> io ()
fileSave fname s = liftIO $ do
  handler <- openFile fname WriteMode
  hSetEncoding handler utf8
  hPutStr handler s
  hClose handler

fileSaveBS :: (MonadIO io) => FilePath -> ByteString -> io ()
fileSaveBS fname s = liftIO $ do
  handler <- openFile fname WriteMode
  hSetEncoding handler utf8
  BS.hPutStr handler s
  hClose handler

filterDirContents :: (MonadIO io) => (FilePath -> IO Bool) -> FilePath -> io [FilePath]
filterDirContents f p = liftIO $ do
  content <- liftM (filter (\d -> not $ or [d == ".", d == ".."])) $ getDirectoryContents p
  filterM f $ map jp content
    where
      jp x = joinPath [p, x]

getSubDirectories :: (MonadIO io) => FilePath -> io [FilePath]
getSubDirectories = filterDirContents doesDirectoryExist

getFilesInFolder :: (MonadIO io) => FilePath -> io [FilePath]
getFilesInFolder = filterDirContents doesFileExist

-- *

initFS :: (MonadIO io) => io ()
initFS = liftIO $ mapM_ createDirWhenDoesNotExist fsDirs
  where
    createDirWhenDoesNotExist d = do
      existDir <- doesDirectoryExist d
      unless existDir . createDirectory $ d

removeFS :: (MonadIO io) => io ()
removeFS = liftIO $ removeDirectoryRecursive datadir

isSetUpFS :: (MonadIO io) => io Bool
isSetUpFS = liftIO . fmap and $ mapM doesDirectoryExist fsDirs

createUserFileDir :: (MonadIO io) => Username -> io ()
createUserFileDir u = liftIO $ do
  let dir = dirName u </> "datadir"
  exists <- doesDirectoryExist dir
  unless exists $ createDirectoryIfMissing True dir

copyUsersFile :: (MonadIO io) => Username -> FilePath -> UsersFile -> io ()
copyUsersFile username tmpPath userfile = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
      dir = dirname </> "datadir"
  Dir.copyFile tmpPath (usersFileCata (dir </>) userfile)

-- Calculates the file modification time in UTC time from the File status
fileModificationInUTCTime = posixSecondsToUTCTime . realToFrac . modificationTime

listFiles :: (MonadIO io) => Username -> io [(UsersFile, FileInfo)]
listFiles username = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
      dir = dirname </> "datadir"
  paths <- getFilesInFolder dir
  forM paths $ \path -> do
    status <- getFileStatus path
    let info = FileInfo
                 (fileOffsetToInt $ fileSize status)
                 (fileModificationInUTCTime status)
    return (UsersFile $ takeFileName path, info)
  where
    fileOffsetToInt (COff x) = fromIntegral x

getFile :: (MonadIO io) => Username -> UsersFile -> io FilePath
getFile username userfile = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
      dir = dirname </> "datadir"
  flip usersFileCata userfile $ \fn -> do
    let fname = dir </> fn
    exists <- doesFileExist fname
    unless exists $ error $ concat [
        "File (", fn, ") does not exist in users folder ("
      , show username, ")"
      ]
    return fname

-- Collects the test script, test case and the submission and copies them to the
-- the directory named after the submission key placed in the test-outgoing directory
saveTestJob :: (MonadIO io) => SubmissionKey -> Submission -> TestScript -> TestCase -> io ()
saveTestJob sk submission testScript testCase = liftIO $ do
  -- If there is a test case, we copy the information to the desired
  let tjk = submissionKeyToTestJobKey sk
      tjPath = dirName tjk
  exists <- doesDirectoryExist tjPath
  when exists $ error $ concat ["Test job directory already exist:", show tjk]
  createDirectory tjPath
  fileSave (tjPath </> "submission") (solution submission)
  fileSave (tjPath </> "script") (tsScript testScript)
  fileSaveBS (tjPath </> "tests") (tcValue testCase)

-- Inserts a test comment for the incoming test comment directory,
-- this function is mainly for testing of this functionality
insertTestComment :: (MonadIO io) => SubmissionKey -> String -> io ()
insertTestComment sk msg = fileSave (submissionKeyMap (testIncomingDataDir </>) sk) msg

-- Test Comments are stored in the persistence layer, in the test-incomming directory
-- each one in a file, named after an existing submission in the system
testComments :: (MonadIO io) => io [(SubmissionKey, Comment)]
testComments = liftIO $ getFilesInFolder testIncomingDataDir >>= createComments
  where
    testCommentType fname =
      case splitExtension fname of
        (_name, ".message") -> CT_Message
        _                   -> CT_TestAgent

    submissionKey fname =
      let (name,_ext) = splitExtension fname
      in SubmissionKey name

    createComments = mapM createComment
    createComment fp = do
      let (dir,fname) = splitFileName fp
      comment <- commentAna (fileLoad (dir </> fname))
                            (return "Testing")
                            (fileModificationInUTCTime <$> (getFileStatus fp))
                            (return $ testCommentType fname)
      return (submissionKey fname, comment)

-- Deletes the comments (test-agent and message as well)
-- contained file from the test-incomming directory, named after
-- an existing submission
deleteTestComment :: (MonadIO io) => SubmissionKey -> io ()
deleteTestComment s = liftIO $ withSubmissionKey s $ \sk -> do
  removeFileIfExists $ testIncomingDataDir </> sk
  removeFileIfExists $ testIncomingDataDir </> concat [sk, ".message"]

-- Removes the file if exists, otherwise do nothing
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

