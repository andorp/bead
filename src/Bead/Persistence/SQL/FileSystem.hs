module Bead.Persistence.SQL.FileSystem where

import           Control.Applicative ((<$>))
import           Control.DeepSeq (deepseq)
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (isSuffixOf)
import           Data.Maybe
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Directory
import qualified System.Directory as Dir
import           System.FilePath
import           System.IO
import           System.Posix.Types (COff(..))
import           System.Posix.Files (getFileStatus, fileSize, modificationTime)

import           Bead.Domain.Entities
import           Bead.Domain.Relationships
import           Bead.Domain.Types

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

createDirectoryLocked :: FilePath -> (FilePath -> IO ()) -> IO ()
createDirectoryLocked d m = do
  let d' = d <.> "locked"
  createDirectory d'
  m d'
  renameDirectory d' d

createUserFileDir :: (MonadIO io) => Username -> io ()
createUserFileDir u = liftIO $
  forM_ ["private-files", "public-files" ] $ \d -> do
    let dir = dirName u </> d
    exists <- doesDirectoryExist dir
    unless exists $ createDirectoryIfMissing True dir

copyUsersFile :: (MonadIO io) => Username -> FilePath -> UsersFile -> io ()
copyUsersFile username tmpPath userfile = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
      publicDir  = dirname </> "public-files"
      privateDir = dirname </> "private-files"
  Dir.copyFile tmpPath $ usersFile (publicDir </>) (privateDir </>) userfile

-- Calculates the file modification time in UTC time from the File status
fileModificationInUTCTime = posixSecondsToUTCTime . realToFrac . modificationTime

listFiles :: (MonadIO io) => Username -> io [(UsersFile, FileInfo)]
listFiles username = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
  privateFiles <- f (dirname </> "private-files") UsersPrivateFile
  publicFiles  <- f (dirname </> "public-files") UsersPublicFile
  return $ privateFiles ++ publicFiles
  where
    f dir typ = do
      paths <- getFilesInFolder dir
      forM paths $ \path -> do
        status <- getFileStatus path
        let info = FileInfo
                   (fileOffsetToInt $ fileSize status)
                   (fileModificationInUTCTime status)
        return (typ $ takeFileName path, info)

    fileOffsetToInt (COff x) = fromIntegral x

getFile :: (MonadIO io) => Username -> UsersFile -> io FilePath
getFile username userfile = liftIO $ do
  createUserFileDir username
  let dirname = dirName username
      publicDir  = dirname </> "public-files"
      privateDir = dirname </> "private-files"
  usersFile (f publicDir) (f privateDir) userfile
  where
    f dir fn = do
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
  createDirectoryLocked tjPath $ \p -> do
    fileSave (p </> "script") (tsScript testScript)
    -- Save Simple or Zipped Submission
    withSubmissionValue (solution submission) (flip fileSave) (flip fileSaveBS) (p </> "submission")
    -- Save Simple or Zipped Test Case
    withTestCaseValue (tcValue testCase) (flip fileSave) (flip fileSaveBS) (p </> "tests")

-- Insert the feedback info for the file system part of the database. This method is
-- used by the tests only, and serves as a model for interfacing with the outside world.
insertTestFeedback :: (MonadIO io) => SubmissionKey -> FeedbackInfo -> io ()
insertTestFeedback sk info = liftIO $ do
  let sDir = submissionKeyMap (testIncomingDataDir </>) sk <.> "locked"
  createDirectoryIfMissing True sDir
  let student comment = fileSave (sDir </> "public") comment
      admin   comment = fileSave (sDir </> "private") comment
      result  bool    = fileSave (sDir </> "result") (show bool)
  feedbackInfo result student admin evaluated info
  where
    evaluated _ _ = error "insertTestComment: Evaluation should not be inserted by test."

finalizeTestFeedback :: (MonadIO io) => SubmissionKey -> io ()
finalizeTestFeedback sk = liftIO $ do
  let sDir = submissionKeyMap (testIncomingDataDir </>) sk
  renameDirectory (sDir <.> "locked") sDir

-- Test Feedbacks are stored in the persistence layer, in the test-incomming directory
-- each one in a file, named after an existing submission in the system
testFeedbacks :: (MonadIO io) => io [(SubmissionKey, Feedback)]
testFeedbacks = liftIO (createFeedbacks =<< processables)
  where
    processables = filter (not . (`isSuffixOf` ".locked")) <$>
      getSubDirectories testIncomingDataDir

    createFeedbacks = fmap join . mapM createFeedback

    createFeedback path = do
      let sk = SubmissionKey . last $ splitDirectories path
          addKey x = (sk, x)

      files <- getFilesInFolder path
      fmap (map addKey . catMaybes) $
        forM files $ \file -> do
          fileDate <- fileModificationInUTCTime <$> getFileStatus file

          let (_dir,fname) = splitFileName file
              feedback f = Feedback f fileDate

          case fname of
            "private" -> Just . feedback . MessageForAdmin <$> fileLoad file
            "public"  -> Just . feedback . MessageForStudent <$> fileLoad file
            "result"  -> Just . feedback . TestResult . readMaybeErr file <$> fileLoad file
            _         -> return Nothing

      where
        readMaybeErr :: (Read a) => FilePath -> String -> a
        readMaybeErr fp = fromMaybe (error $ "Non-parseable data in file: " ++ fp) . readMaybe

-- Deletes the comments (test-agent and message as well)
-- contained file from the test-incomming directory, named after
-- an existing submission
deleteTestFeedbacks :: (MonadIO io) => SubmissionKey -> io ()
deleteTestFeedbacks = liftIO .
  submissionKeyMap (removeDirectoryRecursive . (testIncomingDataDir </>))


-- Removes the file if exists, otherwise do nothing
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

