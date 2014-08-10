{-# LANGUAGE CPP #-}
module Bead.Persistence.NoSQLDirFile where

import           Control.DeepSeq (deepseq)
import           Control.Exception as E
import           Control.Monad (filterM, join, liftM, unless, when)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (ord)
import           Data.Time (UTCTime)
import           Data.List (nub)
import           System.Directory
import           System.FilePath (joinPath)
import           System.IO
import           System.IO.Temp (createTempDirectory)
import           System.IO.Unsafe
import           System.Posix.Files (createSymbolicLink, removeLink, readSymbolicLink, fileExist)

import           Text.JSON.Generic

import           Bead.Domain.Entities
import           Bead.Domain.Relationships
import           Bead.Domain.Types hiding (FileName(..), fileName)
import qualified Bead.Domain.Types as T (FileName(..), fileName)
import           Control.Monad.Transaction.TIO


#ifdef TEST
import           Bead.Invariants (UnitTests(..), InvariantsM2(..))
#endif

type DirPath = FilePath

-- * Type classes

dataDir = "data"
userDir = "user"
courseDir   = "course"
assignmentDir = "assignment"
groupDir    = "group"
submissionDir = "submission"
evaluationDir = "evaluation"
commentDir    = "comment"
openSubmissionDir = "open-submission"
userRegDir = "user-registration"
testScriptDir = "test-script"
testCaseDir = "test-case"
testOutgoingDir = "test-outgoing"
testIncomingDir = "test-incoming"
feedbackDir = "feedback"

courseDataDir   = joinPath [dataDir, courseDir]
userDataDir     = joinPath [dataDir, userDir]
groupDataDir    = joinPath [dataDir, groupDir]
assignmentDataDir = joinPath [dataDir, assignmentDir]
submissionDataDir = joinPath [dataDir, submissionDir]
evaluationDataDir = joinPath [dataDir, evaluationDir]
commentDataDir    = joinPath [dataDir, commentDir]
openSubmissionDataDir = joinPath [dataDir, openSubmissionDir]
openSubmissionAllDataDir = joinPath [openSubmissionDataDir, "all"]
userRegDataDir = joinPath [dataDir, userRegDir]
testScriptDataDir = joinPath [dataDir, testScriptDir]
testCaseDataDir = joinPath [dataDir, testCaseDir]
testOutgoingDataDir = joinPath [dataDir, testOutgoingDir]
testIncomingDataDir = joinPath [dataDir, testIncomingDir]
feedbackDataDir = joinPath [dataDir, feedbackDir]

persistenceDirs :: [FilePath]
persistenceDirs = [
    dataDir
  , userDataDir
  , courseDataDir
  , assignmentDataDir
  , groupDataDir
  , submissionDataDir
  , evaluationDataDir
  , commentDataDir
  , openSubmissionDataDir
  , openSubmissionAllDataDir
  , userRegDataDir
  , testScriptDataDir
  , testCaseDataDir
  , testOutgoingDataDir
  , testIncomingDataDir
  , feedbackDataDir
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

instance DirName EvaluationKey where
  dirName (EvaluationKey ek) = joinPath [evaluationDataDir, ek]

instance DirName CommentKey where
  dirName (CommentKey ck) = joinPath [commentDataDir, ck]

instance DirName TestScriptKey where
  dirName (TestScriptKey k) = joinPath [testScriptDataDir, k]

instance DirName TestCaseKey where
  dirName (TestCaseKey k) = joinPath [testCaseDataDir, k]

instance DirName TestJobKey where
  dirName (TestJobKey k) = joinPath [testCaseDataDir, k]

instance DirName FeedbackKey where
  dirName (FeedbackKey k) = joinPath [feedbackDataDir, k]


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

fileSaveBS :: DirPath -> FilePath -> ByteString -> TIO ()
fileSaveBS d f s = step
    (do let fname = joinPath [d,f]
        handler <- openFile fname WriteMode
        hSetEncoding handler utf8
        BS.hPutStr handler s
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

fileLoadBS :: DirPath -> FilePath -> TIO ByteString
fileLoadBS d f = step
    (do let fname = joinPath [d,f]
        exist <- doesFileExist fname
        h <- openFile fname ReadMode
        hSetEncoding h utf8
        s <- BS.hGetContents h
        s `deepseq` hClose h
        return s)
    (return ())

same :: a -> Maybe a
same = Just

writeUtf8File :: FilePath -> String -> IO ()
writeUtf8File fp s = do
  handler <- openFile fp WriteMode
  hSetEncoding handler utf8
  hPutStr handler s
  hClose handler

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
        writeUtf8File fname c
        return s

      after original = do
        let fname = joinPath [d,f]
        writeUtf8File fname original
        return ()

fileUpdateBS :: DirPath -> FilePath -> ByteString -> TIO ()
fileUpdateBS d f c = do
  stepM action before after
  return ()
    where
      before = return ()

      action = do
        let fname = joinPath [d,f]
        h <- openFile fname ReadMode
        hSetEncoding h utf8
        s <- BS.hGetContents h
        s `deepseq` hClose h
        BS.writeFile fname c
        return s

      after original = do
        let fname = joinPath [d,f]
        BS.writeFile fname original
        return ()


-- Tries to overwrite file, if some error happened tries to replace the original
-- content back
overwriteFile :: FilePath -> FilePath -> TIO ()
overwriteFile old new = stepM action reverseBefore reverseAfter >> return () where
  action = do
    let fname = new
    h <- openFile fname ReadMode
    hSetEncoding h utf8
    s <- hGetContents h
    s `deepseq` hClose h
    copyFile old new
    return s

  reverseBefore = return ()

  reverseAfter original = do
    writeUtf8File new original
    return ()

-- Tries to delete a file if the file exist, saving it's content if something goes wrong
fileDelete :: DirPath -> FilePath -> TIO ()
fileDelete d f = stepM action reverseBefore reverseAfter >> return () where
  action =
    do let fname = joinPath [d,f]
       exist <- doesFileExist fname
       case exist of
         False -> return Nothing
         True  -> do h <- openFile fname ReadMode
                     hSetEncoding h utf8
                     s <- hGetContents h
                     s `deepseq` hClose h
                     removeFile fname
                     return $ Just s

  reverseBefore = return ()

  reverseAfter Nothing  = return ()
  reverseAfter (Just s) = do
    let fname = joinPath [d,f]
    handler <- openFile fname WriteMode
    hSetEncoding handler utf8
    hPutStr handler s
    hClose handler
    return ()

-- * Directories

createDir :: FilePath -> TIO ()
createDir d = step (createDirectory d) (removeDirectory d)

createDirIfMissing :: FilePath -> TIO ()
createDirIfMissing d = step (createDirectoryIfMissing True d) (removeDirectory d)

createTmpDir :: FilePath -> String -> TIO FilePath
createTmpDir f t = stepM (createTempDirectory f t) (return ()) removeDirectory

createLink :: FilePath -> FilePath -> TIO ()
createLink exist link = step (createSymbolicLinkSafely exist link) (removeLinkSafely link)

deleteLink :: FilePath -> FilePath -> TIO ()
deleteLink exist link = step (removeLinkSafely link) (createSymbolicLinkSafely exist link)

-- Copy the old file into the new one
copy :: FilePath -> FilePath -> TIO ()
copy old new = step (copyFile old new) (removeFile new)

-- | Create a symbolic link 'l' if the link does not exist already.
createSymbolicLinkSafely f l = do
  exist <- fileExist l
  unless exist $ createSymbolicLink f l

removeLinkSafely l = do
  exist <- fileExist l
  when exist $ removeLink l

removeSymLink :: FilePath -> TIO ()
removeSymLink link = do
  stepM (do f <- readSymbolicLink link
            removeLinkSafely link
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
  save d r = fileSave d "role" (printRole r)

instance Save Username where
  save d (Username s) = fileSave d "username" s

instance Save Email where
  save d (Email e) = fileSave d "email" e

instance Save Language where
  save d = fileSave d "language" . show

instance Save TimeZoneName where
  save d = fileSave d "timezonename" . show

instance Save Assignment where
  save d = assignmentCata $ \name desc type_ start end evtype -> do
    createStructureDirs d assignmentDirStructure
    saveName d name
    fileSave d "description" desc
    fileSave d "type"        (show type_)
    fileSave d "start"       (show start)
    fileSave d "end"         (show end)
    fileSave d "evtype"      (show evtype)

instance Save Submission where
  save d s = do
    createStructureDirs d submissionDirStructure
    fileSave d "solution" (solution s)
    fileSave d "date"     (show . solutionPostDate $ s)

instance Save Evaluation where
  save d e = do
    createStructureDirs d evaluationDirStructure
    fileSave d "evaluation" (writtenEvaluation e)
    fileSave d "result" (show . evaluationResult $ e)

instance Save Comment where
  save d = commentCata $ \comment author date type_ -> do
    createStructureDirs d commentDirStructure
    fileSave d "comment" comment
    fileSave d "author" author
    fileSave d "date" (show date)
    fileSave d "type" (show type_)

instance Save Course where
  save d = courseCata show $ \name desc type_ -> do
    createStructureDirs d courseDirStructure
    saveDesc d desc
    saveName d name
    fileSave d "scripttype" type_

instance Save Group where
  save d g = do createStructureDirs d groupDirStructure
                saveDesc d (groupDesc g)
                saveName d (groupName g)

instance Save User where
  save d = userCata $ \role username email name timezone language -> do
    createStructureDirs d userDirStructure
    save d role
    save d username
    save d email
    saveName d name
    save d timezone
    save d language

instance Save UserRegistration where
  save d u = do createStructureDirs d userRegDirStructure
                fileSave d "userreg" (show u)

instance Save TestScript where
  save d = testScriptCata show $ \name desc notes script type_ -> do
    createStructureDirs d testScriptDirStructure
    saveName d name
    saveDesc d desc
    fileSave d "notes" notes
    fileSave d "script" script
    fileSave d "type" type_

instance Save TestCase where
  save d = testCaseCata show $ \name desc value type_ info -> do
    createStructureDirs d testCaseDirStructure
    saveName d name
    saveDesc d desc
    fileSaveBS d "value" value
    fileSave d "type" type_
    fileSave d "info" info

instance Save Feedback where
  save d = feedback encodeJSON $ \info date -> do
    createStructureDirs d feedbackDirStructure
    fileSave d "info" info
    fileSave d "date" $ show date

-- * Load instances

instance Load Role where
  load d = fileLoad d "role" (maybe (error "Role parsing was failed") same . parseRole)

instance Load Language where
  load d = fileLoad d "language" readMaybe

instance Load Username where
  load d = fileLoad d "username" (same . Username)

instance Load Email where
  load d = fileLoad d "email" (same . email')

instance Load TimeZoneName where
  load d = fileLoad d "timezonename" readMaybe

instance Load Assignment where
  load d = assignmentAna
      (loadName d)
      (fileLoad d "description" same)
      (fileLoad d "type"  readMaybe)
      (fileLoad d "start" readMaybe)
      (fileLoad d "end"   readMaybe)
      (fileLoad d "evtype" readMaybe)

instance Load Submission where
  load d = do
    s <- fileLoad d "solution" same
    p <- fileLoad d "date" readMaybe
    return $ Submission {
        solution = s
      , solutionPostDate = p
      }

instance Load Evaluation where
  load d = do
    e <- fileLoad d "evaluation" same
    r <- fileLoad d "result" readMaybe
    return Evaluation {
      writtenEvaluation = e
    , evaluationResult  = r
    }

instance Load Comment where
  load d = commentAna
    (fileLoad d "comment" same)
    (fileLoad d "author" same)
    (fileLoad d "date" readMaybe)
    (fileLoad d "type" readMaybe)

instance Load Course where
  load d = courseAppAna
    (loadName d)
    (loadDesc d)
    (fileLoad d "scripttype" readMaybe)

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
    zone  <- load d
    lang  <- load d
    return $ User {
        u_role = role
      , u_username = uname
      , u_email = email
      , u_name = name
      , u_timezone = zone
      , u_language = lang
      }

instance Load UserRegistration where
  load d = fileLoad d "userreg" readMaybe

instance Load TestScript where
  load d = testScriptAppAna
    (loadName d)
    (loadDesc d)
    (fileLoad d "notes" same)
    (fileLoad d "script" same)
    (fileLoad d "type" readMaybe)

instance Load TestCase where
  load d = testCaseAppAna
    (loadName d)
    (loadDesc d)
    (fileLoadBS d "value")
    (fileLoad d "type" readMaybe)
    (fileLoad d "info" same)

instance Load Feedback where
  load d = mkFeedback
    (fileLoad d "info" maybeDecodeJSON)
    (fileLoad d "date" readMaybe)

-- * Update instances

instance Update Language where
  update d = fileUpdate d "language" . show

instance Update Role where
  update d r = fileUpdate d "role" (printRole r)

instance Update Username where
  update d (Username s) = fileUpdate d "username" s

instance Update Email where
  update d (Email e) = fileUpdate d "email" e

instance Update TimeZoneName where
  update d = fileUpdate d "timezonename" . show

instance Update User where
  update d = userCata $ \username role email name timezone language -> do
    update d username
    update d role
    update d email
    updateName d name
    update d timezone
    update d language

instance Update Evaluation where
  update d e = do
    fileUpdate d "evaluation" (writtenEvaluation e)
    fileUpdate d "result"      (show . evaluationResult $ e)

instance Update Assignment where
  update d = assignmentCata $ \name desc type_ start end evtype -> do
    updateName d name
    fileUpdate d "description" desc
    fileUpdate d "type"        (show type_)
    fileUpdate d "start"       (show start)
    fileUpdate d "end"         (show end)
    fileUpdate d "evtype"      (show evtype)

instance Update TestScript where
  update d = testScriptCata show $ \name desc notes script type_ -> do
    updateName d name
    fileUpdate d "description" desc
    fileUpdate d "notes" notes
    fileUpdate d "script" script
    fileUpdate d "type" type_

instance Update TestCase where
  update d = testCaseCata show $ \name desc value type_ info -> do
    updateName d name
    fileUpdate d "description" desc
    fileUpdateBS d "value" value
    fileUpdate d "type" type_
    fileUpdate d "info" info

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
    files       = ["email", "name", "role", "username", "language"]
  , directories = ["course", "group" ,"courseadmin" ,"groupadmin", "submissions", "datadir"]
  }

assignmentDirStructure = DirStructure {
    files =
      [ "name" -- The name of the assignment
      , "description" -- The description that appears on the ui
      , "type"    -- The type of the assignment
      , "start"   -- The start date of from when the assignment is active
      , "end"     -- The end data of from when the assignment is inactive
      , created   -- The time when the assignment is created
      , "evtype"  -- Evaluation type
      ]
  , directories =
      [ "group"      -- The group of the assignment OR
      , "course"     -- The course of the assignment
      , "submission" -- The submissions for the assignment
      , "test-case"  -- The optional test case for the assignment
      ]
  }

submissionDirStructure = DirStructure {
    files = ["solution", "date"]
  , directories = [
        "assignment"
      , "user"
      , "evaluation"
      , "comment"
      , "feedback"
      ]
  }

-- Course directory structure
courseDirStructure = DirStructure {
    files =
      [ "description" -- Short description for the course
      , "name"        -- Name that usually appears on the UI
      , "scripttype"  -- Uniform type of the associated test scripts
      ]
  , directories =
      [ "groups"       -- Soft links to the groups associated with the course
      , "assignments"  -- Soft links to the assignments associated with the course
      , "users"        -- Soft links to the users that are actively registered for the course
      , "admins"       -- Soft links to the users that administrates the course
      , "unsubscribed" -- Soft links to the users that are subscribed and unsubscribed to the course at least once
      , "test-script"  -- Soft link to the test-script for the course
      ]
  }

groupDirStructure = DirStructure {
    files =
      [ "description" -- Short description for the group
      , "name"        -- Name that usually appears on the UI
      ]
  , directories =
      [ "users"        -- Soft links to the users that are actively registered for the group
      , "course"       -- Soft links to the course that the group is associated with
      , "admins"       -- Soft links to the users that administrated the group
      , "assignments"  -- Soft links to the assignments that are associated with the group
      , "unsubscribed" -- Soft links to the users that are subscribed and unsubscribed to the group at least once
      ]
  }

testScriptDirStructure = DirStructure {
    files =
      [ "name"        -- The name of the test script that appears on the UI
      , "description" -- The short description
      , "notes"       -- The Notes appear for the test case creator
      , "script"      -- The script itself, which will be substituated into the test framework
      , "type"        -- The type of the associated test cases
      ]
  , directories =
      [ "course" ]    -- The course that the test script is associated with
  }

testCaseDirStructure = DirStructure {
    files =
      [ "name"        -- The name of the test case that appears on the UI
      , "description" -- The description of the test case that optionally appears on the UI
      , "value"       -- The value of the test case, which can be a zip file or a plain text
      , "type"        -- The type of the value
      , "info"        -- Extra information which interpretation depends on the type of the value
      ]
  , directories =
      [ "test-script" -- The test script of the test case
      , "assignment" -- The assignment of the test case
      ]
  }

evaluationDirStructure = DirStructure {
    files       = ["result", "evaluation"]
  , directories = ["submission"]
  }

commentDirStructure = DirStructure {
    files = ["comment", "author", "date", "type"]
  , directories = ["submission"]
  }

userRegDirStructure = DirStructure {
    files = ["userreg"]
  , directories = []
  }

testJobDirStructure = DirStructure {
    files = [
        "submission" -- Submission for the solution
      , "script"     -- Script for the testing
      , "tests"      -- Test cases for the testing
      ]
  , directories = []
  }

feedbackDirStructure = DirStructure {
    files = [ "info", "date" ]
  , directories = [ "submission" ]
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

maybeDecodeJSON :: (Data a) => String -> Maybe a
maybeDecodeJSON s = unsafePerformIO $
  evaluate (Just $ decodeJSON s)
  `catch`
  nothing
    where
      nothing :: SomeException -> IO (Maybe a)
      nothing _ = return Nothing

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
  , evaluationDirStructure
  , commentDirStructure
  , userRegDirStructure
  , testScriptDirStructure
  , testCaseDirStructure
  , testJobDirStructure
  , feedbackDirStructure
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
