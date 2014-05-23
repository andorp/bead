{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.UploadFile (
    uploadFile
  ) where

import           Control.Monad.Error
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import           Snap.Util.FileUploads
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Snap.Application
import           Bead.View.Snap.Content

newtype PageData = PageData [(UsersFile, FileInfo)]

pageDataCata f (PageData x) = f x

uploadFile = ViewModifyHandler getUploadFile postUploadFile

getUploadFile :: GETContentHandler
getUploadFile = withUserState $ \s -> do
  fs <- userStory Story.listUsersFiles
  renderPagelet . withUserFrame s $ uploadFileContent (PageData fs)

data Success
  = PolicyFailure String
  | UnnamedFile
  | StoryError String
  | Success

successCata
  policyFailure
  unnamedFile
  storyError
  success
  s = case s of
    PolicyFailure msg -> policyFailure msg
    UnnamedFile       -> unnamedFile
    StoryError    msg -> storyError    msg
    Success           -> success

isSuccess Success = True
isSuccess _       = False

isFailure = not . isSuccess

-- NOTE: This content handler a bit special case. As in out model, we use
-- the POST content handlers basically to compute the UserAction which will be interpreted
-- as UserStory, but the handleFileUploads works differently, as it deletes the upladed files
-- after the given 'parts' handlers are run. In our model, this is not an option. In this special
-- case we run the saveUsersFile story manually.
postUploadFile :: POSTContentHandler
postUploadFile =
  join $ lift $ do
    tmpDir <- withTop tempDirContext $ getTempDirectory
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy $ \parts -> do
      case parts of
        [] -> return . return . StatusMessage $ Msg_UploadFile_NoFileReceived "No file was received."
        [part] -> do result <- handlePart part
                     return . return . StatusMessage $ successCata
                       (const $ Msg_UploadFile_PolicyFailure "Upload policy violation.")
                       (Msg_UploadFile_UnnamedFile "No file was chosen.")
                       (const $ Msg_UploadFile_InternalError "Internal error happened during upload.")
                       (Msg_UploadFile_Successful "File upload was sucessful.")
                       result
        _ -> do results <- mapM handlePart parts
                return . return . StatusMessage $ if (null $ filter isFailure results)
                  then (Msg_UploadFile_Successful "File upload was sucessful.")
                  else (Msg_UploadFile_ErrorInManyUploads "An error occured uploading one or more files.")
  where
    size128Kb = 128 * 1024
    perPartUploadPolicy = const $ allowWithMaximumSize size128Kb
    uploadPolicy = setMaximumFormInputSize size128Kb defaultUploadPolicy

    handlePart (_partInfo, Left exception) = return . PolicyFailure . T.unpack $ policyViolationExceptionReason exception
    handlePart (partInfo, Right filePath) =
      case (partFileName partInfo) of
        Just fp | not (B.null fp) -> saveFile fp
        _                         -> return UnnamedFile
      where
        saveFile name = do
          i18n <- i18nH
          r <- runStory $ Story.saveUsersFile filePath (UsersFile $ unpack name)
          case r of
            Left e  -> return . StoryError $ Story.translateUserError i18n e
            Right _ -> return Success

uploadFileContent :: PageData -> IHtml
uploadFileContent pd = do
  i18n <- getI18N
  return $ do
    H.h2 . fromString . i18n $ Msg_UploadFile_FileSelection "File Selection"
    do
      postForm (routeOf uploadFile) ! A.enctype "multipart/form-data" $ do
        fromString . i18n $ Msg_UploadFile_Info "Please choose a file to upload.  Note that the maximum file size is 128 KB."
        H.br
        H.br
        fileInput (fieldName fileUploadField)
        H.br
        submitButton (fieldName fileUploadSubmit) (i18n $ Msg_UploadFile_UploadButton "Upload")
    when (numFiles > 0) $ do
      H.h2 . fromString . i18n $ Msg_UploadFile_Directory "Uploaded Files"
      H.p $ do
        table (fieldName usersFileTableName) (className usersFileTableClass) # informationalTable $ do
          headerLine i18n
          usersFileLines pd
  where
    uploadFile = Pages.uploadFile ()
    dataCell   x = H.td # informationalCell $ x
    headerCell x = H.th # (informationalCell <> grayBackground) $ x
    headerLine i18n = H.tr $ do
      headerCell . fromString . i18n $ Msg_UploadFile_FileName "File Name"
      headerCell . fromString . i18n $ Msg_UploadFile_FileSize "File Size (bytes)"
      headerCell . fromString . i18n $ Msg_UploadFile_FileDate "File Date"
    numFiles       = pageDataCata length pd
    usersFileLines = pageDataCata (mapM_ usersFileLine)
      where
        usersFileLine (usersfile, fileInfo) = H.tr $ do
          usersFileCata (dataCell . fromString) usersfile
          flip fileInfoCata fileInfo $ \size date -> do
            dataCell . fromString $ show size
            dataCell . fromString $ show date
