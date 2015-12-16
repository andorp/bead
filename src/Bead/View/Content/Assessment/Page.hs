{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillNewGroupAssessment
  , fillNewGroupAssessmentPreview
  , fillNewCourseAssessment
  , fillNewCourseAssessmentPreview
  , viewAssessment
  ) where

import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation (binaryConfig)
import           Bead.Domain.Entities (Username(..))
import           Data.String (fromString)
import           Bead.Config (maxUploadSizeInKb)

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))
import           Control.Monad (join,forM_)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)

-- * Content Handlers

newGroupAssessment = ViewModifyHandler newGroupAssessmentPage postNewGroupAssessment
newCourseAssessment = ViewModifyHandler newCourseAssessmentPage postNewCourseAssessment
fillNewGroupAssessment = UserViewHandler fillNewGroupAssessmentPage
fillNewGroupAssessmentPreview = UserViewHandler fillNewGroupAssessmentPreviewPage
fillNewCourseAssessment = UserViewHandler fillNewCourseAssessmentPage
fillNewCourseAssessmentPreview = UserViewHandler fillNewCourseAssessmentPreviewPage
viewAssessment = ViewHandler viewAssessmentPage

data PageDataNew  = PD_NewCourseAssessment CourseKey
                  | PD_NewGroupAssessment  GroupKey
data PageDataFill = PD_FillCourseAssessment CourseKey String String
                  | PD_FillGroupAssessment  GroupKey  String String 
                  | PD_PreviewCourseAssessment CourseKey String String B.ByteString [Username]
                  | PD_PreviewGroupAssessment GroupKey String String B.ByteString [Username]

fillDataCata
  fillCourseAssessment
  fillGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  pdata =
      case pdata of
        PD_FillCourseAssessment ck title description -> fillCourseAssessment ck title description
        PD_FillGroupAssessment gk title description -> fillGroupAssessment gk title description
        PD_PreviewCourseAssessment ck title description csv usernames -> previewCourseAssessment ck title description csv usernames
        PD_PreviewGroupAssessment gk title description csv usernames ->
            previewGroupAssessment gk title description csv usernames

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile
  deriving (Eq,Show)

newGroupAssessmentPage :: GETContentHandler
newGroupAssessmentPage = do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ newAssessmentTemplate $ PD_NewGroupAssessment gk

postNewGroupAssessment :: POSTContentHandler
postNewGroupAssessment = do 
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  let a = Assessment title description binaryConfig
  return $ CreateGroupAssessment gk a

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ newAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  let a = Assessment title description binaryConfig
  return $ CreateCourseAssessment ck a

fillNewGroupAssessmentPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ fillAssessmentTemplate $ PD_FillGroupAssessment gk title description

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"

fillNewCourseAssessmentPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ fillAssessmentTemplate $ PD_FillCourseAssessment ck title description

fillNewGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPreviewPage = do
  uploadResult <- join $ lift $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy $ \parts -> do
      results <- mapM handlePart parts
      return . return $ results
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  let [File _name contents] = uploadResult
  usernames <- userStory (Story.subscribedToGroup gk)
  return $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk title description contents usernames
    where 
      handlePart (_partInfo, Left _exception) = return PolicyFailure
      handlePart (partInfo, Right filePath) =
          case (partFileName partInfo) of
            Just fp | not (B.null fp) -> do
              contents <- liftIO $ do
                exists <- doesFileExist filePath
                if exists
                 then do
                   body <- B.readFile filePath
                   return $ Just body
                 else return $ Nothing
              return $ case contents of
                         Just body -> File (unpack fp) body
                         _         -> InvalidFile
            _         -> return UnnamedFile
      
fillNewCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPreviewPage = error "fillNewCourseAssessmentPreviewPage is undefined"

fillAssessmentTemplate :: PageDataFill -> IHtml
fillAssessmentTemplate pdata = do
  _msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf preview) ! A.enctype "multipart/form-data" $ do
        Bootstrap.textInputWithDefault "n1" "Title" title
        Bootstrap.textInputWithDefault "n2" "Description" description
        fileInput ("as")
        Bootstrap.row $ do
             let formAction page = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
                 downloadCsvButton = Bootstrap.blockButtonLink
                   (routeOf getCsv)
                   "Get CSV"
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction preview) "Preview"
             Bootstrap.colMd6 downloadCsvButton
        let csvTable _ _ _ csv usernames = Bootstrap.table (previewTable csv usernames)
            noPreview = return ()
        fillDataCata
          (\_ _ _ -> noPreview)
          (\_ _ _ -> noPreview)
          csvTable
          csvTable
          pdata

  where
    previewTable :: B.ByteString -> [Username] -> H.Html
    previewTable csv usernames = do
      header
      tableData csv usernames
    header = H.tr $ H.th "Username" >> H.th "Score"
             
    tableData :: B.ByteString -> [Username] -> H.Html
    tableData csv usernames = mapM_ (tableRow (fromBytestring csv)) usernames

    tableRow scores username =
        H.tr $ do
          H.td (H.string $ usernameCata id username)
          H.td $ case M.lookup username scores of
                   Just score -> H.string score
                   Nothing    -> warning
        where
          warning = H.i ! A.class_ "glyphicon glyphicon-warning-sign" ! A.style "color:#AAAAAA; font-size: xx-large" $ mempty

    fromBytestring :: B.ByteString -> M.Map Username String
    fromBytestring bs = case B.lines bs of
                          _:ls@(_:_) -> foldr f M.empty ls
                          _ -> M.empty
        where f line m = let (username,score) = B.break (== ',') line
                             username' = Username . B.unpack $ username
                         in
                         if (not ((B.null score) || (score == ",")))
                         then M.insert username' (B.unpack . B.tail $ score) m
                         else m

    (title,description) = fillDataCata
                            (\_ title description -> (title,description))
                            (\_ title description -> (title,description))
                            (\_ title description _ _ -> (title,description))
                            (\_ title description _ _ -> (title,description))
                            pdata

    preview = fillDataCata
                (\ck _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                pdata
    getCsv = fillDataCata
               (\ck _ _ -> Pages.getCourseCsv ck ())
               (\gk _ _ -> Pages.getGroupCsv gk ())
               (\ck _ _ _ _ -> Pages.getCourseCsv ck ())
               (\gk _ _ _ _ -> Pages.getGroupCsv gk ())
               pdata

viewAssessmentPage :: GETContentHandler
viewAssessmentPage = error "viewAssessmentPage is undefined"
                     
newAssessmentTemplate :: PageDataNew -> IHtml
newAssessmentTemplate pdata = do
  _msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf assessment) $ do
        Bootstrap.textInput "n1" "Title" ""
        Bootstrap.textInput "n2" "Description" ""
        Bootstrap.row $ do
             let formAction page = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction $ fill) "Fill"
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction $ assessment) "Commit"

  where
    assessment = case pdata of
                   PD_NewCourseAssessment ck -> Pages.newCourseAssessment ck ()
                   PD_NewGroupAssessment gk  -> Pages.newGroupAssessment gk ()
    fill = case pdata of
             PD_NewCourseAssessment ck -> Pages.fillNewCourseAssessment ck ()
             PD_NewGroupAssessment gk  -> Pages.fillNewGroupAssessment gk ()

