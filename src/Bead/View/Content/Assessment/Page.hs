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
import           Data.Time (getCurrentTime)
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
                  | PD_PreviewCourseAssessment CourseKey String String (M.Map Username Score) [Username]
                  | PD_PreviewGroupAssessment GroupKey String String (M.Map Username Score) [Username]

fillDataCata
  fillCourseAssessment
  fillGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  pdata =
      case pdata of
        PD_FillCourseAssessment ck title description -> fillCourseAssessment ck title description
        PD_FillGroupAssessment gk title description -> fillGroupAssessment gk title description
        PD_PreviewCourseAssessment ck title description scores usernames -> previewCourseAssessment ck title description scores usernames
        PD_PreviewGroupAssessment gk title description scores usernames -> previewGroupAssessment gk title description scores usernames

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
  scores <- read <$> getParameter scoresParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now binaryConfig
  return $ if M.null scores
             then CreateGroupAssessment gk a
             else SaveScoresOfGroupAssessment gk a scores

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ newAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  scores <- read <$> getParameter scoresParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now binaryConfig
  return $ if M.null scores
             then CreateCourseAssessment ck a
             else SaveScoresOfCourseAssessment ck a scores

fillNewGroupAssessmentPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ fillAssessmentTemplate $ PD_FillGroupAssessment gk title description

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"
scoresParam = stringParameter "scores" "Scores"

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
  return $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk title description (readCsv contents) usernames
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
      H.form ! A.method "post" $ do
        Bootstrap.textInputWithDefault "n1" "Title" title
        Bootstrap.textInputWithDefault "n2" "Description" description
        Bootstrap.formGroup $ fileInput "csv"
        Bootstrap.row $ do
             Bootstrap.colMd4 previewButton 
             Bootstrap.colMd4 downloadCsvButton
             Bootstrap.colMd4 commitButton
        let csvTable _ _ _ scores usernames = do
                                 previewTable usernames scores
                                 hiddenInput "scores" (show scores)
            noPreview = hiddenInput "scores" (show (M.empty :: M.Map Username Score))
            
        fillDataCata
          (\_ _ _ -> noPreview)
          (\_ _ _ -> noPreview)
          csvTable
          csvTable
          pdata

  where
    formAction page encType = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "'; form.enctype='", encType, "';"])
    previewButton = Bootstrap.submitButtonWithAttr
                    (formAction preview "multipart/form-data")
                    "Preview"
    downloadCsvButton = Bootstrap.blockButtonLink
                        (routeOf getCsv)
                        "Get CSV"
    commitButton = Bootstrap.submitButtonWithAttr
                   (formAction commit "application/x-www-form-urlencoded")
                   "Commit"

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

    commit = fillDataCata
               (\ck _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ -> Pages.newGroupAssessment gk ())
               (\ck _ _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ _ -> Pages.newGroupAssessment gk ())
               pdata

previewTable :: [Username] -> M.Map Username Score -> H.Html
previewTable usernames scores = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th "Username" >> H.th "Score"
             
      tableData :: H.Html
      tableData = mapM_ tableRow usernames

      tableRow username =
          H.tr $ do
            H.td $ usernameCata H.string username
            H.td $ case M.lookup username scores of
                     Just score -> scoreCata H.string score
                     Nothing    -> warning
              where
                warning = H.i ! A.class_ "glyphicon glyphicon-warning-sign" ! A.style "color:#AAAAAA; font-size: xx-large" $ mempty

readCsv :: B.ByteString -> M.Map Username Score
readCsv bs = case B.lines bs of
               _:ls@(_:_) -> foldr f M.empty ls
               _ -> M.empty
    where f line m = let (username,score) = B.break (== ',') line
                         username' = Username . B.unpack $ username
                     in
                       if (not ((B.null score) || (score == ",")))
                       then M.insert username' (Score . B.unpack . B.tail $ score) m
                       else m


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
        hiddenInput "scores" (show (M.empty :: M.Map Username Score))
        Bootstrap.row $ do
             let formAction page = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction fill) "Fill"
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction assessment) "Commit"

  where
    assessment = case pdata of
                   PD_NewCourseAssessment ck -> Pages.newCourseAssessment ck ()
                   PD_NewGroupAssessment gk  -> Pages.newGroupAssessment gk ()
    fill = case pdata of
             PD_NewCourseAssessment ck -> Pages.fillNewCourseAssessment ck ()
             PD_NewGroupAssessment gk  -> Pages.fillNewGroupAssessment gk ()


