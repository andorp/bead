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
import           Bead.Domain.Shared.Evaluation
import           Bead.Domain.Entities (Username(..))
import           Data.String (fromString)
import           Bead.Config (maxUploadSizeInKb)
import           Bead.Domain.Types (readMaybe)

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
                  | PD_NewGroupAssessment GroupKey
data PageDataFill = PD_FillCourseAssessment CourseKey String String EvConfig
                  | PD_FillGroupAssessment GroupKey String String EvConfig
                  | PD_PreviewCourseAssessment CourseKey String String EvConfig (M.Map Username Evaluation) [Username]
                  | PD_PreviewGroupAssessment GroupKey String String EvConfig (M.Map Username Evaluation) [Username]

fillDataCata
  fillCourseAssessment
  fillGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  pdata =
      case pdata of
        PD_FillCourseAssessment ck title description evConfig -> fillCourseAssessment ck title description evConfig
        PD_FillGroupAssessment gk title description evConfig -> fillGroupAssessment gk title description evConfig
        PD_PreviewCourseAssessment ck title description evConfig scores usernames -> previewCourseAssessment ck title description evConfig scores usernames
        PD_PreviewGroupAssessment gk title description evConfig scores usernames -> previewGroupAssessment gk title description evConfig scores usernames

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
  evaluations <- read <$> getParameter evaluationsParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now evalConfig
  return $ if M.null evaluations
             then CreateGroupAssessment gk a
             else SaveScoresOfGroupAssessment gk a evaluations

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ newAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  evaluations <- read <$> getParameter evaluationsParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now evalConfig
  return $ if M.null evaluations
             then CreateCourseAssessment ck a
             else SaveScoresOfCourseAssessment ck a evaluations

validEvaluations :: EvConfig -> M.Map Username Evaluation -> M.Map Username Evaluation
validEvaluations config = M.filter valid
    where
      valid :: Evaluation -> Bool
      valid = evaluationCata (\result _ -> validResult result)

      validResult :: EvResult -> Bool
      validResult = evConfigCata
                    binResult
                    pctResult
                    freeResult
                    config

      binResult :: EvResult -> Bool
      binResult = evResultCata
                  (const True)   -- BinEval
                  (const False)  -- PctEval
                  (const False)  -- FreeEval

      pctResult :: Double -> EvResult -> Bool
      pctResult _ = evResultCata
                    (const False)
                    (const True)
                    (const False)

      freeResult :: EvResult -> Bool
      freeResult = evResultCata
                   (const False)
                   (const False)
                   (const True)

parseEvaluations :: EvConfig -> M.Map Username String -> M.Map Username Evaluation
parseEvaluations evalConfig = validEvaluations evalConfig . M.map parseEvaluation

parseEvaluation :: String -> Evaluation
parseEvaluation s = case readMaybe s of
                      Just r -> mkEval (binaryResult r)
                      Nothing -> case readMaybe s of
                                   Just p -> mkEval (percentageResult p)
                                   Nothing -> mkEval (freeFormResult s)
    where mkEval :: EvResult -> Evaluation
          mkEval result = Evaluation result ""

fillNewGroupAssessmentPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evConfig <- getParameter evConfigParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ fillAssessmentTemplate $ PD_FillGroupAssessment gk title description evConfig

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"
evaluationsParam = stringParameter "evaluations" "Evaluations"
evConfigParam = evalConfigParameter "evConfig"

fillNewCourseAssessmentPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPage = do
  title <- getParameter titleParam
  description <- read <$> getParameter descriptionParam
  evConfig <- getParameter evConfigParam
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ fillAssessmentTemplate $ PD_FillCourseAssessment ck title description evConfig

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
  evConfig <- getParameter evConfigParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  let [File _name contents] = uploadResult
      csvContents = readCsv contents
      evaluations = parseEvaluations evConfig csvContents
  usernames <- userStory (Story.subscribedToGroup gk)
  return $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk title description evConfig evaluations usernames
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
        evConfigSelection selectedConfig
        Bootstrap.formGroup $ fileInput "csv"
        Bootstrap.row $ do
             Bootstrap.colMd4 previewButton 
             Bootstrap.colMd4 downloadCsvButton
             Bootstrap.colMd4 commitButton
        let csvTable _ _ _ _ scores usernames = do
              previewTable usernames scores
              hiddenInput "evaluations" (show scores)
            noPreview = hiddenInput "evaluations" (show (M.empty :: M.Map Username Evaluation))
            
        fillDataCata
          (\_ _ _ _ -> noPreview)
          (\_ _ _ _ -> noPreview)
          csvTable
          csvTable
          pdata
        Bootstrap.turnSelectionsOn

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
                            (\_ title description _ -> (title,description))
                            (\_ title description _ -> (title,description))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ title description _ _ _ -> (title,description))
                            pdata

    preview = fillDataCata
                (\ck _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                pdata
    getCsv = fillDataCata
               (\ck _ _ _ -> Pages.getCourseCsv ck ())
               (\gk _ _ _ -> Pages.getGroupCsv gk ())
               (\ck _ _ _ _ _ -> Pages.getCourseCsv ck ())
               (\gk _ _ _ _ _ -> Pages.getGroupCsv gk ())
               pdata

    commit = fillDataCata
               (\ck _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ -> Pages.newGroupAssessment gk ())
               (\ck _ _ _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ _ _ -> Pages.newGroupAssessment gk ())
               pdata

    selectedConfig = fillDataCata
                     (\_ _ _ evConfig -> evConfig)
                     (\_ _ _ evConfig -> evConfig)
                     (\_ _ _ evConfig _ _ -> evConfig)
                     (\_ _ _ evConfig _ _ -> evConfig)
                     pdata

previewTable :: [Username] -> M.Map Username Evaluation -> H.Html
previewTable usernames evaluations = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th "Username" >> H.th "Evaluation"
             
      tableData :: H.Html
      tableData = mapM_ tableRow usernames

      tableRow username =
          H.tr $ do
            H.td $ usernameCata H.string username
            H.td $ case M.lookup username evaluations of
                     Just score -> "evaluation"
                     Nothing    -> warning
              where
                warning = H.i ! A.class_ "glyphicon glyphicon-warning-sign" ! A.style "color:#AAAAAA;" $ mempty

readCsv :: B.ByteString -> M.Map Username String
readCsv bs = case B.lines bs of
               _:ls@(_:_) -> foldr f M.empty ls
               _ -> M.empty
    where f line m = let (username,score) = B.break (== ',') line
                         username' = Username . B.unpack $ username
                     in
                       if (not ((B.null score) || (score == ",")))
                       then M.insert username' (B.unpack . B.tail $ score) m
                       else m


viewAssessmentPage :: GETContentHandler
viewAssessmentPage = error "viewAssessmentPage is undefined"

evConfigSelection selected = Bootstrap.selectionWithLabel "evConfig" "Evaluation Type" (== selected) selection 
    where selection = [ (binaryConfig, "Binary")
                      , (percentageConfig 0.0, "Percentage")
                      , (freeFormConfig, "Free form textual")
                      ]

newAssessmentTemplate :: PageDataNew -> IHtml
newAssessmentTemplate pdata = do
  _msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf assessment) $ do
        Bootstrap.textInput "n1" "Title" ""
        Bootstrap.textInput "n2" "Description" ""
        evConfigSelection binaryConfig
        hiddenInput "evaluations" (show (M.empty :: M.Map Username Evaluation))
        Bootstrap.row $ do
             let formAction page = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction fill) "Fill"
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction assessment) "Commit"
        Bootstrap.turnSelectionsOn

  where
    assessment = case pdata of
                   PD_NewCourseAssessment ck -> Pages.newCourseAssessment ck ()
                   PD_NewGroupAssessment gk  -> Pages.newGroupAssessment gk ()
    fill = case pdata of
             PD_NewCourseAssessment ck -> Pages.fillNewCourseAssessment ck ()
             PD_NewGroupAssessment gk  -> Pages.fillNewGroupAssessment gk ()


