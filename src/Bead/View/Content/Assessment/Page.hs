{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillNewGroupAssessmentPreview
  , fillNewCourseAssessmentPreview
  , modifyAssessment
  , viewAssessment
  ) where

import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.ScoreInfo (scoreInfoToIcon)
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
import           Data.Maybe (maybe)
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
fillNewGroupAssessmentPreview = UserViewHandler fillNewGroupAssessmentPreviewPage
fillNewCourseAssessmentPreview = UserViewHandler fillNewCourseAssessmentPreviewPage
modifyAssessment = ViewModifyHandler modifyAssessmentPage postModifyAssessment
viewAssessment = ViewHandler viewAssessmentPage

data PageDataNew  = PD_NewCourseAssessment CourseKey
                  | PD_NewGroupAssessment GroupKey
                  | PD_PreviewCourseAssessment CourseKey String String EvConfig (M.Map Username Evaluation) [Username]
                  | PD_PreviewGroupAssessment GroupKey String String EvConfig (M.Map Username Evaluation) [Username]

data PageDataModify = PD_PageDataModify {
    aKey             :: AssessmentKey
  , as               :: Assessment
  , isScoreSubmitted :: Bool
  }

newDataAlgebra
  newCourseAssessment
  newGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  pdata =
      case pdata of
        PD_NewCourseAssessment ck -> newCourseAssessment ck
        PD_NewGroupAssessment gk -> newGroupAssessment gk
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
  return $ fillAssessmentTemplate $ PD_NewGroupAssessment gk

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
  return $ fillAssessmentTemplate $ PD_NewCourseAssessment ck

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

parseEvaluations :: EvConfig -> M.Map Username String -> M.Map Username Evaluation
parseEvaluations evalConfig = M.mapMaybe (parseEvaluation evalConfig)

parseEvaluation :: EvConfig -> String -> Maybe Evaluation
parseEvaluation evalConfig s = evConfigCata 
                               (mkEval . binaryResult <$> (readMaybe s))
                               (\_ -> mkEval . percentageResult . (/100) <$> (readMaybe s))
                               (Just . mkEval . freeFormResult $ s)
                               evalConfig
    where mkEval :: EvResult -> Evaluation
          mkEval result = Evaluation result ""

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"
evaluationsParam = stringParameter "evaluations" "Evaluations"
evConfigParam = evalConfigParameter "evConfig"

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

fillAssessmentTemplate :: PageDataNew -> IHtml
fillAssessmentTemplate pdata = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      H.form ! A.method "post" $ do
        Bootstrap.textInputWithDefault "n1" (titleLabel msg) title
        Bootstrap.optionalTextInputWithDefault "n2" (descriptionLabel msg) description
        evTypeSelection msg selectedEvType
        Bootstrap.formGroup $ fileInput "csv"
        Bootstrap.row $ do
          Bootstrap.colMd4 (previewButton msg ! A.disabled "")
          Bootstrap.colMd4 (downloadCsvButton msg)
          Bootstrap.colMd4 (commitButton msg)
        let csvTable _ _ _ _ scores usernames = do
              previewTable msg usernames scores
              hiddenInput "evaluations" (show scores)
            noPreview = hiddenInput "evaluations" (show (M.empty :: M.Map Username Evaluation))
            
        newDataAlgebra
          (\_ -> noPreview)
          (\_ -> noPreview)
          csvTable
          csvTable
          pdata
        enablePreviewButton
        Bootstrap.turnSelectionsOn

  where
    titleLabel msg = msg . msg_NewAssessment_Title $ "Title"
    descriptionLabel msg = msg . msg_NewAssessment_Description $ "Description"

    formAction :: Pages.PageDesc -> String -> H.Attribute
    formAction page encType = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "'; form.enctype='", encType, "';"])
                              
    previewButton msg = Bootstrap.submitButtonWithAttr
                    (formAction preview "multipart/form-data" <> A.id "preview")
                    (msg . msg_NewAssessment_PreviewButton $ "Preview")
    downloadCsvButton msg = Bootstrap.blockButtonLink
                        (routeOf getCsv)
                        (msg . msg_NewAssessment_GetCsvButton $ "Get CSV")
    commitButton msg = Bootstrap.submitButtonWithAttr
                   (formAction commit "application/x-www-form-urlencoded")
                   (msg . msg_NewAssessment_SaveButton $ "Commit")

    enablePreviewButton = H.script . fromString $ unwords
                            [ "document.getElementById('csv').onchange = function() {"
                            , "  document.getElementById('preview').disabled = false;"
                            , "};"
                            ]

    (title,description) = newDataAlgebra
                            (\_ -> ("",""))
                            (\_ -> ("",""))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ title description _ _ _ -> (title,description))
                            pdata

    preview = newDataAlgebra
                (\ck -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                pdata
    getCsv = newDataAlgebra
               (\ck -> Pages.getCourseCsv ck ())
               (\gk -> Pages.getGroupCsv gk ())
               (\ck _ _ _ _ _ -> Pages.getCourseCsv ck ())
               (\gk _ _ _ _ _ -> Pages.getGroupCsv gk ())
               pdata

    commit = newDataAlgebra
               (\ck -> Pages.newCourseAssessment ck ())
               (\gk -> Pages.newGroupAssessment gk ())
               (\ck _ _ _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ _ _ -> Pages.newGroupAssessment gk ())
               pdata

    selectedEvType = newDataAlgebra
                       (\_ -> defaultEvType)
                       (\_ -> defaultEvType)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       pdata
                           where defaultEvType = binaryConfig

previewTable :: I18N -> [Username] -> M.Map Username Evaluation -> H.Html
previewTable msg usernames evaluations = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th username >> H.th score
          where username = fromString . msg . msg_NewAssessment_UserName $ "Username"
                score = fromString . msg . msg_NewAssessment_Score $ "Score"
             
      tableData :: H.Html
      tableData = mapM_ tableRow usernames

      tableRow username =
          H.tr $ do
            H.td $ usernameCata H.string username
            H.td $ case M.lookup username evaluations of
                     Just evaluation -> let scoreInfo = evaluationCata (\result _comment -> (Score_Result (EvaluationKey "") result)) evaluation in
                                        scoreInfoToIcon msg scoreInfo
                     Nothing         -> scoreInfoToIcon msg Score_Not_Found

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

evTypeSelection msg selected = Bootstrap.selectionWithLabel "evConfig" evalType (== selected) selection 
    where selection = [ (binaryConfig, binary)
                      , (percentageConfig 0.0, percentage)
                      , (freeFormConfig, freeForm)
                      ]
          evalType = msg . msg_NewAssessment_EvaluationType $ "Evaluation Type"
          binary = msg . msg_NewAssessment_BinaryEvaluation $ "Binary"
          percentage = msg . msg_NewAssessment_PercentageEvaluation $ "Percentage"
          freeForm = msg . msg_NewAssessment_FreeFormEvaluation $ "Free form textual"

modifyAssessmentPage :: GETContentHandler
modifyAssessmentPage = do
  ak <- getParameter assessmentKeyPrm
  (as,scoreSubmitted) <- userStory $ do
                           as <- Story.loadAssessment ak
                           scoreSubmitted <- Story.isThereAScore ak
                           return (as,scoreSubmitted)
  return . modifyAssessmentTemplate $ PD_PageDataModify ak as scoreSubmitted

postModifyAssessment :: POSTContentHandler
postModifyAssessment = do
  ak <- getParameter assessmentKeyPrm
  newTitle <- getParameter titleParam
  newDesc <- getParameter descriptionParam
  evConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let a = Assessment {
            title         = newTitle
          , description   = newDesc
          , evaluationCfg = evConfig
          , created       = now
          }
  return $ ModifyAssessment ak a

modifyAssessmentTemplate :: PageDataModify -> IHtml
modifyAssessmentTemplate pdata = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf commitPage) $ do
        Bootstrap.textInputWithDefault "n1" (title msg) aTitle
        Bootstrap.optionalTextInputWithDefault "n2" (description msg) aDesc
        if readOnly
          then Bootstrap.formGroup $ do
            showEvaluationType msg evConfig
            fromString . msg $ msg_NewAssessment_EvalTypeWarn "The evaluation type can not be modified, there is a score for the assessment."
          else
            evTypeSelection msg evConfig
        Bootstrap.rowColMd12 $ saveButton msg
        Bootstrap.turnSelectionsOn
    where
      title msg = msg . msg_NewAssessment_Title $ "Title"
      description msg = msg . msg_NewAssessment_Description $ "Description"

      aTitle,aDesc :: String
      evConfig     :: EvConfig
      (aTitle,aDesc,evConfig) = assessment (\title desc _creation cfg -> (title,desc,cfg)) (as pdata)

      readOnly :: Bool
      readOnly = isScoreSubmitted pdata
                                
      showEvaluationType msg eType = do
        Bootstrap.readOnlyTextInputWithDefault ""
          (msg $ msg_NewAssessment_EvaluationType "Evaluation Type")
          (evConfigCata
            (fromString . msg $ msg_NewAssessment_BinaryEvaluation "Binary")
            (const . fromString . msg $ msg_NewAssessment_PercentageEvaluation "Percentage")
            (fromString . msg $ msg_NewAssessment_FreeFormEvaluation "Free form textual")
            eType)
        hiddenInput "evConfig" (Bootstrap.encode "Evaluation type" eType)

      saveButton msg = Bootstrap.submitButtonWithAttr mempty commit
          where commit = msg . msg_NewAssessment_SaveButton $ "Commit"
                     
      commitPage = Pages.modifyAssessment (aKey pdata) ()


      
