{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillNewGroupAssessmentPreview
  , fillNewCourseAssessmentPreview
  , modifyAssessment
  , modifyAssessmentPreview
  , viewAssessment
  ) where

import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.ScoreInfo (scoreInfoToIcon)
import           Bead.View.RequestParams
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation
import           Data.String (fromString)
import           Bead.Config (maxUploadSizeInKb)
import           Bead.Domain.Types (readMaybe)

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)

import           Data.Char (toUpper,isSpace)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BsUTF8 (toString)
import           Data.Function (on)
import qualified Data.Map as M
import           Data.List (sortBy)
import           Data.String.Utils (strip)
import           Data.Time (getCurrentTime)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))
import           Control.Monad (join)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)

-- * Content Handlers

newGroupAssessment = ViewModifyHandler newGroupAssessmentPage postNewGroupAssessment
newCourseAssessment = ViewModifyHandler newCourseAssessmentPage postNewCourseAssessment
fillNewGroupAssessmentPreview = UserViewHandler fillNewGroupAssessmentPreviewPage
fillNewCourseAssessmentPreview = UserViewHandler fillNewCourseAssessmentPreviewPage
modifyAssessment = ViewModifyHandler modifyAssessmentPage postModifyAssessment
modifyAssessmentPreview = UserViewHandler modifyAssessmentPreviewPage
viewAssessment = ViewHandler viewAssessmentPage

type Title = String
type Description = String
type CsvContent = B.ByteString

data PageData = PD_NewCourseAssessment CourseKey
              | PD_NewGroupAssessment GroupKey
              | PD_PreviewCourseAssessment CourseKey Title Description EvConfig CsvContent [UserDesc]
              | PD_PreviewGroupAssessment GroupKey Title Description EvConfig CsvContent [UserDesc]
              | PD_ModifyAssessment {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , courseOrGroupKey   :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                }
              | PD_ModifyAssessmentPreview {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , courseOrGroupKey   :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                , pdCsv              :: CsvContent
                , pdUsernames        :: [UserDesc]
                } 

pageDataAlgebra
  newCourseAssessment
  newGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  modifyAssessment
  modifyAssessmentPreview
  pdata =
      case pdata of
        PD_NewCourseAssessment ck -> newCourseAssessment ck
        PD_NewGroupAssessment gk -> newGroupAssessment gk
        PD_PreviewCourseAssessment ck title description evConfig scores usernames -> previewCourseAssessment ck title description evConfig scores usernames
        PD_PreviewGroupAssessment gk title description evConfig scores usernames -> previewGroupAssessment gk title description evConfig scores usernames
        PD_ModifyAssessment ak as cGKey isScoreSubmitted -> modifyAssessment ak as cGKey isScoreSubmitted
        PD_ModifyAssessmentPreview ak as cGKey isScoreSubmitted csv usernames -> modifyAssessmentPreview ak as cGKey isScoreSubmitted csv usernames

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
  msg <- lift i18nH
  uploadResult <- uploadFile
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  evaluations <- read <$> getParameter evaluationsParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now evalConfig
  case uploadResult of
    [File _name contents] -> do
      let scores = parseEvaluations msg evalConfig (readCsv contents)
      return $ SaveScoresOfGroupAssessment gk a scores
    _ -> return $ if M.null evaluations
                  then CreateGroupAssessment gk a
                  else SaveScoresOfGroupAssessment gk a evaluations

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ fillAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  msg <- lift i18nH
  uploadResult <- uploadFile
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  evaluations <- read <$> getParameter evaluationsParam
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let a = Assessment title description now evalConfig
  case uploadResult of
    [File _name contents] -> do
      let scores = parseEvaluations msg evalConfig (readCsv contents)
      return $ SaveScoresOfCourseAssessment ck a evaluations
    _ -> return $ if M.null evaluations
                  then CreateCourseAssessment ck a
                  else SaveScoresOfCourseAssessment ck a evaluations

parseEvaluations :: I18N -> EvConfig -> M.Map Username String -> M.Map Username Evaluation
parseEvaluations msg evalConfig = M.mapMaybe (parseEvaluation msg evalConfig)

parseEvaluation :: I18N -> EvConfig -> String -> Maybe Evaluation
parseEvaluation _msg _evalConfig "" = Nothing
parseEvaluation msg evalConfig s   = evConfigCata 
                                 (mkEval <$> readBinary)
                                 (\_ -> mkEval <$> readPercentage)
                                 (Just . mkEval . freeFormResult $ s)
                                 evalConfig
    where mkEval :: EvResult -> Evaluation
          mkEval result = Evaluation result ""

          readBinary :: Maybe EvResult
          readBinary | isAccepted = Just . binaryResult $ Passed
                     | isRejected = Just . binaryResult $ Failed
                     | otherwise  = Nothing
              where
                isAccepted = normalized `elem` [accepted,"+","1"]
                isRejected = normalized `elem` [rejected,"-","0"]
                normalized = map toUpper (strip s)
                accepted   = map toUpper (msg . msg_NewAssessment_Accepted $ "Accepted")
                rejected   = map toUpper (msg . msg_NewAssessment_Rejected $ "Rejected")

          readPercentage :: Maybe EvResult
          readPercentage = case readMaybe s :: Maybe Int of
                             Just p -> if p >= 0 && p <= 100
                                       then Just . percentageResult . (/100) . fromIntegral $ p
                                       else Nothing
                             Nothing -> Nothing

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"
evaluationsParam = stringParameter "evaluations" "Evaluations"
evConfigParam = evalConfigParameter "evConfig"

fillNewGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPreviewPage = do
  uploadResult <- uploadFile
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evConfig <- getParameter evConfigParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  let [File _name csvContents] = uploadResult
  users <- userStory $ do
    usernames <- Story.subscribedToGroup gk
    mapM Story.loadUserDesc usernames
  return $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk title description evConfig csvContents users

uploadFile :: ContentHandler [UploadResult]
uploadFile = do
  results <- join . lift $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy $ \parts -> do
      results <- mapM handlePart parts
      return . return $ results
  return $ filter isFile results
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

          isFile :: UploadResult -> Bool
          isFile (File _ _) = True
          isFile _          = False
                         
fillNewCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPreviewPage = error "fillNewCourseAssessmentPreviewPage is undefined"

fillAssessmentTemplate :: PageData -> IHtml
fillAssessmentTemplate pdata = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      H.form ! A.method "post" $ do
        Bootstrap.textInputWithDefault "n1" (titleLabel msg) title
        Bootstrap.optionalTextInputWithDefault "n2" (descriptionLabel msg) description
        if readOnlyEvType
          then showEvaluationType msg selectedEvType
          else evTypeSelection msg selectedEvType
        Bootstrap.formGroup $ fileInput "csv"
        Bootstrap.row $ do
          Bootstrap.colMd4 (previewButton msg ! A.disabled "")
          Bootstrap.colMd4 (downloadCsvButton msg)
          Bootstrap.colMd4 (commitButton msg)
        let csvTable csvContents usernames = do
              let scores = parseEvaluations msg selectedEvType (readCsv csvContents)
              previewTable msg usernames scores
              hiddenInput "evaluations" (show scores)
            noPreview = hiddenInput "evaluations" (show (M.empty :: M.Map Username Evaluation))
            
        pageDataAlgebra
          (\_ -> noPreview)
          (\_ -> noPreview)
          (\_ _ _ _ csv usernames -> csvTable csv usernames)
          (\_ _ _ _ csv usernames -> csvTable csv usernames)
          (\_ _ _ _ -> noPreview)
          (\_ _ _ _ csv usernames -> csvTable csv usernames)
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
                        getCsvLink
                        (msg . msg_NewAssessment_GetCsvButton $ "Get CSV")
    commitButton msg = Bootstrap.submitButtonWithAttr
                   (formAction commit "multipart/form-data")
                   (msg . msg_NewAssessment_SaveButton $ "Commit")

    enablePreviewButton = H.script . fromString $ unwords
                            [ "document.getElementById('csv').onchange = function() {"
                            , "  document.getElementById('preview').disabled = false;"
                            , "};"
                            ]

    title, description :: String
    (title,description) = pageDataAlgebra
                            (\_ -> ("",""))
                            (\_ -> ("",""))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ as _ _ -> assessment (\title description _ _ -> (title,description)) as)
                            (\_ as _ _ _ _ -> assessment (\title description _ _ -> (title,description)) as)
                            pdata

    preview = pageDataAlgebra
                (\ck -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ak _ _ _ -> Pages.modifyAssessmentPreview ak ())
                (\ak _ _ _ _ _ -> Pages.modifyAssessmentPreview ak ())
                pdata
    getCsvLink = pageDataAlgebra
                 (\ck -> getEmptyCourseCsv ck)
                 (\gk -> getEmptyGroupCsv gk)
                 (\ck _ _ _ _ _ -> getEmptyCourseCsv ck)
                 (\gk _ _ _ _ _ -> getEmptyGroupCsv gk)
                 (\ak _ cGKey _ -> getFilledCsv ak cGKey)
                 (\ak _ cGKey _ _ _ -> getFilledCsv ak cGKey)
               pdata
        where
          getFilledCsv ak cGKey = either (getFilledCourseCsv ak) (getFilledGroupCsv ak) cGKey
          getFilledCourseCsv ak ck = routeWithOptionalParams (Pages.getCourseCsv ck ()) [requestParam ak]
          getFilledGroupCsv ak gk = routeWithOptionalParams (Pages.getGroupCsv gk ()) [requestParam ak]
          getEmptyCourseCsv ck = routeOf $ Pages.getCourseCsv ck ()
          getEmptyGroupCsv gk = routeOf $ Pages.getGroupCsv gk ()

    commit = pageDataAlgebra
               (\ck -> Pages.newCourseAssessment ck ())
               (\gk -> Pages.newGroupAssessment gk ())
               (\ck _ _ _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ _ _ -> Pages.newGroupAssessment gk ())
               (\ak _ _ _ -> Pages.modifyAssessment ak ())
               (\ak _ _ _ _ _ -> Pages.modifyAssessment ak ())
               pdata

    readOnlyEvType = pageDataAlgebra
                       (\_ -> False)
                       (\_ -> False)
                       (\_ _ _ _ _ _ -> False)
                       (\_ _ _ _ _ _ -> False)
                       (\_ _ _ isScoreSubmitted -> isScoreSubmitted)
                       (\_ _ _ isScoreSubmitted _ _ -> isScoreSubmitted)
                       pdata

    showEvaluationType :: I18N -> EvConfig -> H.Html
    showEvaluationType msg eType =
      Bootstrap.formGroup $ do
        Bootstrap.readOnlyTextInputWithDefault ""
          (msg $ msg_NewAssessment_EvaluationType "Evaluation Type")
          (evConfigCata
            (fromString . msg $ msg_NewAssessment_BinaryEvaluation "Binary")
            (const . fromString . msg $ msg_NewAssessment_PercentageEvaluation "Percentage")
            (fromString . msg $ msg_NewAssessment_FreeFormEvaluation "Free form textual")
            eType)
        hiddenInput "evConfig" (Bootstrap.encode "Evaluation type" eType)
        fromString . msg $ msg_NewAssessment_EvalTypeWarn "The evaluation type can not be modified, there is a score for the assessment."

    selectedEvType = pageDataAlgebra
                       (\_ -> defaultEvType)
                       (\_ -> defaultEvType)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       (\_ as _ _ -> evaluationCfg as)
                       (\_ as _ _ _ _ -> evaluationCfg as)
                       pdata
                           where defaultEvType = binaryConfig

previewTable :: I18N -> [UserDesc] -> M.Map Username Evaluation -> H.Html
previewTable msg users evaluations = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th studentName >> H.th username >> H.th score
          where 
            studentName = fromString . msg . msg_NewAssessment_StudentName $ "Name"
            username = fromString . msg . msg_NewAssessment_UserName $ "Username"
            score = fromString . msg . msg_NewAssessment_Score $ "Score"
             
      tableData :: H.Html
      tableData = mapM_ tableRow (sortBy (compare `on` ud_fullname) users)

      tableRow :: UserDesc -> H.Html
      tableRow user = H.tr $ do
        H.td $ fromString fullname
        H.td $ fromString user_uid
        H.td $ case M.lookup username evaluations of
                 Just evaluation -> let scoreInfo = evaluationCata (\result _comment -> (Score_Result (EvaluationKey "") result)) evaluation in
                                    scoreInfoToIcon msg scoreInfo
                 Nothing         -> scoreInfoToIcon msg Score_Not_Found

          where fullname = ud_fullname user
                user_uid = uid id . ud_uid $ user
                username = ud_username user

readCsv :: B.ByteString -> M.Map Username String
readCsv bs = foldr (f . dropSpaces) M.empty (B.lines bs)
    where
      f line m | B.null line        = m
               | B.head line == '#' = m
               | otherwise          = if (not (null score'))
                                      then M.insert username' score' m
                                      else m
          where
            (_fullname,unameAndScore) = B.break (== ',') line
 
            (username,score) = case B.uncons unameAndScore of
                                 Just (',',bs) -> B.break (== ',') bs
                                 _             -> ("","")
 
            score' = case B.uncons score of
                       Just (',',cs) -> BsUTF8.toString cs
                       _             -> ""
 
            username' = Username . B.unpack . stripSpaces $ username

      dropSpaces = B.dropWhile isSpace
      stripSpaces = B.reverse . dropSpaces . B.reverse . dropSpaces

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
  (as,cGKey,scoreSubmitted) <- userStory $ do
    as <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    return (as,cGKey,scoreSubmitted)
  return . fillAssessmentTemplate $ PD_ModifyAssessment ak as cGKey scoreSubmitted

postModifyAssessment :: POSTContentHandler
postModifyAssessment = do
  msg <- lift i18nH
  uploadResult <- uploadFile
  ak <- getParameter assessmentKeyPrm
  newTitle <- getParameter titleParam
  newDesc <- getParameter descriptionParam
  selectedEvType <- getParameter evConfigParam
  evaluations <- read <$> getParameter evaluationsParam
  now <- liftIO getCurrentTime
  let a = Assessment {
            title         = newTitle
          , description   = newDesc
          , evaluationCfg = selectedEvType
          , created       = now
          }
  case uploadResult of
    [File _name contents] -> do
      let scores = parseEvaluations msg selectedEvType (readCsv contents)
      return $ ModifyAssessmentAndScores ak a scores
    _ -> return $ if M.null evaluations
                  then ModifyAssessment ak a
                  else ModifyAssessmentAndScores ak a evaluations
  
modifyAssessmentPreviewPage :: ViewPOSTContentHandler
modifyAssessmentPreviewPage = do
  uploadResult <- uploadFile
  ak <- getParameter assessmentKeyPrm
  (as,cGKey,scoreSubmitted,users) <- userStory $ do
    as <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    usernames <- either Story.subscribedToCourse Story.subscribedToGroup cGKey
    users <- mapM Story.loadUserDesc usernames
    return (as,cGKey,scoreSubmitted,users)
  let [File _name csvContents] = uploadResult
  return . fillAssessmentTemplate $ PD_ModifyAssessmentPreview ak as cGKey scoreSubmitted csvContents users

