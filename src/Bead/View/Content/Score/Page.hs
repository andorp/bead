{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Score.Page (
    viewUserScore
  , newUserScore
  , modifyUserScore
  ) where

import           Bead.View.Content
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams
import           Bead.View.Content.VisualConstants
import           Bead.View.Content.ScoreInfo (scoreInfoToText)
import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation hiding (evConfig)

import           Text.Blaze.Html5 (Html,Attribute,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf
import           Data.Either (either)
import           Data.List (intercalate)
import           Data.String (fromString)
import           Data.Monoid ((<>))

data PageData
  = PD_NewUserScore {
      pdStudent        :: String
    , pdUsername       :: Username
    , pdUid            :: Uid
    , pdAssessmentDesc :: AssessmentDesc
    }      
  | PD_ModifyUserScore {
      pdStudent        :: String
    , pdUsername       :: Username
    , pdUid            :: Uid
    , pdAssessmentDesc :: AssessmentDesc
    , pdScore          :: ScoreInfo
    , pdScoreKey       :: ScoreKey
    }

pageDataAlgebra
  newUserScore_
  modifyUserScore_
  pd = case pd of
         PD_NewUserScore student uname uid assessmentDesc -> newUserScore_ student uname uid assessmentDesc
         PD_ModifyUserScore student uname uid assessmentDesc score sk -> modifyUserScore_ student uname uid assessmentDesc score sk
    
newUserScore :: ViewModifyHandler
newUserScore = ViewModifyHandler scorePage newScorePostHandler

modifyUserScore :: ViewModifyHandler
modifyUserScore = ViewModifyHandler modifyScorePage modifyScorePostHandler

viewUserScore :: ViewHandler
viewUserScore = ViewHandler viewScorePage

scorePage :: GETContentHandler
scorePage = do 
  username <- getParameter $ usernamePrm
  ak <- getParameter $ assessmentKeyPrm
  user <- userStory (Story.loadUser username)
  let uname = u_name user
      uid = u_uid user
  assessmentDesc <- userStory (Story.assessmentDesc ak)
  return . scoreContent $ PD_NewUserScore uname username uid assessmentDesc

newScorePostHandler :: POSTContentHandler
newScorePostHandler = do
  username <- getParameter $ usernamePrm
  ak <- getParameter $ assessmentKeyPrm
  evConfig <- evConfig <$> userStory (Story.loadAssessment ak)
  evaluation <- getEvaluation evConfig
  return $ either id (SaveUserScore username ak) evaluation

getEvaluation :: EvConfig -> ContentHandler (Either UserAction Evaluation)
getEvaluation evConfig = do
  commentOrResult <-
    evConfigCata
    (getJSONParam (fieldName evaluationResultField) "No evaluation can be found.")
    (\_ -> do
      percentage  <- getParameter evaluationPercentagePrm
      return . EvCmtResult $ percentageResult (fromIntegral percentage / 100))
    (do freeForm <- getParameter freeFormEvaluationParam
        return . EvCmtResult $ freeFormResult freeForm)
    evConfig
  withEvalOrComment commentOrResult
    (return . Left . ErrorMessage $ msg_Evaluation_EmptyCommentAndFreeFormResult "Comments are not supported yet.")
    (\result -> return . Right $ Evaluation {
        evaluationResult = result
        , writtenEvaluation = ""       
        })

modifyScorePage :: GETContentHandler
modifyScorePage = do
  sk <- getParameter scoreKeyPrm
  (user,username,assessmentDesc,score) <- userStory $ do
     username <- Story.usernameOfScore sk
     user <- Story.loadUser username
     ak <- Story.assessmentOfScore sk
     assessmentDesc <- Story.assessmentDesc ak
     score <- Story.scoreInfo sk
     return (user,username,assessmentDesc,score)
  let uname = u_name user
      uid = u_uid user
  return . scoreContent $ PD_ModifyUserScore uname username uid assessmentDesc score sk

modifyScorePostHandler :: POSTContentHandler
modifyScorePostHandler = do
  sk <- getParameter scoreKeyPrm
  evConfig <- evConfig <$> userStory (do
                           ak <- Story.assessmentOfScore sk
                           Story.loadAssessment ak)
  evaluation <- getEvaluation evConfig
  return $ either id (ModifyUserScore sk) evaluation

scoreContent :: PageData -> IHtml
scoreContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_NewUserScore_Course $ "Course:" )    .|. fromString (adCourse aDesc)
      (msg . msg_NewUserScore_Assessment $ "Assessment:") .|. fromString aTitle
      maybe mempty (\g -> (msg . msg_NewUserScore_Group $ "Group:") .|. fromString g) (adGroup aDesc)
      (msg . msg_NewUserScore_Student $ "Student:")    .|. fromString (pdStudent pd)
      (msg . msg_NewUserScore_UserName $ "Username:")   .|. (uid fromString $ pdUid pd)
    postForm (routeOf handler) $ do
      view msg
      evaluationFrame (evConfig as) msg mempty
      submit msg
    where
      aDesc :: AssessmentDesc
      aDesc = pdAssessmentDesc pd

      as :: Assessment
      as = adAssessment aDesc

      aTitle :: String
      aTitle = assessment (\title _desc _creation _cfg -> title) as

      submit :: I18N -> Html
      submit msg = Bootstrap.submitButtonWithAttr mempty (msg . msg_NewUserScore_Submit $ "Submit")

      handler = pageDataAlgebra
                  (\_student uname _uid aDesc -> Pages.newUserScore (adAssessmentKey aDesc) uname ())
                  (\_student _uname _uid _aDesc _score sk -> Pages.modifyUserScore sk ())
                  pd

      view msg = pageDataAlgebra
                 (\_student _uname _uid _aDesc -> mempty)
                 (\_student _uname _uid _aDesc score _sk -> Bootstrap.rowColMd12 . H.p . fromString $ (scoreInfoToText msg) score)
                 pd

viewScorePage :: GETContentHandler
viewScorePage = do
  sk <- getParameter scoreKeyPrm
  sDesc <- userStory $ Story.scoreDesc sk
  return $ viewScoreContent sDesc

viewScoreContent :: ScoreDesc -> IHtml
viewScoreContent sd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_ViewUserScore_Course $ "Course:")   .|. fromString (scdCourse sd)
      maybe mempty (\g -> (msg . msg_ViewUserScore_Group $ "Group:") .|. fromString g) (scdGroup sd)
      (msg . msg_ViewUserScore_Teacher $ "Teacher:") .|. (fromString . intercalate ", " . scdTeacher) sd
      (msg . msg_ViewUserScore_Assessment $ "Assessment:") .|. fromString (scdAssessment sd)
    Bootstrap.rowColMd12 . H.p . fromString . (scoreInfoToText msg) $ scdScore sd

evConfig :: Assessment -> EvConfig
evConfig = assessment (\_title _desc _creation cfg -> cfg)

evaluationFrame :: EvConfig -> I18N -> Html -> Html
evaluationFrame evConfig msg content = do
  hiddenInput (fieldName evalConfigParam) (encodeToFay' "inputEvalType" evConfig)
  withEvConfig evConfig
    (do content
        Bootstrap.formGroup $ evaluationDiv $
          Bootstrap.radioButtonGroup (fieldName evaluationResultField) $
            [ (True, encodeToFay' "inputEvalResult" $ binary Passed, msg $ msg_Evaluation_Accepted "Accepted")
            , (False, encodeToFay' "inputEvalResult" $ binary Failed, msg $ msg_Evaluation_Rejected "Rejected")
            ])
    -- When the page is dynamic the percentage spinner is hooked on the field
    (\_ ->
      do content
         Bootstrap.formGroup . evaluationDiv . Bootstrap.rowColMd12 $ do           
           fromString . msg $ msg_Evaluation_Percentage "Percentage: "
           H.input ! A.name (fieldName evaluationPercentagePrm) ! A.type_ "number"
                   ! A.min "0" ! A.max "100")
    (do Bootstrap.optionalTextInput (fieldName freeFormEvaluationParam) (msg $ msg_Evaluation_FreeFormEvaluation "Evaluation") ""
        H.p . fromString $ printf (msg $ msg_Evaluation_FreeForm_Information $ unwords
          [ "Note that this text will be used everywhere as the evaluation itself.  Hence it is recommended to keep"
          , "the length of the text under size %d, otherwise it may not be directly shown." ]) displayableFreeFormResultLength
        content)
  where
    binary = EvCmtResult . binaryResult
    evaluationDiv = withEvConfig
      evConfig
      (H.div)
      (const $ H.div ! A.id (fieldName evaluationPercentageDiv))
      (H.div)
             
evalConfigParam = evalConfigParameter (fieldName evaluationConfigField)
freeFormEvaluationParam = stringParameter (fieldName evaluationFreeFormField) "Free format evaluation"

