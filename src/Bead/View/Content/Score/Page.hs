{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Score.Page (
    newUserScore
  , modifyUserScore
  ) where

import           Bead.View.Content
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams
import           Bead.View.Content.VisualConstants
import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation hiding (evConfig)

import           Text.Blaze.Html5 (Html,Attribute,(!))
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Printf
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
    
newUserScore :: ViewModifyHandler
newUserScore = ViewModifyHandler scorePage newScorePostHandler

modifyUserScore :: ViewModifyHandler
modifyUserScore = ViewModifyHandler modifyScorePage modifyScorePostHandler

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
    (return $ ErrorMessage $ msg_Evaluation_EmptyCommentAndFreeFormResult "Comments are not supported yet.")
    (\result -> do
      let e = Evaluation {
          evaluationResult = result
        , writtenEvaluation = ""
        }
      return $ SaveUserScore username ak e)

modifyScorePage = error "modifyScorePage is undefined"

modifyScorePostHandler = error "modifyScorePostHandler is undefined"

scoreContent :: PageData -> IHtml
scoreContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      "Course:"     .|. fromString (adCourse aDesc)
      "Assessment:" .|. fromString aTitle
      maybe mempty (\g -> "Group:" .|. fromString g) (adGroup aDesc)
      "Student:"    .|. fromString (pdStudent pd)
      "Username:"   .|. (uid fromString $ pdUid pd)
    postForm (routeOf newUserScore) $ do
      evaluationFrame (evConfig as) msg mempty
      submit
    where
      aDesc :: AssessmentDesc
      aDesc = pdAssessmentDesc pd

      as :: Assessment
      as = adAssessment aDesc

      aTitle :: String
      aTitle = assessment (\title _desc _creation _cfg -> title) as

      submit :: Html
      submit = Bootstrap.submitButtonWithAttr mempty "Submit"

      newUserScore = Pages.newUserScore (adAssessmentKey aDesc) (pdUsername pd) ()

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
