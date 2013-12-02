{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaluation (
    evaluation
  , modifyEvaluation
  ) where

import Data.String (fromString)
import Data.Time (UTCTime, LocalTime)
import Control.Monad (liftM)

import Bead.Domain.Types (readMsg)
import Bead.Domain.Relationships (SubmissionDesc(..))
import Bead.Controller.Pages as P(Page(Evaluation, ModifyEvaluation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionDescription)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content as C
import Bead.View.Snap.Content.Comments

import Bead.Domain.Evaluation

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (id, class_, style)

evaluation :: Content
evaluation = getPostContentHandler evaluationPage evaluationPostHandler

modifyEvaluation :: Content
modifyEvaluation = getPostContentHandler modifyEvaluationPage modifyEvaluationPost

data PageData = PageData {
    sbmDesc :: SubmissionDesc
  , sbmKey  :: Either EvaluationKey SubmissionKey
  , userTime :: UserTimeConverter
  }

render (BinEval _) = renderPagelet
render (PctEval _) = renderDynamicPagelet

evaluationPage :: GETContentHandler
evaluationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  sd <- runStoryE (submissionDescription sk)
  tc <- usersTimeZoneConverter
  let pageData = PageData {
      sbmKey  = Right sk
    , sbmDesc = sd
    , userTime = tc
    }
  render (eConfig sd) $ withUserFrame s (evaluationContent pageData)

modifyEvaluationPage :: GETContentHandler
modifyEvaluationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  ek <- getParameter evaluationKeyPrm
  sd <- runStoryE (submissionDescription sk)
  tc <- usersTimeZoneConverter
  let pageData = PageData {
    sbmKey  = Left ek
  , sbmDesc = sd
  , userTime = tc
  }
  render (eConfig sd) $ withUserFrame s (evaluationContent pageData)

evaluationPostHandler :: POSTContentHandler
evaluationPostHandler = do
  sk <- getParameter submissionKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaluationResultField) "Evaluation result is not found"
  let e = C.Evaluation {
    evaluationResult = evResult er
  , writtenEvaluation = ev
  }
  return $ NewEvaluation sk e

modifyEvaluationPost :: POSTContentHandler
modifyEvaluationPost = do
  ek <- getParameter evaluationKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaluationResultField) "Evaluation result is not found"
  let e = C.Evaluation {
    evaluationResult = evResult er
  , writtenEvaluation = ev
  }
  return $ C.ModifyEvaluation ek e

evaluationContent :: PageData -> Pagelet
evaluationContent pd = onlyHtml $ mkI18NHtml $ \i -> do
  let sd = sbmDesc pd
      tc = userTime pd
  postForm (routeOf . evPage . sbmKey $ pd) $ H.div ! formDiv $ do
    H.div ! title $ H.h2 (translate i "Evaluation")
    H.div ! leftInfo $ do
      H.table $ do
        H.tr $ do
          H.td $ H.b $ (translate i "Course, Group: ")
          H.td $ (fromString . eGroup $ sd)
        H.tr $ do
          H.td $ H.b $ (translate i "Student: ")
          H.td $ (fromString . eStudent $ sd)
      H.div ! A.id (fieldName evaluationPercentageDiv) $
        translate i . inputEvalResult . eConfig $ sd
      submitButton (fieldName saveEvalBtn) (i "Save Evaluation")
    H.div ! rightText $ do
      textAreaInput (fieldName evaluationValueField) Nothing ! fillDiv
      hiddenKeyField . sbmKey $ pd
  H.div $ H.h2 $ (translate i "Submitted solution")
  H.div # submissionTextDiv $ H.pre # submissionTextPre $ do
    (fromString . eSolution $ sd)
  when (not . null $ eComments sd) $ do
    translate i . commentsDiv tc . eComments $ sd

  where
    defaultEvalCfg :: EvaluationResult
    defaultEvalCfg = BinEval (Binary Passed)

    hiddenKeyField (Left ek)  = hiddenInput (fieldName evaluationKeyField) (paramValue ek)
    hiddenKeyField (Right sk) = hiddenInput (fieldName submissionKeyField) (paramValue sk)

    evPage (Left  _) = P.ModifyEvaluation
    evPage (Right _) = P.Evaluation

inputEvalResult :: EvaluationConfig -> I18NHtml
inputEvalResult (BinEval cfg) = mkI18NHtml $ \i -> do
  valueSelection valueAndText (fieldName evaluationResultField) $
    [(Passed, i "Passed"), (Failed, i "Failed")]
  where
    valueAndText :: (Result, String) -> (String, String)
    valueAndText (v,n) = (errorOnNothing . encodeToFay . EvResult . mkEvalResult $ Binary v, n)

-- When the page is dynamic the percentage spinner is hooked on the field
inputEvalResult (PctEval cfg) = mkI18NHtml $ \i -> do
  hiddenInput
    (fieldName evaluationResultField)
    (fromString . errorOnNothing . encodeToFay . EvResult . mkEvalResult . Percentage $ Scores [0.0])

errorOnNothing = maybe (error "Error is encoding input result") id

-- CSS Section

formDiv = A.style "width: 100%; height: 200px"
title   = A.style "width: 100%"
leftInfo = A.style "float: left; width: 28%; height: 100%"
rightText = A.style "float: right; width: 68%; height: 100%"
fillDiv = A.style "width: 98%; height: 98%"

