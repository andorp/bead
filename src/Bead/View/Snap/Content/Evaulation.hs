{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaulation (
    evaulation
  , modifyEvaulation
  ) where

import Data.String (fromString)
import Control.Monad (liftM)

import Bead.Domain.Types (readMsg)
import Bead.Domain.Relationships (SubmissionDesc(..))
import Bead.Controller.Pages as P(Page(Evaulation, ModifyEvaulation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionDescription)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content as C
import Bead.View.Snap.Content.Comments

import Bead.Domain.Evaulation

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (id, class_, style)

evaulation :: Content
evaulation = getPostContentHandler evaulationPage evaulationPostHandler

modifyEvaulation :: Content
modifyEvaulation = getPostContentHandler modifyEvaulationPage modifyEvaulationPost

data PageData = PageData {
    sbmDesc :: SubmissionDesc
  , sbmKey  :: Either EvaulationKey SubmissionKey
  }

render (BinEval _) = renderPagelet
render (PctEval _) = renderDynamicPagelet

evaulationPage :: GETContentHandler
evaulationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
      sbmKey  = Right sk
    , sbmDesc = sd
    }
  render (eConfig sd) $ withUserFrame s (evaulationContent pageData)

modifyEvaulationPage :: GETContentHandler
modifyEvaulationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  ek <- getParameter evaluationKeyPrm
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
    sbmKey  = Left ek
  , sbmDesc = sd
  }
  render (eConfig sd) $ withUserFrame s (evaulationContent pageData)

evaulationPostHandler :: POSTContentHandler
evaulationPostHandler = do
  sk <- getParameter submissionKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaulationResultField) "Evaulation result is not found"
  let e = C.Evaulation {
    evaulationResult = evResult er
  , writtenEvaulation = ev
  }
  return $ NewEvaulation sk e

modifyEvaulationPost :: POSTContentHandler
modifyEvaulationPost = do
  ek <- getParameter evaluationKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaulationResultField) "Evaulation result is not found"
  let e = C.Evaulation {
    evaulationResult = evResult er
  , writtenEvaulation = ev
  }
  return $ C.ModifyEvaulation ek e

evaulationContent :: PageData -> Pagelet
evaulationContent pd = onlyHtml $ mkI18NHtml $ \i -> do
  let sd = sbmDesc pd
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
      H.div ! A.id (fieldName evaulationPercentageDiv) $
        translate i . inputEvalResult . eConfig $ sd
      submitButton (fieldName saveEvalBtn) (i "Save Evaluation")
    H.div ! rightText $ do
      textAreaInput (fieldName evaulationValueField) Nothing ! fillDiv
      hiddenKeyField . sbmKey $ pd
  H.div $ H.h2 $ (translate i "Submitted solution")
  H.div ! A.class_ (className assignmentTextDiv) $ H.pre $ do
    (fromString . eSolution $ sd)
  translate i . commentsDiv . eComments $ sd

  where
    defaultEvalCfg :: EvaulationResult
    defaultEvalCfg = BinEval (Binary Passed)

    hiddenKeyField (Left ek)  = hiddenInput (fieldName evaulationKeyField) (paramValue ek)
    hiddenKeyField (Right sk) = hiddenInput (fieldName submissionKeyField) (paramValue sk)

    evPage (Left  _) = P.ModifyEvaulation
    evPage (Right _) = P.Evaulation

inputEvalResult :: EvaulationConfig -> I18NHtml
inputEvalResult (BinEval cfg) = mkI18NHtml $ \i -> do
  valueSelection valueAndText (fieldName evaulationResultField) $
    [(Passed, i "Passed"), (Failed, i "Failed")]
  where
    valueAndText :: (Result, String) -> (String, String)
    valueAndText (v,n) = (errorOnNothing . encodeToFay . EvResult . mkEvalResult . Binary $ v, n)

    errorOnNothing (Just x) = x
    errorOnNothing Nothing  = error "Error in encoding Binary input result"

-- When the page is dynamic the percentage spinner is hooked on the field
inputEvalResult (PctEval cfg) = mkI18NHtml $ \i -> do
  -- TODO: field validation
  hiddenInput (fieldName evaulationResultField) ""

-- CSS Section

formDiv = A.style "width: 100%; height: 200px"
title   = A.style "width: 100%"
leftInfo = A.style "float: left; width: 28%; height: 100%"
rightText = A.style "float: right; width: 68%; height: 100%"
fillDiv = A.style "width: 98%; height: 98%"
