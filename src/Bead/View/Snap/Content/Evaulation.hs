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
import qualified Text.Blaze.Html5.Attributes as A (id)

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
evaulationPage = withUserStateE $ \s -> do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key is not found"
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
      sbmKey  = Right sk
    , sbmDesc = sd
    }
  render (eConfig sd) $ withUserFrame s (evaulationContent pageData)

modifyEvaulationPage :: GETContentHandler
modifyEvaulationPage = withUserStateE $ \s -> do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key is not found"
  ek <- getParamE (fieldName evaulationKeyField) EvaulationKey "Evaulation kes is not found"
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
    sbmKey  = Left ek
  , sbmDesc = sd
  }
  render (eConfig sd) $ withUserFrame s (evaulationContent pageData)

evaulationPostHandler :: POSTContentHandler
evaulationPostHandler = do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key is not found"
  ev <- getParamE (fieldName evaulationValueField) id "Evaulation value is not found"
  er <- getJSONParam (fieldName evaulationResultField) "Evaulation result is not found"
  let e = C.Evaulation {
    evaulationResult = evResult er
  , writtenEvaulation = ev
  }
  return $ NewEvaulation sk e

modifyEvaulationPost :: POSTContentHandler
modifyEvaulationPost = do
  ek <- getParamE (fieldName evaulationKeyField) EvaulationKey "Evaulation key is not found"
  ev <- getParamE (fieldName evaulationValueField) id "Evaulation value is not found"
  er <- getJSONParam (fieldName evaulationResultField) "Evaulation result is not found"
  let e = C.Evaulation {
    evaulationResult = evResult er
  , writtenEvaulation = ev
  }
  return $ C.ModifyEvaulation ek e

evaulationContent :: PageData -> Pagelet
evaulationContent pd = onlyHtml $ mkI18NHtml $ \i -> do
  let sd = sbmDesc pd
  H.p $ do
    (translate i "Information: Course, Group, Student")
    (fromString . eGroup   $ sd)
    (fromString . eStudent $ sd)
  H.p $ postForm (routeOf . evPage . sbmKey $ pd) $ do
          H.p $ do
            (translate i "Evaulation text block")
            textAreaInput (fieldName evaulationValueField) Nothing
          H.p $ do
            (translate i "Evaulation checkbox, Submit button")
            -- TODO: Checkbox
          hiddenKeyField . sbmKey $ pd
          H.div ! A.id (fieldName evaulationPercentageDiv) $ do
            translate i . inputEvalResult . eConfig $ sd
            submitButton (fieldName saveEvalBtn) (i "Save Evaulation")
  H.p $ do
    (translate i "Submitted solution")
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
