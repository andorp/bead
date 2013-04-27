{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaulation (
    evaulation
  , modifyEvaulation
  ) where

import Data.String (fromString)
import Control.Monad (liftM)

import Bead.Domain.Relationships (SubmissionDesc(..))
import Bead.Controller.Pages as P(Page(Evaulation, ModifyEvaulation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionDescription)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content as C

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

evaulation :: Content
evaulation = getPostContentHandler evaulationPage evaulationPostHandler

modifyEvaulation :: Content
modifyEvaulation = getPostContentHandler modifyEvaulationPage modifyEvaulationPost

data PageData = PageData {
    sbmDesc :: SubmissionDesc
  , sbmKey  :: Either EvaulationKey SubmissionKey
  }

evaulationPage :: GETContentHandler
evaulationPage = withUserStateE $ \s -> do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key does not found"
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
      sbmKey  = Right sk
    , sbmDesc = sd
    }
  blaze $ withUserFrame s (evaulationContent pageData)

modifyEvaulationPage :: GETContentHandler
modifyEvaulationPage = withUserStateE $ \s -> do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key does not found"
  ek <- getParamE (fieldName evaulationKeyField) EvaulationKey "Evaulation kes does not found"
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
    sbmKey  = Left ek
  , sbmDesc = sd
  }
  blaze $ withUserFrame s (evaulationContent pageData)

evaulationPostHandler :: POSTContentHandler
evaulationPostHandler = do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key does not found"
  ev <- getParamE (fieldName evaulationValueField) id "Evaulation value does not found"
  es <- getParamE (fieldName evaulationStateField) read "Evaulation state does nof found"
  let e = C.Evaulation {
    evaulationState = es
  , writtenEvaulation = ev
  }
  return $ NewEvaulation sk e

modifyEvaulationPost :: POSTContentHandler
modifyEvaulationPost = do
  ek <- getParamE (fieldName evaulationKeyField) EvaulationKey "Evaulation key does not found"
  ev <- getParamE (fieldName evaulationValueField) id "Evaulation value does not found"
  es <- getParamE (fieldName evaulationStateField) read "Evaulation state does nof found"
  let e = C.Evaulation {
    evaulationState = es
  , writtenEvaulation = ev
  }
  return $ C.ModifyEvaulation ek e

evaulationContent :: PageData -> Html
evaulationContent pd = do
  let sd = sbmDesc pd
  H.p $ do
    "Information: Course, Group, Student"
    (fromString . eGroup   $ sd)
    (fromString . eStudent $ sd)
  H.p $ postForm (routeOf . evPage . sbmKey $ pd) $ do
          H.p $ do
            "Evaulation text block"
            textAreaInput (fieldName evaulationValueField) 300 200 Nothing
          H.p $ do
            "Evaulation checkbox, Submit button"
            -- TODO: Checkbox
            textInput (fieldName evaulationStateField) 10 (Just (show (Passed 10)))
          hiddenKeyField . sbmKey $ pd
          submitButton (fieldName saveEvalBtn) "Save Evaulation"
  H.p $ do
    "Submitted solution"
    (fromString . eSolution $ sd)

  where
    hiddenKeyField (Left ek)  = hiddenInput (fieldName evaulationKeyField) (paramValue ek)
    hiddenKeyField (Right sk) = hiddenInput (fieldName submissionKeyField) (paramValue sk)

    evPage (Left  _) = P.ModifyEvaulation
    evPage (Right _) = P.Evaulation
