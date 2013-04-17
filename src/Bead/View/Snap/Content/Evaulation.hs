{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaulation (
    evaulation
  ) where

import Data.String (fromString)
import Control.Monad (liftM)

import Bead.Domain.Relationships (SubmissionDesc(..))
import Bead.Controller.Pages as P(Page(Evaulation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionDescription)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content as C

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

evaulation :: Content
evaulation = getPostContentHandler evaulationPage evaulationPostHandler

data PageData = PageData {
    sbmDesc :: SubmissionDesc
  , sbmKey  :: SubmissionKey
  }

evaulationPage :: GETContentHandler
evaulationPage = withUserStateE $ \s -> do
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key does not found"
  sd <- runStoryE (submissionDescription sk)
  let pageData = PageData {
      sbmKey = sk
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

evaulationContent :: PageData -> Html
evaulationContent pd = do
  let sd = sbmDesc pd
  H.p $ do
    "Information: Course, Group, Student"
    (fromString . eGroup   $ sd)
    (fromString . eStudent $ sd)
  H.p $ postForm (routeOf P.Evaulation) $ do
          H.p $ do
            "Evaulation text block"
            textAreaInput (fieldName evaulationValueField) 300 200 Nothing
          H.p $ do
            "Evaulation checkbox, Submit button"
            -- TODO: Checkbox
            textInput (fieldName evaulationStateField) 10 (Just (show (Passed 10)))
          hiddenInput (fieldName submissionKeyField) (paramValue . sbmKey $ pd)
          submitButton "Save Evaulation"
  H.p $ do
    "Submitted solution"
    (fromString . eSolution $ sd)
