{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.EvaluationTable (
    evaluationTable
  ) where

import Control.Monad (liftM)
import Data.String (fromString)

import Bead.Controller.Pages as P (Page(Evaluation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (openSubmissions)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import qualified Bead.View.Snap.I18NHtml as H

evaluationTable :: Content
evaluationTable = getContentHandler evaluationTablePage

evaluationTablePage :: GETContentHandler
evaluationTablePage = withUserState $ \s -> do
  keys <- userStory (openSubmissions)
  renderPagelet $ withUserFrame s (evaluationTableContent keys)

evaluationTableContent :: [(SubmissionKey, SubmissionDesc)] -> Pagelet
evaluationTableContent ks = onlyHtml $
  if (null ks)
    then "Nincsenek nem értékelt megoldások."
    else do
      H.p $ table "evaluation-table" (className evaluationClassTable) # informationalTable $ do
        H.tr # grayBackground $ do
          H.th "Csoport"
          H.th "Hallgató"
          H.th "Feladat"
          H.th "Link"
        forM_ ks submissionInfo

submissionInfo (key, desc) = H.tr $ do
  H.td . fromString . eGroup $ desc
  H.td . fromString . eStudent $ desc
  H.td . fromString . eAssignmentTitle $ desc
  H.td $ link (routeOf (P.Evaluation key)) "Megoldás"
