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

import qualified Text.Blaze.Html5 as H

evaluationTable :: Content
evaluationTable = getContentHandler evaluationTablePage

evaluationTablePage :: GETContentHandler
evaluationTablePage = withUserState $ \s -> do
  keys <- userStory (openSubmissions)
  renderPagelet $ withUserFrame s (evaluationTableContent keys)

evaluationTableContent :: [(SubmissionKey, SubmissionDesc)] -> IHtml
evaluationTableContent ks = do
  msg <- getI18N
  return $ if (null ks)
    then (fromString . msg $ Msg_EvaluationTable_EmptyUnevaluatedSolutions "Nincsenek nem értékelt megoldások.")
    else do
      H.p $ table "evaluation-table" (className evaluationClassTable) # informationalTable $ do
        H.tr # grayBackground $ do
          H.th (fromString . msg $ Msg_EvaluationTable_Group "Csoport")
          H.th (fromString . msg $ Msg_EvaluationTable_Student "Hallgató")
          H.th (fromString . msg $ Msg_EvaluationTable_Assignment "Feladat")
          H.th (fromString . msg $ Msg_EvaluationTable_Link "Link")
        forM_ ks (submissionInfo msg)

submissionInfo msg (key, desc) = H.tr $ do
  H.td . fromString . eGroup $ desc
  H.td . fromString . eStudent $ desc
  H.td . fromString . eAssignmentTitle $ desc
  H.td $ link (routeOf (P.Evaluation key)) (msg $ Msg_EvaluationTable_Solution "Megoldás")
