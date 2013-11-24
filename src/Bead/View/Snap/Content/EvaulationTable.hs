{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.EvaulationTable (
    evaulationTable
  ) where

import Control.Monad (liftM)
import Data.String (fromString)

import Bead.Controller.Pages as P (Page(Evaulation))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (openSubmissions)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

evaulationTable :: Content
evaulationTable = getContentHandler evaulationTablePage

evaulationTablePage :: GETContentHandler
evaulationTablePage = withUserState $ \s -> do
  keys <- runStoryE (openSubmissions)
  renderPagelet $ withUserFrame s (evaulationTableContent keys)

evaulationTableContent :: [(SubmissionKey, SubmissionDesc)] -> Pagelet
evaulationTableContent ks = onlyHtml $ mkI18NHtml $ \i -> do
  H.p $ table "evaulation-table" (className evaulationClassTable) # informationalTable $ do
    H.tr # grayBackground $ do
      H.th (fromString $ i "Group")
      H.th (fromString $ i "Student")
      H.th (fromString $ i "Assignment")
      H.th (fromString $ i "Link")
    forM_ ks (submissionInfo i)

submissionInfo i (key, desc) = H.tr $ do
  H.td . fromString . eGroup $ desc
  H.td . fromString . eStudent $ desc
  H.td . fromString . eAssignmentTitle $ desc
  H.td $ link (routeWithParams P.Evaulation [requestParam key]) (i "Submission")
