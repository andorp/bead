{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.EvaulationTable (
    evaulationTable
  ) where

import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

evaulationTable :: Content
evaulationTable = getContentHandler evaulationTablePage

evaulationTablePage :: GETContentHandler
evaulationTablePage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (evaulationTableContent)

evaulationTableContent :: Html
evaulationTableContent = do
  H.p $ "Table of new unevaulated assignements"
