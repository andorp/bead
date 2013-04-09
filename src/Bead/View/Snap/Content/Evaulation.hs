{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaulation (
    evaulation
  ) where

import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content hiding (evaulation)

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

evaulation :: Content
evaulation = getContentHandler evaulationPage

evaulationPage :: GETContentHandler
evaulationPage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (evaulationContent)

evaulationContent :: Html
evaulationContent = do
  H.p $ "Information: Course, Group, Student"
  H.p $ "Evaulation text block"
  H.p $ "Evaulation checkbox, Submit button"
  H.p $ "Submitted solution"
