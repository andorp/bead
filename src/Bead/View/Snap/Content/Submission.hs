{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  ) where

import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

submission :: Content
submission = getContentHandler submissionPage

submissionPage :: GETContentHandler
submissionPage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (submissionContent) Nothing

submissionContent :: Html
submissionContent = do
  H.p $ "Solution text box / Solution files"
  H.p $ "Description of the Assignment"
  H.p $ "Course / Group / Teacher / Assignment Information"
  H.p $ "Submit"
