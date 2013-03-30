{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseRegistration (
    courseRegistration
  ) where

import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

courseRegistration :: Content
courseRegistration = getContentHandler courseRegistrationPage

courseRegistrationPage :: GETContentHandler
courseRegistrationPage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (courseRegistrationContent) Nothing

courseRegistrationContent :: Html
courseRegistrationContent = do
  H.p $ "Table of registered courses and teachers of them"
  H.p $ "Course / Group selection"
  H.p $ "Choose"
