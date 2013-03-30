{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Control.Monad (when, liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

home :: Content
home = getContentHandler homePage

homePage :: GETContentHandler
homePage = withUserStateE $ \s -> do
  blaze $ withUserFrame s (homeContent s) Nothing

homeContent :: UserState -> Html
homeContent s = do
  when (isAdmin s) $ H.p $ "Admin's menu"
  when (isCourseAdmin s) $ H.p $ do
    "Course Admin's menu"
    linkToPage P.NewCourseAssignment
  when (isProfessor s) $ H.p $ do
    "Teacher's menu"
    linkToPage P.NewGroupAssignment
  when (isStudent s) $ H.p $ "Student's menu"

