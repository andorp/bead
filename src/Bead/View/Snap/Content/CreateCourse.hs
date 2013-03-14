{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateCourse (
    createCourse
  ) where

import Bead.View.Snap.Content
import qualified Bead.View.UserActions as UA
import qualified Bead.Controller.Pages as P (Page(CreateCourse))


import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

createCourse :: Content
createCourse = getPostContentHandler createPage submitCourse

createPage :: GETContentHandler
createPage = withUserStateAndFrame . const $ do
  postForm (routeOf P.CreateCourse) $ do
    "Create a new course"
    inputPagelet emptyCourse
    submitButton "Create Course"

submitCourse :: POSTContentHandler
submitCourse = do
  course <- getValue
  return . UA.CreateCourse $ course
