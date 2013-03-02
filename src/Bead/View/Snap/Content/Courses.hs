{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Courses (
    courses
  ) where

import Bead.Controller.Pages as P (Page(Course))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (selectCourses)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.HandlerUtils

courses :: Content
courses = Content {
    get  = Just coursesPage
  , post = Nothing
  }

coursesPage :: GETContentHandler
coursesPage = withUserState $ \s -> do
  eKeys <- runStory (selectCourses each)
  case eKeys of
    Left err -> error "Error happened: selecting courses"
    Right keys -> do
      let cs = courseKeys (routeOf P.Course) (map fst keys)
      blaze $ withUserFrame s cs Nothing

  where
    each _ _ = True