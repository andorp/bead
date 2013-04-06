{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseAdmin (
    courseAdmin
  , createGroup
  , assignProfessor
  ) where

import Control.Monad (liftM)

import Bead.Controller.Pages as P (Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories hiding (createGroup)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import qualified Bead.View.UserActions as UA (UserAction(..))

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

courseAdmin :: Content
courseAdmin = getContentHandler courseAdminPage

data PageData = PageData {
    courses    :: [(CourseKey, Course)]
  , professors :: [User]
  , groups     :: [(GroupKey, Group)]
  }

courseAdminPage :: GETContentHandler
courseAdminPage = withUserStateE $ \s -> do
  cs <- runStoryE administratedCourses
  gs <- runStoryE $ do
          courseAndGroupKeys <- mapM (loadCourse . fst) cs
          let gks = join . map snd $ courseAndGroupKeys
          mapM loadGroup' gks
  ps <- runStoryE (selectUsers professor)
  let pageData = PageData {
      courses    = cs
    , professors = ps
    , groups     = gs
    }
  blaze $ withUserFrame s (courseAdminContent pageData)
  where
    professor = (Professor ==) . u_role

    loadGroup' gk = do
      g <- loadGroup gk
      return (gk,g)

courseAdminContent :: PageData -> Html
courseAdminContent info = do
  H.p $ "New group for the course"
  H.p $ postForm (routeOf P.CreateGroup) $ do
          valueTextSelection (fieldName courseKeyInfo) (courses info)
          inputPagelet emptyGroup
          submitButton "Create Group"
  H.p $ "Assign teacher to the group"
  H.p $ postForm (routeOf P.AssignProfessor) $ do
          valueTextSelection (fieldName selectedGroup) (groups info)
          valueTextSelection (fieldName selectedProfessor) (professors info)
          submitButton "Assign"

-- * Create group

createGroup :: Content
createGroup = postContentHandler submitGroup

submitGroup :: POSTContentHandler
submitGroup = do
  courseKey <- getValue
  group     <- getValue
  return $ UA.CreateGroup courseKey group

-- * Assign Professor to a group

assignProfessor :: Content
assignProfessor = postContentHandler submitProfessor

submitProfessor :: POSTContentHandler
submitProfessor = do
  username <- getParamE (fieldName selectedProfessor) Username "Professor was not found"
  groupKey <- getParamE (fieldName selectedGroup) GroupKey "Group key was not found"
  return $ UA.CreateProfessor username groupKey
