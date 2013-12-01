{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseAdmin (
    courseAdmin
  , createGroup
  , assignGroupAdmin
  ) where

import Control.Monad (liftM)
import Data.String (fromString)

import Bead.Controller.Pages as P (Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories hiding (createGroup)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Fay.Hooks
import qualified Bead.View.UserActions as UA (UserAction(..))

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

courseAdmin :: Content
courseAdmin = getContentHandler courseAdminPage

data PageData = PageData {
    courses    :: [(CourseKey, Course)]
  , groups     :: [(GroupKey, Group)]
  , groupAdmins :: [User]
  }

courseAdminPage :: GETContentHandler
courseAdminPage = withUserState $ \s -> do
  pageData <- runStoryE $ do
    cs <- administratedCourses
    gs <- do courseAndGroupKeys <- mapM (loadCourse . fst) cs
             let gks = join . map snd $ courseAndGroupKeys
             mapM loadGroup' gks
    ps <- selectUsers group_admin
    return PageData {
        courses    = cs
      , groups     = gs
      , groupAdmins = ps
      }
  renderDynamicPagelet $ withUserFrame s (courseAdminContent pageData)
  where
    group_admin = groupAdmin . u_role

    loadGroup' gk = do
      g <- loadGroup gk
      return (gk,g)

courseAdminContent :: PageData -> Pagelet
courseAdminContent info = onlyHtml $ mkI18NHtml $ \i -> do
  H.h3 (translate i "New group for the course")
  H.p $ nonEmpty (courses info) (translate i "No courses were found") $
        (postForm (routeOf P.CreateGroup) `withId` (evFormId createGroupHook)) $ do
          (translate i "Select a course")
          H.br
          valueTextSelection (fieldName courseKeyInfo) (courses info)
          -- Help message for the percentage
          H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
            fromString (i "The minimum percentage that the students need to reach")
          inputPagelet emptyGroup
          submitButton (fieldName createGroupBtn) (i "Create Group")
  H.h3 (translate i "Assign teacher to the group")
  H.p $ nonEmpty (groups info)      (translate i "No groups were found") $
        nonEmpty (groupAdmins info) (translate i "No group admins were found") $
        postForm (routeOf P.AssignGroupAdmin) $ do
          table
            (header (translate i "Group") (translate i "Group Admin"))
            (selections
               (valueTextSelection (fieldName selectedGroup) (groups info))
               (valueTextSelection (fieldName selectedGroupAdmin) (groupAdmins info)))
          H.br
          submitButton (fieldName assignGroupAdminBtn) (i "Assign")
  where
    table h l = H.table # (centerTable <> border 1 "solid" "black") $ do h; l

    header h1 h2 = H.tr $ do
      H.th h1
      H.th h2

    selections s1 s2 = H.tr $ do
      H.td s1
      H.td s2

-- * Create group

createGroup :: Content
createGroup = postContentHandler submitGroup

submitGroup :: POSTContentHandler
submitGroup = do
  courseKey <- getValue
  group     <- getValue
  return $ UA.CreateGroup courseKey group

-- * Assign GroupAdmin to a group

assignGroupAdmin :: Content
assignGroupAdmin = postContentHandler submitGroupAdmin

submitGroupAdmin :: POSTContentHandler
submitGroupAdmin = UA.CreateGroupAdmin
  <$> getParameter (customUsernamePrm (fieldName selectedGroupAdmin))
  <*> getParameter (customGroupKeyPrm (fieldName selectedGroup))
