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

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

courseAdmin :: Content
courseAdmin = getContentHandler courseAdminPage

data PageData = PageData {
    courses     :: [(CourseKey, Course)]
  , groups      :: [(GroupKey, String)]
  , groupAdmins :: [User]
  }

courseAdminPage :: GETContentHandler
courseAdminPage = withUserState $ \s -> do
  pageData <- userStory $ do
    theCourses <- administratedCourses
    courseAndGroupKeys <- forM theCourses $ \(ck,_) -> loadCourse ck
    theGroups <- forM courseAndGroupKeys $ \(c,gkeys) -> do
      grps <- forM gkeys loadGroup
      let courseWithGroups = repeat c `zip` grps
      let expandName x y = courseName x ++ " - " ++ groupName y
      return (gkeys `zip` (map (uncurry expandName) courseWithGroups))
    ps <- selectUsers group_admin
    return PageData {
        courses     = theCourses
      , groups      = concat theGroups
      , groupAdmins = ps
      }
  renderDynamicPagelet $ withUserFrame s (courseAdminContent pageData)
  where
    group_admin = groupAdmin . u_role

courseAdminContent :: PageData -> IHtml
courseAdminContent info = do
  msg <- getI18N
  return $ H.div # textAlign "left" $ do
    H.h3 $ (fromString $ msg $ Msg_CourseAdmin_CreateGroup "New group for the course")
    H.p $ nonEmpty (courses info) (fromString $ msg $ Msg_CourseAdmin_NoCourses "There are no courses.") $
          (postForm (routeOf P.CreateGroup) `withId` (evFormId createGroupHook)) $ do
            H.b $ (fromString $ msg $ Msg_CourseAdmin_Course "Course")
            H.br
            valueTextSelection (fieldName courseKeyInfo) (courses info)
            -- Help message for the percentage
            H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
              (fromString $ msg $ Msg_CourseAdmin_PctHelpMessage "Minimum of percent to achieve by students")
            i18n msg $ inputPagelet emptyGroup
            H.br
            submitButton (fieldName createGroupBtn) (fromString $ msg $ Msg_CourseAdmin_CreateCourse "Create group")
    H.h3 $ (fromString $ msg $ Msg_CourseAdmin_AssignAdmin "Assign teacher to the group")
    H.p $ nonEmpty (groups info) (fromString . msg $ Msg_CourseAdmin_NoGroups "There are no groups.") $
          nonEmpty (groupAdmins info) (fromString . msg $ Msg_CourseAdmin_NoGroupAdmins "There are no teachers.") $
          postForm (routeOf P.AssignGroupAdmin) $ do
            H.table $ do
              (header (fromString . msg $ Msg_CourseAdmin_Group "Group") (fromString . msg $ Msg_CourseAdmin_Admin "Teacher"))
              (selections
                 (valueTextSelection (fieldName selectedGroup) (groups info))
                 (valueTextSelection (fieldName selectedGroupAdmin) (groupAdmins info)))
            H.br
            submitButton (fieldName assignGroupAdminBtn) (msg $ Msg_CourseAdmin_AssignAdmin_Button "Assign")
  where
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
