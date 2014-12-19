{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseAdmin.Page (
    courseAdmin
  , createGroup
  , assignGroupAdmin
  ) where

import           Control.Arrow ((***), (&&&))
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories hiding (createGroup)
import           Bead.View.Snap.Content hiding (table, option)
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import qualified Bead.View.UserActions as UA (UserAction(..))

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

courseAdmin = ViewHandler courseAdminPage

data PageData = PageData {
    courses     :: [(CourseKey, Course)]
  , groups      :: [(GroupKey, String)]
  , groupAdmins :: [User]
  , assignedGroups :: [(Course,[(Group,[User])])]
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
      return (gkeys `zip` (Prelude.map (uncurry expandName) courseWithGroups))
    ps <- selectUsers group_admin
    gs <- groupAdministrators
    return PageData {
        courses     = theCourses
      , groups      = concat theGroups
      , groupAdmins = ps
      , assignedGroups = gs
      }
  renderBootstrapPage . bootstrapUserFrame s $ courseAdminContent pageData
  where
    group_admin = groupAdmin . u_role


-- * Create group

createGroup :: ModifyHandler
createGroup = ModifyHandler submitGroup

submitGroup :: POSTContentHandler
submitGroup = UA.CreateGroup
  <$> getParameter (jsonCourseKeyPrm (fieldName courseKeyInfo))
  <*> getGroup

-- * Assign GroupAdmin to a group

assignGroupAdmin :: ModifyHandler
assignGroupAdmin = ModifyHandler submitGroupAdmin

submitGroupAdmin :: POSTContentHandler
submitGroupAdmin = UA.CreateGroupAdmin
  <$> getParameter (jsonUsernamePrm (fieldName selectedGroupAdmin))
  <*> getParameter (jsonGroupKeyPrm (fieldName selectedGroup))

-- * View

groupAdministratorsTable :: I18N -> [(Course, [(Group, [User])])] -> H.Html
groupAdministratorsTable _ [] = return ()
groupAdministratorsTable i18n cgroups = do
  let cgroups' = sortBy (compare `on` (courseName . fst)) cgroups
  forM_ cgroups $ \(course, groups) -> when (not $ null groups) $ do
    let groups' = sortBy (compare `on` (groupName . fst)) groups
        cname   = courseName course
    Bootstrap.row $ Bootstrap.colMd12  $ Bootstrap.table $ do
      H.thead $ tr $ H.th $ fromString cname
      H.thead $ tr $ do
        H.th (fromString . i18n $ Msg_CourseAdmin_GroupAdmins_Group "Group")
        H.th (fromString . i18n $ Msg_CourseAdmin_GroupAdmins_Admins "Group Admins")
      H.tbody $ forM_ groups' $ \(group, admins) -> do
        H.tr $ do
          H.td (fromString $ groupName group)
          H.td (fromString . concat . intersperse ", " $ Prelude.map (usernameCata Prelude.id . u_username) admins)

courseAdminContent :: PageData -> IHtml
courseAdminContent info = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ do
      -- New Group for the course
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ Msg_CourseAdmin_CreateGroup "New group for the course")
        nonEmpty (courses info) (H.p $ fromString $ msg $ Msg_CourseAdmin_NoCourses "There are no courses.") $
          postForm (routeOf createGroup) $ do
            -- Hidden message
            H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
              (fromString $ msg $ Msg_CourseAdmin_PctHelpMessage "Minimum of percent to achieve by students")
            Bootstrap.selection (fieldName courseKeyInfo) (const False) courses'
            Bootstrap.textInput (fieldName groupNameField) (msg $ Msg_Input_Group_Name "Title") ""
            Bootstrap.textInput (fieldName groupDescField) (msg $ Msg_Input_Group_Description "Description") ""
            Bootstrap.submitButton (fieldName createGroupBtn) (fromString $ msg $ Msg_CourseAdmin_CreateCourse "Create group")
            hr
      -- Assign teacher to the group
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ Msg_CourseAdmin_AssignAdmin "Assign teacher to the group")
        nonEmpty (groups info) (H.p $ fromString . msg $ Msg_CourseAdmin_NoGroups "There are no groups.") $
          nonEmpty (groupAdmins info) (H.p $ fromString . msg $ Msg_CourseAdmin_NoGroupAdmins "There are no teachers.") $
          postForm (routeOf assignGroupAdmin) $ do
            Bootstrap.selection (fieldName selectedGroup) (const False) groups'
            Bootstrap.selection (fieldName selectedGroupAdmin) (const False) groupAdmins'
            Bootstrap.submitButton (fieldName assignGroupAdminBtn) (fromString $ msg $ Msg_CourseAdmin_AssignAdmin_Button "Assign")

    Bootstrap.row $ Bootstrap.colMd12 $ hr
    Bootstrap.row $ Bootstrap.colMd12 $ p $
      fromString $ msg $ Msg_CourseAdmin_GroupAdmins_Info
        "The following table(s) contain(s) the course related groups and the username of the group admins."

    -- Group Administrators table
    groupAdministratorsTable msg (assignedGroups info)

    Bootstrap.turnSelectionsOn
  where
    courses' = Prelude.map (Prelude.id *** courseName) (courses info)
    groups' = (groups info)
    groupAdmins' = Prelude.map (u_username &&& userLongname) (groupAdmins info)

    userLongname u = concat [ usernameCata Prelude.id $ u_username u, " - ", u_name u]

    createGroup = Pages.createGroup ()
    assignGroupAdmin = Pages.assignGroupAdmin ()

getGroup = Group
  <$> getParameter (stringParameter (fieldName groupNameField) "Csoport neve")
  <*> getParameter (stringParameter (fieldName groupDescField) "Csoport leírása")
