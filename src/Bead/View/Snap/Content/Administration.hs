{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Administration (
    administration
  , assignCourseAdmin
  ) where

import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (selectCourses, selectUsers)
import           Bead.View.Snap.Content
import           Bead.View.Snap.Fay.Hooks
import qualified Bead.View.UserActions as UA (UserAction(..))

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

administration :: Content
administration = getContentHandler administrationPage

data PageInfo = PageInfo {
    courses      :: [(CourseKey, Course)]
  , admins       :: [User]
  , courseAdmins :: [User]
  }

administrationPage :: GETContentHandler
administrationPage = withUserState $ \s -> do
  (cs,ausers) <- userStory $ do
    cs <- selectCourses each
    ausers <- selectUsers adminOrCourseAdmin
    return (cs,ausers)
  let info = PageInfo {
      courses = cs
    , admins = filter admin ausers
    , courseAdmins = filter courseAdmin ausers
    }
  renderDynamicPagelet $ withUserFrame s (administrationContent info)
  where
    each _ _ = True

    adminOrCourseAdmin u =
      case u_role u of
        Admin -> True
        CourseAdmin -> True
        _ -> False

    admin = (Admin ==) . u_role
    courseAdmin = (CourseAdmin ==) . u_role

administrationContent :: PageInfo -> IHtml
administrationContent info = do
  msg <- getI18N
  return $ do
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_NewCourse "New course")
      (postForm (routeOf createCourse) `withId` (evFormId createCourseHook)) $ do
        i18n msg $ inputPagelet emptyCourse
        -- Help message for the percentage
        H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
          (fromString . msg $ Msg_Administration_PctHelpMessage "Minimum of percent to achieve by students")
        submitButton (fieldName createCourseBtn) (fromString . msg $ Msg_Administration_CreateCourse "Create")
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_AssignCourseAdminTitle "Assign teacher to course")
      let coursesInfo = courses info
      nonEmpty coursesInfo (fromString . msg $ Msg_Administration_NoCourses "There are no courses.") $ do
        nonEmpty (courseAdmins info)
          (do fromString . msg $ Msg_Administration_NoCourseAdmins "There are no teacchers.  Teachers can be created by modifying roles of users."
              H.table $ do
                H.tr $ headerCell (fromString . msg $ Msg_Administration_CreatedCourses "Courses")
                forM_ coursesInfo $ \(_ckey,c) ->
                  H.tr $ dataCell (fromString $ courseName c)
              H.br) $ do
          H.p (fromString . msg $
            Msg_Administration_HowToAddMoreAdmins
            "Further teachers can be added by modifying roles of users, then assign them to courses.")
          postForm (routeOf assignCourseAdmin) $ do
            valueTextSelection (fieldName selectedCourse) (courses info)
            valueTextSelection (fieldName selectedCourseAdmin) (courseAdmins info)
            H.br
            submitButton (fieldName assignBtn) (fromString . msg $ Msg_Administration_AssignCourseAdminButton "Assign")
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_ChangeUserProfile "Modify users")
      getForm (routeOf userDetails) $ do
        i18n msg $ inputPagelet emptyUsername
        submitButton (fieldName selectBtn) (fromString . msg $ Msg_Administration_SelectUser "Select")
  where
    createCourse      = Pages.createCourse ()
    assignCourseAdmin = Pages.assignCourseAdmin ()
    userDetails       = Pages.userDetails ()

    headerCell = H.th # (informationalCell <> grayBackground)
    dataCell   = H.td # informationalCell

-- Add Course Admin

assignCourseAdmin :: Content
assignCourseAdmin = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourseAdmin
  <$> getParameter (customUsernamePrm  (fieldName selectedCourseAdmin))
  <*> getParameter (customCourseKeyPrm (fieldName selectedCourse))
