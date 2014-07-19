{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Administration (
    administration
  , assignCourseAdmin
  , createCourse
  ) where

import           Control.Arrow ((***), (&&&))
import           Data.Function (on)
import           Data.List
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories hiding (createCourse)
import           Bead.Domain.Types (str)
import           Bead.View.Snap.Content
import           Bead.View.Snap.Fay.Hooks
import qualified Bead.View.UserActions as UA (UserAction(..))

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

administration = ViewHandler administrationPage

data PageInfo = PageInfo {
    courses      :: [(CourseKey, Course)]
  , admins       :: [User]
  , courseAdmins :: [User]
  , assignedCourseAdmins :: [(Course, [User])]
  }

administrationPage :: GETContentHandler
administrationPage = withUserState $ \s -> do
  (cs,ausers,assigned) <- userStory $ do
    cs <- selectCourses each
    ausers <- selectUsers adminOrCourseAdmin
    assigned <- courseAdministrators
    return (cs,ausers,assigned)
  let info = PageInfo {
      courses = cs
    , admins = filter admin ausers
    , courseAdmins = filter courseAdmin ausers
    , assignedCourseAdmins = assigned
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
      (postForm (routeOf createCourse) {-`withId` (evFormId createCourseHook)-}) $ do
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
            selection (fieldName selectedCourse) courses'
            selection (fieldName selectedCourseAdmin) courseAdmins'
            H.br
            submitButton (fieldName assignBtn) (fromString . msg $ Msg_Administration_AssignCourseAdminButton "Assign")
      courseAdministratorsTable msg (assignedCourseAdmins info)
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_ChangeUserProfile "Modify users")
      getForm (routeOf userDetails) $ do
        i18n msg $ inputPagelet emptyUsername
        submitButton (fieldName selectBtn) (fromString . msg $ Msg_Administration_SelectUser "Select")
  where
    userLongname u = concat [ str $ u_username u, " - ", u_name u ]
    courses' = map (id *** courseName) $ courses info
    courseAdmins' = map (u_username &&& userLongname) $ courseAdmins info

    createCourse      = Pages.createCourse ()
    assignCourseAdmin = Pages.assignCourseAdmin ()
    userDetails       = Pages.userDetails ()

    headerCell = H.th # (informationalCell <> grayBackground)
    dataCell   = H.td # informationalCell

courseAdministratorsTable :: I18N -> [(Course,[User])] -> H.Html
courseAdministratorsTable _ [] = return ()
courseAdministratorsTable i18n courses = do
  H.p . fromString . i18n . Msg_Administration_CourseAdmins_Info $ unlines
    [ "Each line of the following table contains a course and the username of the administrators,"
    , " that are assigned to the course."
    ]
  let courses' = sortBy (compare `on` (courseName . fst)) courses
  H.p $ table (fieldName courseAdministratorsTableName) (fieldName courseAdministratorsTableName)
      # informationalTable $ do
    H.tr # grayBackground $ do
      H.th (fromString . i18n $ Msg_Administration_CourseAdmins_Course "Course")
      H.th (fromString . i18n $ Msg_Administration_CourseAdmins_Admins "Administrators")
    forM_ courses' $ \(course, admins) -> do
      H.tr $ do
        H.td . fromString $ courseName course
        let admins' = sort $ map (usernameCata id . u_username) admins
        H.td . fromString . concat $ intersperse ", " admins'

-- Add Course Admin

assignCourseAdmin :: ModifyHandler
assignCourseAdmin = ModifyHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourseAdmin
  <$> getParameter (jsonUsernamePrm  (fieldName selectedCourseAdmin))
  <*> getParameter (jsonCourseKeyPrm (fieldName selectedCourse))

-- Create Course

createCourse :: ModifyHandler
createCourse = ModifyHandler $ UA.CreateCourse <$> getValue
