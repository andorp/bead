{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Administration (
    administration
  , assignCourseAdmin
  ) where

import Control.Monad (liftM)

import Bead.Domain.Entities (User(..), Role(..))
import Bead.Controller.Pages as P (Page(CreateCourse, UserDetails, AssignCourseAdmin))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (selectCourses, selectUsers)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Fay.Hooks
import qualified Bead.View.UserActions as UA (UserAction(..))

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

administration :: Content
administration = getContentHandler administrationPage

data PageInfo = PageInfo {
    courses      :: [(CourseKey, Course)]
  , admins       :: [User]
  , courseAdmins :: [User]
  }

administrationPage :: GETContentHandler
administrationPage = withUserState $ \s -> do
  cs <- runStoryE (selectCourses each)
  ausers <- runStoryE (selectUsers adminOrCourseAdmin)
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

administrationContent :: PageInfo -> Pagelet
administrationContent info = onlyHtml $ mkI18NHtml $ \i18n -> do
  H.div $ do
    H.h3 $ (translate i18n "New Course")
    (postForm (routeOf P.CreateCourse) `withId` (evFormId createCourseHook)) $ do
      inputPagelet emptyCourse
      submitButton (fieldName createCourseBtn) (i18n "Create Course")
  H.div $ do
    H.h3 $ (translate i18n "Add course admin to the course")
    nonEmpty (courses info)      (translate i18n "No courses were found") $
      nonEmpty (courseAdmins info) (translate i18n "No course admins were found") $
        postForm (routeOf P.AssignCourseAdmin) $ do
          valueTextSelection (fieldName selectedCourse) (courses info)
          valueTextSelection (fieldName selectedCourseAdmin) (courseAdmins info)
          H.br
          submitButton (fieldName assignBtn) (i18n "Assign")
  H.div $ do
    H.h3 $ (translate i18n "Modify user's account")
    getForm (routeOf P.UserDetails) $ do
      inputPagelet emptyUsername
      submitButton (fieldName selectBtn) (i18n "Select")

-- Add Course Admin

assignCourseAdmin :: Content
assignCourseAdmin = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourseAdmin
  <$> getParameter (customUsernamePrm  (fieldName selectedCourseAdmin))
  <*> getParameter (customCourseKeyPrm (fieldName selectedCourse))
