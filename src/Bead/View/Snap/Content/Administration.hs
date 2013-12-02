{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Administration (
    administration
  , assignCourseAdmin
  ) where

import Control.Monad (liftM)
import Data.String (fromString)

import Bead.Domain.Entities (User(..), Role(..))
import Bead.Controller.Pages as P (Page(CreateCourse, UserDetails, AssignCourseAdmin))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (selectCourses, selectUsers)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Fay.Hooks
import qualified Bead.View.UserActions as UA (UserAction(..))

import Text.Blaze.Html5 (Html, (!))
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
    H.h3 $ (translate i18n "Új tárgy")
    (postForm (routeOf P.CreateCourse) `withId` (evFormId createCourseHook)) $ do
      inputPagelet emptyCourse
      -- Help message for the percentage
      H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $ fromString
        (i18n "A hallgatók által minimálisan teljesítendő százalék")
      submitButton (fieldName createCourseBtn) (i18n "Létrehozás")
  H.div $ do
    H.h3 $ (translate i18n "Oktató hozzárendelése a tárgyhoz")
    nonEmpty (courses info) (translate i18n "Nincsenek tárgyak!") $
      nonEmpty (courseAdmins info) (translate i18n "Nincsenek oktatók!") $
        postForm (routeOf P.AssignCourseAdmin) $ do
          valueTextSelection (fieldName selectedCourse) (courses info)
          valueTextSelection (fieldName selectedCourseAdmin) (courseAdmins info)
          H.br
          submitButton (fieldName assignBtn) (i18n "Hozzárendelés")
  H.div $ do
    H.h3 $ (translate i18n "Felhasználó adatainak módosítása")
    getForm (routeOf P.UserDetails) $ do
      inputPagelet emptyUsername
      submitButton (fieldName selectBtn) (i18n "Kiválasztás")

-- Add Course Admin

assignCourseAdmin :: Content
assignCourseAdmin = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourseAdmin
  <$> getParameter (customUsernamePrm  (fieldName selectedCourseAdmin))
  <*> getParameter (customCourseKeyPrm (fieldName selectedCourse))
