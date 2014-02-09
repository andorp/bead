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

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
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
      H.h3 $ (fromString . msg $ Msg_Administration_NewCourse "Új tárgy")
      (postForm (routeOf P.CreateCourse) `withId` (evFormId createCourseHook)) $ do
        i18n msg $ inputPagelet emptyCourse
        -- Help message for the percentage
        H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
          (fromString . msg $ Msg_Administration_PctHelpMessage "A hallgatók által minimálisan teljesítendő százalék")
        submitButton (fieldName createCourseBtn) (fromString . msg $ Msg_Administration_CreateCourse "Létrehozás")
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_AssignCourseAdminTitle "Oktató hozzárendelése a tárgyhoz")
      let coursesInfo = courses info
      nonEmpty coursesInfo (fromString . msg $ Msg_Administration_NoCourses "Nincsenek tárgyak!") $ do
        nonEmpty (courseAdmins info)
          (do fromString . msg $ Msg_Administration_NoCourseAdmins "Nincsenek oktatók! Oktatókat a regisztrált felhasználók adatainak módosításával lehet létrehozni."
              H.table $ do
                H.tr $ headerCell (fromString . msg $ Msg_Administration_CreatedCourses "Tárgyak")
                forM_ coursesInfo $ \(_ckey,c) ->
                  H.tr $ dataCell (fromString $ courseName c)
              H.br) $ do
          H.p (fromString . msg $
            Msg_Administration_HowToAddMoreAdmins
            "További oktatókat a felhasználói adatok módosításával lehet létrehozni, majd ezt követően tudjuk őket a tárgyakhoz rendelni.")
          postForm (routeOf P.AssignCourseAdmin) $ do
            valueTextSelection (fieldName selectedCourse) (courses info)
            valueTextSelection (fieldName selectedCourseAdmin) (courseAdmins info)
            H.br
            submitButton (fieldName assignBtn) (fromString . msg $ Msg_Administration_AssignCourseAdminButton "Hozzárendelés")
    H.div $ do
      H.h3 $ (fromString . msg $ Msg_Administration_ChangeUserProfile "Felhasználó adatainak módosítása")
      getForm (routeOf P.UserDetails) $ do
        i18n msg $ inputPagelet emptyUsername
        submitButton (fieldName selectBtn) (fromString . msg $ Msg_Administration_SelectUser "Kiválasztás")
  where
    headerCell = H.th # (informationalCell <> grayBackground)
    dataCell   = H.td # informationalCell

-- Add Course Admin

assignCourseAdmin :: Content
assignCourseAdmin = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourseAdmin
  <$> getParameter (customUsernamePrm  (fieldName selectedCourseAdmin))
  <*> getParameter (customCourseKeyPrm (fieldName selectedCourse))
