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
import qualified Bead.View.Snap.I18NHtml as H

courseAdmin :: Content
courseAdmin = getContentHandler courseAdminPage

data PageData = PageData {
    courses    :: [(CourseKey, Course)]
  , groups     :: [(GroupKey, Group)]
  , groupAdmins :: [User]
  }

courseAdminPage :: GETContentHandler
courseAdminPage = withUserState $ \s -> do
  pageData <- userStory $ do
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
courseAdminContent info = onlyHtml $ H.div # textAlign "left" $ do
  H.h3 $ "Új csoport létrehozása a tárgyhoz"
  H.p $ nonEmpty (courses info) "Nincsenek tárgyak!" $
        (postForm (routeOf P.CreateGroup) `withId` (evFormId createGroupHook)) $ do
          H.b $ "Tárgy"
          H.br
          valueTextSelection (fieldName courseKeyInfo) (courses info)
          -- Help message for the percentage
          H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
            "A hallgatók által minimálisan teljesítendő százalék"
          inputPagelet emptyGroup
          H.br
          submitButton (fieldName createGroupBtn) "Csoport létrehozása"
  H.h3 $ "Oktató hozzárendelése a csoporthoz"
  H.p $ nonEmpty (groups info) "Nincsenek csoportok!" $
        nonEmpty (groupAdmins info) "Nincsenek oktatók!" $
        postForm (routeOf P.AssignGroupAdmin) $ do
          H.table $ do
            (header "Csoport" "Oktató")
            (selections
               (valueTextSelection (fieldName selectedGroup) (groups info))
               (valueTextSelection (fieldName selectedGroupAdmin) (groupAdmins info)))
          H.br
          submitButton (fieldName assignGroupAdminBtn) "Hozzárendelés"
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
