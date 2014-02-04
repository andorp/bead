{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GroupRegistration (
    groupRegistration
  , unsubscribeFromCourse
  ) where

import Data.List (intersperse)
import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (availableGroups, attendedGroups)
import qualified Bead.Controller.Pages as P (Page(GroupRegistration, UnsubscribeFromCourse))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Data.String (fromString)
import Text.Blaze.Html5 ((!))
import Bead.View.Snap.I18N (IHtml)
import qualified Text.Blaze.Html5 as H

groupRegistration :: Content
groupRegistration = getPostContentHandler groupRegistrationPage postGroupReg

unsubscribeFromCourse :: Content
unsubscribeFromCourse =
  postContentHandler (UnsubscribeFromCourse <$> getParameter unsubscribeUserGroupKeyPrm)

data GroupRegData = GroupRegData {
    groups :: [(GroupKey, GroupDesc)]
  , groupsRegistered :: [(GroupKey, GroupDesc, Bool)]
  }

postGroupReg :: POSTContentHandler
postGroupReg = SubscribeToGroup
  <$> getParameter (customGroupKeyPrm (fieldName groupRegistrationField))

groupRegistrationPage :: GETContentHandler
groupRegistrationPage = withUserState $ \s -> do
  desc <- userStory $ do
    as <- attendedGroups
    let attendedGroupKeys = map fst3 as
        newGroupForUser (gk,_) = not (elem gk attendedGroupKeys)
    gs <- (filter newGroupForUser) <$> availableGroups
    return GroupRegData {
        groups = gs
      , groupsRegistered = as
      }
  renderPagelet $ withUserFrame s (groupRegistrationContent desc)
  where
    fst3 (f,_,_) = f

groupRegistrationContent :: GroupRegData -> IHtml
groupRegistrationContent desc = do
  msg <- getI18N
  return $ do
    H.p $ do
      H.h3 $ (fromString . msg $ Msg_GroupRegistration_RegisteredCourses "Felvett csoportok")
      i18n msg $ groupsAlreadyRegistered (groupsRegistered desc)
    H.p $ do
      H.h3 $ (fromString . msg $ Msg_GroupRegistration_SelectGroup "Tárgy és csoport kiválasztása")
      i18n msg $ groupsForTheUser (groups desc)

groupsAlreadyRegistered :: [(GroupKey, GroupDesc, Bool)] -> IHtml
groupsAlreadyRegistered ds = do
  msg <- getI18N
  return $ nonEmpty ds
    (fromString . msg $ Msg_GroupRegistration_NoRegisteredCourses "Nincsenek felvett tárgyak.")
    (H.table # informationalTable $ do
      H.tr $ do
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Courses "Csoportok")
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Admins "Oktatók")
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Unsubscribe "Leiratkozás")
      mapM_ (groupLine msg) ds)
  where
    groupLine msg (key, desc, hasSubmission) = flip groupDescFold desc $ \n as -> do
      H.tr $ do
        H.td # informationalCell $ fromString $ n
        H.td # informationalCell $ fromString $ join $ intersperse " " as
        H.td # informationalCell $
          if hasSubmission
            then (fromString . msg $ Msg_GroupRegistration_NoUnsubscriptionAvailable "Már van beadott megoldásod")
            else postForm (routeOf $ P.UnsubscribeFromCourse key) $
                   submitButton
                     (fieldName unsubscribeFromCourseSubmitBtn)
                     (msg $ Msg_GroupRegistration_Unsubscribe "Leiratkozás")

groupsForTheUser :: [(GroupKey, GroupDesc)] -> IHtml
groupsForTheUser gs = do
  msg <- getI18N
  return $ nonEmpty gs (fromString . msg $ Msg_GroupRegistration_NoAvailableCourses "Nincsenek elérhető csoportok.") $
    postForm (routeOf P.GroupRegistration) $ do
      selection (fieldName groupRegistrationField) $ do
        mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
      H.br
      submitButton (fieldName regGroupSubmitBtn) (msg $ Msg_GroupRegistration_Register "Felvesz")

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

