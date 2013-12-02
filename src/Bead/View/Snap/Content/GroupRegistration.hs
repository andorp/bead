{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GroupRegistration (
    groupRegistration
  ) where

import Data.List (intersperse)
import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (availableGroups, attendedGroups)
import Bead.Controller.Pages as P (Page(GroupRegistration))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Data.String (fromString)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H

groupRegistration :: Content
groupRegistration = getPostContentHandler groupRegistrationPage postGroupReg

data GroupRegData = GroupRegData {
    groups :: [(GroupKey, GroupDesc)]
  , groupsRegistered :: [(GroupKey, GroupDesc)]
  }

postGroupReg :: POSTContentHandler
postGroupReg = SubscribeToGroup
  <$> getParameter (customGroupKeyPrm (fieldName groupRegistrationField))

groupRegistrationPage :: GETContentHandler
groupRegistrationPage = withUserState $ \s -> do
  desc <- runStoryE $ do
    as <- attendedGroups
    let attendedGroupKeys = map fst as
        newGroupForUser (gk,_) = not (elem gk attendedGroupKeys)
    gs <- (filter newGroupForUser) <$> availableGroups
    return GroupRegData { 
        groups = gs
      , groupsRegistered = as
      }
  renderPagelet $ withUserFrame s (groupRegistrationContent desc)

groupRegistrationContent :: GroupRegData -> Pagelet
groupRegistrationContent desc = onlyHtml $ mkI18NHtml $ \i -> do
  H.p $ do
    H.h3 $ (translate i "Felvett csoportok")
    groupsAlreadyRegistered i (groupsRegistered desc)
  H.p $ do
    H.h3 $ (translate i "Tárgy és csoport kiválasztása")
    groupsForTheUser i (groups desc)

groupsAlreadyRegistered :: I18N -> [(GroupKey, GroupDesc)] -> Html
groupsAlreadyRegistered i18n ds =
  nonEmpty ds
    (fromString . i18n $ "Nincsenek felvett tárgyak.")
    (H.table # informationalTable $ do
      H.tr $ do
        H.th # (grayBackground <> informationalCell) $ fromString $ i18n $ "Csoportok"
        H.th # (grayBackground <> informationalCell) $ fromString $ i18n $ "Oktatók"
      mapM_ (groupLine . snd) ds)
  where
    groupLine = groupDescFold $ \n as -> do
      H.tr $ do
        H.td # informationalCell $ fromString $ n
        H.td # informationalCell $ fromString $ join $ intersperse " " as

groupsForTheUser :: I18N -> [(GroupKey, GroupDesc)] -> Html
groupsForTheUser i18n gs = nonEmpty gs (fromString . i18n $ "Nincsenek elérhető csoportok.") $
  postForm (routeOf P.GroupRegistration) $ do
    selection (fieldName groupRegistrationField) $ do
      mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
    H.br
    submitButton (fieldName regGroupSubmitBtn) (i18n "Felvesz")

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

