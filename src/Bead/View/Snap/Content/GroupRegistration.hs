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
import Text.Blaze.Html5 ((!))
import Bead.View.Snap.I18N (IHtml, constant)
import qualified Bead.View.Snap.I18NHtml as H

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
  desc <- userStory $ do
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
groupRegistrationContent desc = onlyHtml $ do
  H.p $ do
    H.h3 $ "Felvett csoportok"
    groupsAlreadyRegistered (groupsRegistered desc)
  H.p $ do
    H.h3 $ "Tárgy és csoport kiválasztása"
    groupsForTheUser (groups desc)

groupsAlreadyRegistered :: [(GroupKey, GroupDesc)] -> IHtml
groupsAlreadyRegistered ds =
  nonEmpty ds
    ("Nincsenek felvett tárgyak.")
    (H.table # informationalTable $ do
      H.tr $ do
        H.th # (grayBackground <> informationalCell) $ "Csoportok"
        H.th # (grayBackground <> informationalCell) $ "Oktatók"
      mapM_ (groupLine . snd) ds)
  where
    groupLine = groupDescFold $ \n as -> do
      H.tr $ do
        H.td # informationalCell $ constant n
        H.td # informationalCell $ constant $ join $ intersperse " " as

groupsForTheUser :: [(GroupKey, GroupDesc)] -> IHtml
groupsForTheUser gs = nonEmpty gs ("Nincsenek elérhető csoportok.") $
  postForm (routeOf P.GroupRegistration) $ do
    selection (fieldName groupRegistrationField) $ do
      mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
    H.br
    submitButton (fieldName regGroupSubmitBtn) ("Felvesz")

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

