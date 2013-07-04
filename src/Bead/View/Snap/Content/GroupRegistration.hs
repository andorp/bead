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
import Text.Blaze.Html5 (Html)
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
groupRegistrationPage = withUserStateE $ \s -> do
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
    (translate i "Table of registered courses and teachers of them")
    groupsAlreadyRegistered i (groupsRegistered desc)
  H.p $ do
    (translate i "Course / Group selection")
    groupsForTheUser i (groups desc)
  H.p $ (translate i "Choose")

-- TODO
groupsAlreadyRegistered :: I18N -> [(GroupKey, GroupDesc)] -> Html
groupsAlreadyRegistered i18n ds =
  nonEmpty ds (fromString . i18n $ "There are no attended groups on") $
    return ()

groupsForTheUser :: I18N -> [(GroupKey, GroupDesc)] -> Html
groupsForTheUser i18n gs = nonEmpty gs (fromString . i18n $ "No groups were found") $
  postForm (routeOf P.GroupRegistration) $ do
    selection (fieldName groupRegistrationField) $ do
      mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
    submitButton (fieldName regGroupSubmitBtn) (i18n "Register")

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

