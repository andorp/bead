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

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

groupRegistration :: Content
groupRegistration = getPostContentHandler groupRegistrationPage postGroupReg

data GroupRegData = GroupRegData {
    groups :: [(GroupKey, GroupDesc)]
  , groupsRegistered :: [(GroupKey, GroupDesc)]
  }

postGroupReg :: POSTContentHandler
postGroupReg =
  SubscribeToGroup <$> getParamE (fieldName groupRegistrationField) GroupKey "Group key is not found"

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
  blaze $ withUserFrame s (groupRegistrationContent desc)

groupRegistrationContent :: GroupRegData -> Html
groupRegistrationContent desc = do
  H.p $ do
    "Table of registered courses and teachers of them"
    groupsAlreadyRegistered (groupsRegistered desc)
  H.p $ do
    "Course / Group selection"
    groupsForTheUser (groups desc)
  H.p $ "Choose"

-- TODO
groupsAlreadyRegistered :: [(GroupKey, GroupDesc)] -> Html
groupsAlreadyRegistered ds = return ()
  
groupsForTheUser :: [(GroupKey, GroupDesc)] -> Html
groupsForTheUser gs = do
  postForm (routeOf P.GroupRegistration) $ do
    selection (fieldName groupRegistrationField) $ do
      mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
    submitButton (fieldName regGroupSubmitBtn) "Register"

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

