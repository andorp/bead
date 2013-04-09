{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GroupRegistration (
    groupRegistration
  ) where

import Data.List (intersperse)
import Control.Monad (liftM)

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (availableGroups)
import Bead.Controller.Pages as P (Page(GroupRegistration))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

groupRegistration :: Content
groupRegistration = getPostContentHandler groupRegistrationPage postGroupReg

data GroupRegData = GroupRegData {
    groups :: [(GroupKey, GroupDesc)]
  }

postGroupReg :: POSTContentHandler
postGroupReg =
  SubscribeToGroup <$> getParamE (fieldName groupRegistrationField) GroupKey "Group key is not found"

groupRegistrationPage :: GETContentHandler
groupRegistrationPage = withUserStateE $ \s -> do
  gs <- runStoryE availableGroups
  let c = GroupRegData { groups = gs }
  blaze $ withUserFrame s (groupRegistrationContent c)

groupRegistrationContent :: GroupRegData -> Html
groupRegistrationContent c = do
  H.p $ "Table of registered courses and teachers of them"
  H.p $ do
    "Course / Group selection"
    groupsForTheUser (groups c)
  H.p $ "Choose"

groupsForTheUser :: [(GroupKey, GroupDesc)] -> Html
groupsForTheUser gs = do
  postForm (routeOf P.GroupRegistration) $ do
    selection (fieldName groupRegistrationField) $ do
      mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
    submitButton "Register"

  where
    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

