{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Group (
    group
  ) where

import Control.Monad.Error (Error(..), throwError)

import Bead.View.Snap.Content hiding (Group(..))
import Bead.Controller.Pages (Page(Group))
import Bead.Controller.UserStories (isUserInGroup)
import Bead.Domain.Relationships (CourseKey(..))
import Data.String

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

group :: Content
group = Content {
    get  = Just groupPageHandler
  , post = Just subscriptionHandler
  }

groupPageHandler :: GETContentHandler
groupPageHandler = withUserStateE $ \s -> do
  cKey <- liftM (CourseKey . fromString . unpack) $ getParamE (fieldName courseKeyInfo)
  gKey <- liftM (GroupKey . fromString . unpack)  $ getParamE (fieldName groupKeyName)
  isSubscribed <- runStoryE . isUserInGroup $ gKey
  lift $ blaze $ withUserFrame s (groupPage isSubscribed cKey gKey) Nothing

subscriptionHandler :: POSTContentHandler
subscriptionHandler = do
  gKey <- liftM (GroupKey . fromString . unpack) $ getParamE (fieldName groupKeyName)
  cKey <- liftM (CourseKey . fromString . unpack) $ getParamE (fieldName courseKeyInfo)
  isSubscribed <- runStoryE . isUserInGroup $ gKey
  when isSubscribed . throwError . strMsg $ "User is already subscribed"
  return $ SubscribeToGroup cKey gKey

groupPage :: Bool -> CourseKey -> GroupKey -> Html
groupPage True  = alreadyRegistered
groupPage False = newToGroup

alreadyRegistered :: CourseKey -> GroupKey -> Html
alreadyRegistered _ _ = do
  H.br
  "User is already registered"

newToGroup :: CourseKey -> GroupKey -> Html
newToGroup ck gk = do
  H.br
  "User is new to the group"
  H.br
  "Do you want to subscribe?"
  H.form ! A.method "post" ! A.action (fromString . routeOf $ Group) $ do
    H.table ! A.id "yesOrNo" $ do
      H.tr $ do
        H.td $ hiddenGroupKeyInput  gk
        H.td $ hiddenCourseKeyInput ck
        H.td $ H.input ! A.type_ "submit" ! A.value ("Subscribe")

