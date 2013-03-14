{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateGroup (
    createGroup
  ) where

import Bead.View.Snap.Content
import qualified Bead.Controller.Pages as P (Page(CreateGroup))
import qualified Bead.View.UserActions as UA (UserAction(CreateGroup))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

createGroup :: Content
createGroup = getPostContentHandler createGroupHandler submitGroup

createGroupHandler :: GETContentHandler
createGroupHandler = withUserStateE $ \s -> do
  ck <- getValue
  blaze $ withUserFrame s (createGroupHtml ck) Nothing

createGroupHtml :: CourseKey -> Html
createGroupHtml ck = do
  postForm (routeOf P.CreateGroup) $ do
    "Create a new group"
    inputPagelet emptyGroup
    hiddenInput (fieldName courseKeyInfo) (keyString ck)
    submitButton "Create Group"

submitGroup :: POSTContentHandler
submitGroup = do
  courseKey <- getValue
  group     <- getValue
  setReqParamInSession . requestParam $ courseKey
  return $ UA.CreateGroup courseKey group

