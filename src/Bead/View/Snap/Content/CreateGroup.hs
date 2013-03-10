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
createGroup = Content {
    get  = Just createGroupHandler
  , post = Just submitGroup
  }

createGroupHandler :: GETContentHandler
createGroupHandler = withUserStateE $ \s -> do
  ck <- getParamE (fieldName courseKeyInfo) CourseKey "Course key is not found"
  lift $ blaze $ withUserFrame s (createGroupHtml ck) Nothing

createGroupHtml :: CourseKey -> Html
createGroupHtml ck = do
  H.form ! A.method "post" ! A.action (routeWithParams P.CreateGroup [requestParam ck]) $ do
    "Create a new group"
    H.table ! A.id "create-group" $ do
      mapM_ field [
          ("Group Code", fieldName groupCodeField)
        , ("Group Name", fieldName groupNameField)
        , ("Group Desc", fieldName groupDescField)
        ]
    H.input ! A.type_ "submit"
  where
    field (text, name) = do
      H.tr $ do
        H.td text
        H.td $ H.textarea ! A.name name ! A.cols "10" ! A.rows "1" $ empty

submitGroup :: POSTContentHandler
submitGroup = do
  courseKey <- getParamE (fieldName courseKeyInfo) CourseKey "Course key is not found"
  groupCodeText <- getParamE (fieldName groupCodeField) GroupCode "Group code is not found"
  groupNameText <- getParamE (fieldName groupNameField) id "Group name is not found"
  groupDescText <- getParamE (fieldName groupDescField) id "Group description is not found"
  setReqParamInSession . requestParam $ courseKey
  return . UA.CreateGroup courseKey $ Group {
      groupCode = groupCodeText
    , groupName = groupNameText
    , groupDesc = groupDescText
    }

