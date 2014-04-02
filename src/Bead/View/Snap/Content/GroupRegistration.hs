{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GroupRegistration (
    groupRegistration
  , unsubscribeFromCourse
  ) where

import           Data.List (intersperse)
import           Data.String (fromString)

import qualified Text.Blaze.Html5 as H

import           Bead.Controller.UserStories (availableGroups, attendedGroups)
import qualified Bead.Controller.Pages as Pages
import           Bead.View.Snap.Content

groupRegistration = ViewModifyHandler groupRegistrationPage postGroupReg

unsubscribeFromCourse =
  ModifyHandler (UnsubscribeFromCourse <$> getParameter unsubscribeUserGroupKeyPrm)

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
    let registeredGroups = groupsRegistered desc
    H.p $ do
      H.h3 $ (fromString . msg $ Msg_GroupRegistration_RegisteredCourses "Registered courses")
      i18n msg $ groupsAlreadyRegistered registeredGroups
    when (not . null $ registeredGroups) $ do
      H.p $ (fromString . msg $ Msg_GroupRegistration_Warning $ concat
        [ "It is possible to quit from a group or move between groups until a submission is "
        , "submitted.  Otherwise, the teacher of the given group should be asked to undo the "
        , "group registration."
        ])
    H.p $ do
      H.h3 $ (fromString . msg $ Msg_GroupRegistration_SelectGroup "Select course and group")
      i18n msg $ groupsForTheUser (groups desc)

groupsAlreadyRegistered :: [(GroupKey, GroupDesc, Bool)] -> IHtml
groupsAlreadyRegistered ds = do
  msg <- getI18N
  return $ nonEmpty ds
    (fromString . msg $ Msg_GroupRegistration_NoRegisteredCourses
      "No registered courses.  Choose a group.")
    (H.table # informationalTable $ do
      H.tr $ do
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Courses "Groups")
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Admins "Teachers")
        H.th # (grayBackground <> informationalCell) $ (fromString . msg $ Msg_GroupRegistration_Unsubscribe "Unregister")
      mapM_ (groupLine msg) ds)
  where
    unsubscribeFromCourse k = Pages.unsubscribeFromCourse k ()

    groupLine msg (key, desc, hasSubmission) = flip groupDescFold desc $ \n as -> do
      H.tr $ do
        H.td # informationalCell $ fromString $ n
        H.td # informationalCell $ fromString $ join $ intersperse " " as
        H.td # informationalCell $
          if hasSubmission
            then (fromString . msg $ Msg_GroupRegistration_NoUnsubscriptionAvailable
              "Unregistration is not allowed.")
            else postForm (routeOf $ unsubscribeFromCourse key) $
                   submitButton
                     (fieldName unsubscribeFromCourseSubmitBtn)
                     (msg $ Msg_GroupRegistration_Unsubscribe "Unregister")

groupsForTheUser :: [(GroupKey, GroupDesc)] -> IHtml
groupsForTheUser gs = do
  msg <- getI18N
  return $
    nonEmpty gs (fromString . msg $ Msg_GroupRegistration_NoAvailableCourses
      "There are no available groups yet.") $
    postForm (routeOf groupRegistration) $ do
      selection (fieldName groupRegistrationField) $ do
        mapM_ (\(gk,gd) -> option (paramValue gk) (descriptive gd) False) gs
      H.br
      submitButton (fieldName regGroupSubmitBtn) (msg $ Msg_GroupRegistration_Register "Register")

  where
    groupRegistration = Pages.groupRegistration ()

    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

