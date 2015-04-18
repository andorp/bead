{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GroupRegistration.Page (
    groupRegistration
  , unsubscribeFromCourse
  ) where

import           Control.Monad
import           Control.Arrow ((***))
import           Data.List (intersperse)
import           Data.String (fromString)

import           Text.Blaze.Html5 as H hiding (map)

import           Bead.Controller.UserStories (availableGroups, attendedGroups)
import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

groupRegistration = ViewModifyHandler groupRegistrationPage postGroupReg

unsubscribeFromCourse =
  ModifyHandler (UnsubscribeFromCourse <$> getParameter unsubscribeUserGroupKeyPrm)

data GroupRegData = GroupRegData {
    groups :: [(GroupKey, GroupDesc)]
  , groupsRegistered :: [(GroupKey, GroupDesc, Bool)]
  }

postGroupReg :: POSTContentHandler
postGroupReg = SubscribeToGroup
  <$> getParameter (jsonGroupKeyPrm (fieldName groupRegistrationField))

groupRegistrationPage :: GETContentHandler
groupRegistrationPage = do
  desc <- userStory $ do
    as <- attendedGroups
    let attendedGroupKeys = map fst3 as
        newGroupForUser (gk,_) = not (elem gk attendedGroupKeys)
    gs <- (filter newGroupForUser) <$> availableGroups
    return GroupRegData {
        groups = gs
      , groupsRegistered = as
      }
  return $ groupRegistrationContent desc
  where
    fst3 (f,_,_) = f

groupRegistrationContent :: GroupRegData -> IHtml
groupRegistrationContent desc = do
  msg <- getI18N
  return $ do
    let registeredGroups = groupsRegistered desc
    Bootstrap.rowColMd12 $ do
      H.h3 $ fromString $ msg $ msg_GroupRegistration_RegisteredCourses "Registered courses"
      i18n msg $ groupsAlreadyRegistered registeredGroups
    when (not . null $ registeredGroups) $ Bootstrap.rowColMd12 $ do
      H.p $ (fromString . msg $ msg_GroupRegistration_Warning $ concat
        [ "It is possible to quit from a group or move between groups until a submission is "
        , "submitted.  Otherwise, the teacher of the given group should be asked to undo the "
        , "group registration."
        ])
    Bootstrap.rowColMd12 $ do
      H.h3 $ (fromString . msg $ msg_GroupRegistration_SelectGroup "Select course and group")
    i18n msg $ groupsForTheUser (groups desc)
    Bootstrap.turnSelectionsOn

groupsAlreadyRegistered :: [(GroupKey, GroupDesc, Bool)] -> IHtml
groupsAlreadyRegistered ds = do
  msg <- getI18N
  return $ nonEmpty ds
    (fromString . msg $ msg_GroupRegistration_NoRegisteredCourses
      "No registered courses.  Choose a group.")
    (Bootstrap.table $ do
      thead $ H.tr $ do
        H.th . fromString . msg $ msg_GroupRegistration_Courses "Groups"
        H.th . fromString . msg $ msg_GroupRegistration_Admins "Teachers"
        H.th . fromString . msg $ msg_GroupRegistration_Unsubscribe "Unregister"
      tbody $ mapM_ (groupLine msg) ds)
  where
    unsubscribeFromCourse k = Pages.unsubscribeFromCourse k ()

    groupLine msg (key, desc, hasSubmission) = flip groupDescFold desc $ \n as -> do
      H.tr $ do
        H.td $ fromString n
        H.td $ fromString $ join $ intersperse " " as
        H.td $
          if hasSubmission
            then (fromString . msg $ msg_GroupRegistration_NoUnsubscriptionAvailable
              "Unregistration is not allowed.")
            else postForm (routeOf $ unsubscribeFromCourse key) $
                   Bootstrap.smallSubmitButton
                     (fieldName unsubscribeFromCourseSubmitBtn)
                     (msg $ msg_GroupRegistration_Unsubscribe "Unregister")

groupsForTheUser :: [(GroupKey, GroupDesc)] -> IHtml
groupsForTheUser gs = do
  msg <- getI18N
  return $
    nonEmpty gs
      (Bootstrap.rowColMd12 $ p $ fromString . msg $ msg_GroupRegistration_NoAvailableCourses "There are no available groups yet.") $
      postForm (routeOf groupRegistration) $ do
        Bootstrap.selection (fieldName groupRegistrationField) (const False) (map (id *** descriptive) gs)
        Bootstrap.submitButton (fieldName regGroupSubmitBtn) (msg $ msg_GroupRegistration_Register "Register")
  where
    groupRegistration = Pages.groupRegistration ()

    descriptive :: GroupDesc -> String
    descriptive g = join [gName g, " / ", join (intersperse " , " (gAdmins g))]

