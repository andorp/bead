{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Notifications.Page (
    notifications
  ) where

import           Control.Monad (forM_)
import           Data.String (fromString)
import           Text.Printf (printf)

import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import           Bead.View.I18N
import           Bead.View.RouteOf
import           Bead.Domain.Entity.Notification
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.Controller.UserStories as Story (notifications)

data PageData = PageData {
      pdNotifications :: [(Notification, NotificationState, NotificationReference)]
    }

notifications :: ViewHandler
notifications = ViewHandler notificationsPage

notificationsPage :: GETContentHandler
notificationsPage = do
  pageData <- PageData <$> userStory Story.notifications
  return $ notificationsContent pageData

notificationsContent :: PageData -> IHtml
notificationsContent p = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ Bootstrap.colMd12 $ do
      Bootstrap.listGroup $
        forM_ (pdNotifications p) $ \(notif, state, ref) ->
          (if state == Seen
            then Bootstrap.listGroupLinkItem
            else Bootstrap.listGroupAlertLinkItem Bootstrap.Info
          ) (linkFromNotif ref) . fromString $
              unwords
                [ show (notifDate notif)
                , translateEvent msg $ notifEvent notif
                ]

-- TODO: Redirect for a page which marks a notification as seen.
linkFromNotif :: NotificationReference -> String
linkFromNotif =
  routeOf . notificationReference
              (\ak sk _ck -> Pages.submissionDetails ak sk ()) -- TODO: Anchor the commenr
              (\ak sk _ek -> Pages.submissionDetails ak sk ()) -- TODO: Get the submissionkey of the evaluation
              (\sk _ek    -> Pages.viewUserScore sk ()) -- TODO: Get the submissionkey of the evaluation
              (\ak -> Pages.viewAssignment ak ())
              (\ak -> Pages.viewAssessment ak ())
              (Pages.notifications ()) -- System notifications are one liners

-- Resolve a notification event to an actual message through the I18N layer.
translateEvent :: I18N -> NotificationEvent -> String
translateEvent i18n e = case e of
  NE_CourseAdminCreated course -> printf
    (i18n $ msg_NE_CourseAdminCreated "A course has been assigned: %s")
      course
  NE_CourseAdminAssigned course assignee -> printf
    (i18n $ msg_NE_CourseAdminAssigned "An administrator has been added to course \"%s\": %s")
      course assignee
  NE_TestScriptCreated creator course -> printf
    (i18n $ msg_NE_TestScriptCreated "%s created a new test script for course \"%s\"")
      creator course
  NE_TestScriptUpdated editor script course -> printf
    (i18n $ msg_NE_TestScriptUpdated "%s modified test script \"%s\" for course \"%s\"")
      editor script course
  NE_RemovedFromGroup group deletor -> printf
    (i18n $ msg_NE_RemovedFromGroup "Removed from group \"%s\" by %s")
      group deletor
  NE_GroupAdminCreated course creator group -> printf
    (i18n $ msg_NE_GroupAdminCreated "A group of course \"%s\" has been assigned by %s: %s")
      course creator group
  NE_GroupAssigned group course assignor assignee -> printf
    (i18n $ msg_NE_GroupAssigned "Group \"%s\" of course \"%s\" has been assigned to %s by %s")
      group course assignor assignee
  NE_GroupCreated course creator group -> printf
    (i18n $ msg_NE_GroupCreated "A group has been created for course \"%s\" by %s: %s")
      course creator group
  NE_GroupAssignmentCreated creator group course assignment -> printf
    (i18n $ msg_NE_GroupAssignmentCreated "%s created a new assignment for group \"%s\" (\"%s\"): %s")
      creator group course assignment
  NE_CourseAssignmentCreated creator course assignment -> printf
    (i18n $ msg_NE_CourseAssignmentCreated "%s created a new assignment for course \"%s\": %s")
      creator course assignment
  NE_GroupAssessmentCreated creator group course assessment -> printf
    (i18n $ msg_NE_GroupAssessmentCreated "%s created a new assessment for group \"%s\" (\"%s\"): %s")
      creator group course assessment
  NE_CourseAssessmentCreated creator course assessment -> printf
    (i18n $ msg_NE_CourseAssessmentCreated "%s created a new assessment for course \"%s\": %s")
      creator course assessment
  NE_AssessmentUpdated editor assessment -> printf
    (i18n $ msg_NE_AssessmentUpdated "%s modified assessment: %s")
      editor assessment
  NE_AssignmentUpdated editor assignment -> printf
    (i18n $ msg_NE_AssignmentUpdated "%s modified assignment: %s")
      editor assignment
  NE_EvaluationCreated evaluator submission -> printf
    (i18n $ msg_NE_EvaluationCreated "%s evaluated submission: %s")
      evaluator submission
  NE_AssessmentEvaluationUpdated editor assessment -> printf
    (i18n $ msg_NE_AssessmentEvaluationUpdated "%s modified evaluation of score: %s")
      editor assessment
  NE_AssignmentEvaluationUpdated editor submission -> printf
    (i18n $ msg_NE_AssignmentEvaluationUpdated "%s modified evaluation of submission: %s")
      editor submission
  NE_CommentCreated commenter submission body -> printf
    (i18n $ msg_NE_CommentCreated "%s commented on submission %s: \"%s\"")
      commenter submission body
