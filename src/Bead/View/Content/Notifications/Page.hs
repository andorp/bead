{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Notifications.Page (
    notifications
  ) where

import           Control.Monad (forM_)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import           Bead.View.I18N
import           Bead.View.RouteOf
import           Bead.Domain.Entity.Notification
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.Controller.UserStories as Story (notifications)

data PageData = PageData {
      pdNotifications :: [(Notification, NotificationState)]
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
        forM_ (pdNotifications p) $ \(notif, state) ->
          Bootstrap.listGroupLinkItem (linkFromNotif notif) . fromString $
            unwords
              [ show (notifDate notif)
              , show (notifMessage notif)
              , if state == Seen then "" else "NEW!"
              ]

-- TODO: Redirect for a page which marks a notification as seen.
linkFromNotif :: Notification -> String
linkFromNotif =
  routeOf . notificationType
              (\commentKey -> Pages.home ()) -- TODO: Anchor the comment
              (\evaluationKey -> Pages.home ()) -- TODO: Get the submissionkey of the evaluation
              (\assignmentKey -> Pages.viewAssignment assignmentKey ())
              (\assessmentKey -> Pages.viewAssessment assessmentKey ())
              (Pages.home ()) -- TODO: How system notification is shown?
              . notifType
