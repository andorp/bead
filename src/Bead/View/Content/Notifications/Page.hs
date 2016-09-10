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
                , show (notifMessage notif)
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
