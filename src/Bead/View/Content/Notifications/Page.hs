{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Notifications.Page (
    notifications
  ) where

import           Bead.View.Content
import           Bead.View.I18N

notifications :: ViewHandler
notifications = ViewHandler notificationsPage

notificationsPage :: GETContentHandler
notificationsPage = return notificationsContent

notificationsContent :: IHtml
notificationsContent = do
  msg <- getI18N
  return $ do
    return ()
