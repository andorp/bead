module Bead.Domain.Entity.Notification where

import Data.Text
import Data.Time (UTCTime)

data NotificationState = New | Seen
  deriving (Eq, Show)

-- The notification is rendered for the user on some informational
-- page or send via email.
data Notification = Notification {
    notifMessage :: Text
  , notifDate    :: UTCTime
  } deriving (Eq, Show)

notification f (Notification msg date) = f msg date
