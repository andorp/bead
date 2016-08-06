module Bead.Domain.Entity.Notification where

import Data.Text
import Data.Time.Clock (UTCTime)

-- The notifications can come from different sources
data NotifType
  = Comment
  | Feedback
  | System
  deriving (Eq, Show)

notifType
  comment
  feedback
  system
  n = case n of
    Comment  -> comment
    Feedback -> feedback
    System   -> system

data NotificationState = New | Seen
  deriving (Eq, Show)

-- The notification is rendered for the user on some informational
-- page or send via email.
data Notification = Notification {
    notifMessage :: Text
  , notifDate    :: UTCTime
  } deriving (Eq, Show)

notification f (Notification msg date) = f msg date
