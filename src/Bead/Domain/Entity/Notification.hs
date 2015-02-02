module Bead.Domain.Entity.Notification where

import Data.Text

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

-- The notification is rendered for the user on some informational
-- page or send via email.
data Notification = Notification {
    notifMessage :: Text
  } deriving (Eq, Show)

notification f (Notification msg) = f msg

