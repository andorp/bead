module Bead.Persistence.SQL.Notification where

import Control.Applicative ((<$>))
import Database.Persist.Sql
import Data.Maybe (listToMaybe)

import qualified Bead.Domain.Entities as Domain (Username)
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Entity.Notification as Domain
import Bead.Persistence.SQL.Class
import Bead.Persistence.SQL.Entities
import Bead.Persistence.SQL.User (usernames)

attachNotificationToUser :: Domain.Username -> Domain.NotificationKey -> Persist ()
attachNotificationToUser username nk = withUser username
  (persistError "attachNotificationToUser" $ "User is not found:" ++ show username) $ \user ->
    void $ insertUnique (UserNotification (entityKey user) (fromDomainKey nk))

notificationsOfUser :: Domain.Username -> Persist [Domain.NotificationKey]
notificationsOfUser username = withUser username (return []) $ \user ->
  map (toDomainKey . userNotificationNotification . entityVal) <$>
    selectList [UserNotificationUser ==. (entityKey user)] []

saveCommentNotification :: Domain.CommentKey -> Domain.Notification -> Persist Domain.NotificationKey
saveCommentNotification ck n = do
  key <- insert (fromDomainValue n)
  insertUnique (CommentNotification (fromDomainKey ck) key)
  return (toDomainKey key)

saveFeedbackNotification :: Domain.FeedbackKey -> Domain.Notification -> Persist Domain.NotificationKey
saveFeedbackNotification fk n = do
  key <- insert (fromDomainValue n)
  insertUnique (FeedbackNotification (fromDomainKey fk) key)
  return (toDomainKey key)

saveSystemNotification :: Domain.Notification -> Persist Domain.NotificationKey
saveSystemNotification n = do
  key <- insert (fromDomainValue n)
  return (toDomainKey key)

loadNotification :: Domain.NotificationKey -> Persist Domain.Notification
loadNotification nk = do
  mNot <- get (fromDomainKey nk)
  return $!
    maybe
      (persistError "loadNotification" $ "Notification is not found: " ++ show nk)
      toDomainValue
      mNot

commentOfNotification :: Domain.NotificationKey -> Persist (Maybe Domain.CommentKey)
commentOfNotification nk = do
  keys <- map (toDomainKey . commentNotificationComment . entityVal) <$>
            selectList [CommentNotificationNotification ==. (fromDomainKey nk)] []
  return $! listToMaybe keys

feedbackOfNotification :: Domain.NotificationKey -> Persist (Maybe Domain.FeedbackKey)
feedbackOfNotification nk = do
  keys <- map (toDomainKey . feedbackNotificationFeedback . entityVal) <$>
            selectList [FeedbackNotificationNotification ==. (fromDomainKey nk)] []
  return $! listToMaybe keys

usersOfNotification :: Domain.NotificationKey -> Persist [Domain.Username]
usersOfNotification nk = do
  userIds <- map (userNotificationUser . entityVal) <$>
               selectList [UserNotificationNotification ==. (fromDomainKey nk)] []
  usernames userIds
