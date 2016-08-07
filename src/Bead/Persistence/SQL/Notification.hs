module Bead.Persistence.SQL.Notification where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Database.Persist.Sql
import Data.Maybe (catMaybes)

import qualified Bead.Domain.Entities as Domain (Username, User)
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Entity.Notification as Domain
import Bead.Persistence.SQL.Class
import Bead.Persistence.SQL.Entities
import Bead.Persistence.SQL.User (usernames)

saveNotification :: Domain.Notification -> Persist Domain.NotificationKey
saveNotification n = toDomainKey <$> insert (fromDomainValue n)

attachNotificationToUser :: Domain.Username -> Domain.NotificationKey -> Persist ()
attachNotificationToUser username nk = withUser username
  (persistError "attachNotificationToUser" $ "User is not found:" ++ show username) $ \user ->
    -- Insert not seen
    void $ insertUnique (UserNotification (entityKey user) (fromDomainKey nk) False False)

notificationsOfUser :: Domain.Username -> Persist [Domain.NotificationKey]
notificationsOfUser username = withUser username (return []) $ \user ->
  map (toDomainKey . userNotificationNotification . entityVal) <$>
    selectList [UserNotificationUser ==. (entityKey user)] []

loadNotification :: Domain.NotificationKey -> Persist Domain.Notification
loadNotification nk = do
  mNot <- get (fromDomainKey nk)
  return $!
    maybe
      (persistError "loadNotification" $ "Notification is not found: " ++ show nk)
      toDomainValue
      mNot

unprocessedNotifications :: Persist [(Domain.User, Domain.NotificationKey, Domain.NotificationState)]
unprocessedNotifications = do
  userNotifs <- selectList [UserNotificationProcessed ==. False] []
  catMaybes <$> (forM userNotifs $ \ent' -> do
    let ent = entityVal ent'
    let userId  = userNotificationUser ent
    let notifId = userNotificationNotification ent
    mUser <- get userId
    return $ case mUser of
      Nothing -> Nothing
      Just un -> Just (toDomainValue un, toDomainKey notifId, undefined))

mark :: EntityField UserNotification Bool -> Domain.Username -> Domain.NotificationKey -> Persist ()
mark f username nk = withUser username (return ()) $ \user -> do
  uns <- selectList
    [ UserNotificationUser ==. (entityKey user)
    , UserNotificationNotification ==. (fromDomainKey nk)
    ] []
  forM_ uns $ \n -> update (entityKey n) [ f =. True ]

markRead      = mark UserNotificationSeen
markProcessed = mark UserNotificationProcessed

usersOfNotification :: Domain.NotificationKey -> Persist [Domain.Username]
usersOfNotification nk = do
  userIds <- map (userNotificationUser . entityVal) <$>
               selectList [UserNotificationNotification ==. (fromDomainKey nk)] []
  usernames userIds
