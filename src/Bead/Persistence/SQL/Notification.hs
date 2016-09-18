module Bead.Persistence.SQL.Notification where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql
import Data.Maybe (catMaybes, maybeToList)
import Data.Time (getCurrentTime, UTCTime)
import Text.JSON.Generic (encodeJSON)

import qualified Bead.Domain.Entities as Domain (Username, User)
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Entity.Notification as Domain
import qualified Bead.Domain.Entity.Notification as Notification
import Bead.Persistence.SQL.Class
import Bead.Persistence.SQL.Entities
import Bead.Persistence.SQL.User (usernames)

saveNotification :: Domain.Notification -> Persist Domain.NotificationKey
saveNotification n = toDomainKey <$> insert (fromDomainValue n)

attachNotificationToUser :: Domain.Username -> Domain.NotificationKey -> UTCTime -> Persist ()
attachNotificationToUser username nk t = withUser username
  (persistError "attachNotificationToUser" $ "User is not found:" ++ show username) $ \user -> do
    -- Insert not seen
    void $ insertUnique (UserNotification (entityKey user) (fromDomainKey nk) False False t)

-- | Returns all the new notification on top and the limited seen notifications
notificationsOfUser :: Domain.Username -> Maybe Int -> Persist [(Domain.NotificationKey, Domain.NotificationState, Domain.NotificationProcessed)]
notificationsOfUser username seenLimit = withUser username (return []) $ \user -> do
  now <- liftIO getCurrentTime
  unseen <- selectList [ UserNotificationUser ==. (entityKey user)
                       , UserNotificationSeen ==. False
                       , UserNotificationCreated <=. now ]
                       [ Desc UserNotificationCreated ]
  seen <- selectList [ UserNotificationUser ==. (entityKey user)
                     , UserNotificationSeen ==. True
                     , UserNotificationCreated <=. now ]
                     ([ Desc UserNotificationCreated] ++ maybeToList (LimitTo <$> seenLimit))

  return . map (transformEntity . entityVal) $ unseen ++ seen
  where
    transformEntity e =
        ( toDomainKey $ userNotificationNotification e
        , if userNotificationSeen e      then Domain.Seen      else Domain.New
        , if userNotificationProcessed e then Domain.Processed else Domain.Unprocessed
        )

loadNotification :: Domain.NotificationKey -> Persist Domain.Notification
loadNotification nk = do
  mNot <- get (fromDomainKey nk)
  return $!
    maybe
      (persistError "loadNotification" $ "Notification is not found: " ++ show nk)
      toDomainValue
      mNot

notificationsOfAssignment :: Domain.AssignmentKey -> Persist [Domain.NotificationKey]
notificationsOfAssignment ak = do
  notifications <- selectList
    [ NotificationType ==. (encodeJSON $ Notification.Assignment ak) ]
    []
  return $! fmap (toDomainKey . entityKey) notifications

updateNotification :: Domain.NotificationKey -> Domain.Notification -> Persist ()
updateNotification nk n = do
  update (toEntityKey nk) $ Domain.withNotification n
    $ \event date typ ->
        [ NotificationEvent =. encodeJSON event
        , NotificationDate  =. date
        , NotificationType  =. encodeJSON typ
        ]

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

noOfUnseenNotifications :: Domain.Username -> Persist Int
noOfUnseenNotifications username = withUser username (return 0) $ \user -> do
  now <- liftIO getCurrentTime
  count [UserNotificationUser ==. (entityKey user), UserNotificationSeen ==. False, UserNotificationCreated <=. now]

mark :: EntityField UserNotification Bool -> Domain.Username -> Domain.NotificationKey -> Persist ()
mark f username nk = withUser username (return ()) $ \user -> do
  uns <- selectList
    [ UserNotificationUser ==. (entityKey user)
    , UserNotificationNotification ==. (fromDomainKey nk)
    ] []
  forM_ uns $ \n -> update (entityKey n) [ f =. True ]

markSeen      = mark UserNotificationSeen
markProcessed = mark UserNotificationProcessed

usersOfNotification :: Domain.NotificationKey -> Persist [Domain.Username]
usersOfNotification nk = do
  userIds <- map (userNotificationUser . entityVal) <$>
               selectList [UserNotificationNotification ==. (fromDomainKey nk)] []
  usernames userIds

updateUserNotification :: Domain.NotificationKey -> UTCTime -> Persist ()
updateUserNotification nk t = do
  uns <- selectList [ UserNotificationNotification ==. (fromDomainKey nk) ] []
  forM_ uns $ \n -> update (entityKey n) [ UserNotificationCreated =. t ]
