{-# LANGUAGE CPP #-}
module Bead.Daemon.Logout (
    startLogoutDaemon
  , LogoutDaemon(..)
#ifdef TEST
  , logoutQueueTests
#endif
  ) where

{-
The purpose of the logout daemon is to make logout the active
users after a given amount of a time, maintaining the activity information
of the users.

This functionality is needed because is possible that the users abadon
their sessions without inactivating it by logging out from the service
-}

import           Prelude hiding (log)

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Time

import           Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

import           Bead.Controller.Logging
import           Bead.Controller.ServiceContext

#ifdef TEST
import           Control.Monad.State
import qualified Data.Set as Set
import           Test.Tasty.TestSet
#endif

-- * User Queue

-- Logout queue that contains information about the time of the user's
-- last action, and a queue of the possible logout time of the user
-- if the user remains inactive
data Queue u = Queue {
    qPqueue  :: MinPQueue UTCTime u
    -- ^ Priority queue that contains the logout times of the users
  , qUserMap :: Map u UTCTime
    -- ^ Usermap that containt the last activity time of the users
  } deriving (Eq, Show)

queueCata f (Queue queue userMap) = f queue userMap

withQueue (Queue queue userMap) f = f queue userMap

emptyQueue :: (Ord u) => Queue u
emptyQueue = Queue PQueue.empty Map.empty

-- Creates a list of the users which need to be logged out,
-- and a new queue which does not contains the users which are in the
-- logout list
usersLogout :: Ord u => UTCTime -> Queue u -> (Queue u,[u])
usersLogout now = queueCata delete where

  untilNow key _usrToken = (key < now)

  delete queue userMap = (Queue queue' userMap', users) where
    (users',queue') = PQueue.spanWithKey untilNow queue
    users    = map snd $ users'
    userMap' = foldl (flip Map.delete) userMap users

-- Updates the user possible logout time, with the given time parameter
-- that represents the actual time, and the possible timeout value
-- is the now and the given delta in seconds
updateTimeout :: (Ord u) => u -> UTCTime -> Int -> Queue u -> Queue u
updateTimeout user now delta = queueCata update where
  update queue userMap =
    let newTime  = addUTCTime (fromIntegral delta) now
        userMap' = Map.insert user newTime userMap
    in case Map.lookup user userMap of
         Nothing   -> Queue (PQueue.insert newTime user queue) userMap'
         Just time -> Queue
           (PQueue.insert newTime user $
              PQueue.filterWithKey (\time' user' -> not (time == time' && user == user')) queue)
           userMap'

-- Removes the user from the queue
removeUser :: (Ord u) => u -> Queue u -> Queue u
removeUser user = queueCata remove where
  remove queue userMap = case Map.lookup user userMap of
    Nothing   -> Queue queue userMap
    Just time ->
      let userMap' = Map.delete user userMap
          queue'   = PQueue.filterWithKey (\time' user' -> not (time == time' && user == user')) queue
      in Queue queue' userMap'

-- Returns True if the user is in the queue otherwise False
isUserInQueue :: (Ord u) => u -> Queue u -> Bool
isUserInQueue user = queueCata $ \_queue userMap -> isJust $ Map.lookup user userMap

-- * Logout Daemon

-- A logout daemon run as a seperate thread and
data LogoutDaemon = LogoutDaemon {
    userActivity :: UsrToken -> UTCTime -> IO ()
    -- ^ Updates the user activity at the given time of the call
  , userLogout :: UsrToken -> IO ()
    -- ^ Deletes the information related to the user
  }

-- Start a logout deamon, which logs the events with the logger, the default
-- logout time is dt seconds, and removes the user from the given service context
-- invoking the daemon in each inv seconds
startLogoutDaemon :: Logger -> Int -> Int -> UserContainer UserState -> IO LogoutDaemon
startLogoutDaemon logger dt inv container = do
  varQueue <- newMVar emptyQueue
  ldInfoRef <- newIORef (0,0,0)

  let invMs = secondsToMicroseconds inv

  -- Activated by other thread, when the given user has some activity
  let activity usrToken now =
        modifyMVar_ varQueue (return . updateTimeout usrToken now dt)

  -- Activated by other thread, when the given user logs out
  let userlogout usrToken =
        modifyMVar_ varQueue (return . removeUser usrToken)

  -- The daemon checks the timeout queue and logs out the users from
  -- time to time
  let daemon = do
        now <- getCurrentTime
        -- Log out all the timed out user from the user container
        (userMapSize, noOfUsers) <- modifyMVar varQueue $ \queue -> do
          let (queue', users) = usersLogout now queue
              usersMapSize = withQueue queue' $ \_queue usersMap -> Map.size usersMap
              noOfUsers    = length users
          forM users $ \user -> userLogsOut container user
          return (queue', (usersMapSize, noOfUsers))
        -- Checks if there is a difference from the last invocation in
        -- the stored information
        after <- getCurrentTime
        let delta = round' (1000 * diffUTCTime after now)
            actLDInfo = (userMapSize, noOfUsers, delta)
        prevLDInfo <- readIORef ldInfoRef
        when (prevLDInfo /= actLDInfo) $ do
          -- If there is a difference update the info and log
          writeIORef ldInfoRef actLDInfo
          let msg = concat [ "LOGOUT DAEMON: Number of users logged in: ", show userMapSize
                           , ". Number of users logging out: ", show noOfUsers
                           , " in ", show delta, "ms."
                           ]
          log logger INFO msg
        threadDelay invMs
        daemon

  forkIO $ daemon
  return $ LogoutDaemon activity userlogout
  where
    secondsToMicroseconds = (* 1000000)

    round' :: NominalDiffTime -> Integer
    round' = round

#ifdef TEST

utcTimer :: Int -> UTCTime
utcTimer s = addUTCTime (fromIntegral s) (read "2014-03-06 23:21:13.510557 UTC")

emptyQueueInt :: Queue Int
emptyQueueInt = emptyQueue

user1 = 1
user2 = 2
dt10  = 10

simulate s = evalState s emptyQueueInt
simulate2 = snd . simulate

step = modify
info = gets

logoutQueueTests = do
  assertEquals "Empty queue logout" (emptyQueueInt, []) (usersLogout (utcTimer 0) emptyQueueInt) "Some user is logged out from empty queue"
  assertEquals "New user" True
    (simulate $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       info $ isUserInQueue user1)
    "New user is not placed in the queue"
  assertEquals "Non existing user" False
    (simulate $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       info $ isUserInQueue user2)
    "User is not deleted from the queue"
  assertEquals "User does not time out" []
    (simulate2 $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       info $ usersLogout (utcTimer 10))
    "User is deleted before time out"
  assertEquals "User times out" [user1]
     (simulate2 $ do
        step $ updateTimeout user1 (utcTimer 0) dt10
        info $ usersLogout (utcTimer 11))
     "User is not deleted after time out"
  assertEquals "One user times out" [user1]
     (simulate2 $ do
        step $ updateTimeout user1 (utcTimer 0) dt10
        step $ updateTimeout user2 (utcTimer 1) dt10
        info $ usersLogout (utcTimer 11))
     "More than one user is deleted from the queue"
  assertEquals "One user times out" [user1]
     (simulate2 $ do
        step $ updateTimeout user1 (utcTimer 0) dt10
        step $ updateTimeout user2 (utcTimer 1) dt10
        info $ usersLogout (utcTimer 11))
    "More than one user is deleted from the queue"
  assertEquals "Two users time out" (Set.fromList [user1, user2])
    (Set.fromList . simulate2 $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       step $ updateTimeout user2 (utcTimer 1) dt10
       info $ usersLogout (utcTimer 20))
    "Two users are not deleted from the queue"
  assertEquals "Non logged in user logs out" []
    (simulate2 $ do
       step $ removeUser user1
       info $ usersLogout (utcTimer 20))
    "Non logged is user is logged out"
  assertEquals "Logged in user logs out" []
    (simulate2 $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       step $ removeUser user1
       info $ usersLogout (utcTimer 20))
    "User is not removed from the queue"
  assertEquals "One user logs out" [user1]
    (simulate2 $ do
       step $ updateTimeout user1 (utcTimer 0) dt10
       step $ updateTimeout user2 (utcTimer 0) dt10
       step $ removeUser user2
       info $ usersLogout (utcTimer 20))
    "More than one users are logged out"

#endif
