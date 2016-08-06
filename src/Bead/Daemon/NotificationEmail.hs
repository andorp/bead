{-# LANGUAGE OverloadedStrings #-}
module Bead.Daemon.NotificationEmail where

import           Debug.Trace
import           Control.Exception
import           Control.Concurrent

import qualified Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as S
import           Bead.Controller.UserStories (runUserStory, notificationEmails, UserStory)
import           Bead.View.Translation (trans)

startNotificationEmailDaemon :: L.Logger -> Int -> Int -> ServiceContext -> IO ()
startNotificationEmailDaemon logger initWait wait context = do
  let agent = do threadDelay (secToMicroSec wait)
                 ((runUserStory context trans NotificationAgent placeHolder) >> return ()) `catch` someException
                 putStrLn "Running Notification Email Daemon"
                 agent
  forkIO $ do
    threadDelay (secToMicroSec initWait)
    agent
  return ()
  where
    secToMicroSec = (* 1000000)

    someException :: SomeException -> IO ()
    someException e = do
      (L.log logger L.ERROR (show e)) `catch` (loggerException e)
      return ()
      where
        loggerException :: SomeException -> SomeException -> IO ()
        loggerException original e = do
          print original
          print e

placeHolder :: UserStory ()
placeHolder = trace "NotificationEmailDaemon" (return ())
