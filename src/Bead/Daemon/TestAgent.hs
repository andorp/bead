{-# LANGUAGE OverloadedStrings #-}
module Bead.Daemon.TestAgent where

import           Control.Exception
import           Control.Concurrent

import qualified Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as S
import           Bead.Controller.UserStories (runUserStory, testAgentFeedbacks)
import           Bead.View.Snap.Translation (trans)

-- Starts a thread that polls the persistence layer for new test agent comments in every w seconds
-- and places them into the right place.
startTestCommentsAgent :: L.Logger -> Int -> Int -> ServiceContext -> IO ()
startTestCommentsAgent logger initWait wait context = do
  let agent = do threadDelay (secToMicroSec wait)
                 ((runUserStory context trans TestAgent testAgentFeedbacks) >> return ()) `catch` someException
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
