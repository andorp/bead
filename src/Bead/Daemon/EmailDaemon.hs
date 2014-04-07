{-# LANGUAGE OverloadedStrings #-}
module Bead.Daemon.EmailDaemon (
    EmailDaemon(..)
  , startEmailDaemon
  ) where

import           Prelude hiding (log)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad (join)
import qualified Data.List as List
import qualified Data.Text as Text

import           Network.Mail.Mime

import           Bead.Controller.Logging

{-
The email daemon recives email from the handlers and
send them out with the SendMail client without blocking the
main application.
-}

data EmailDaemon = EmailDaemon {
    sendEmail :: Mail -> IO () -- ^ Enqueue the email to the sending queue
  }

-- Starts the email daemon in a different thread and returns the enqueue function
-- for the thread.
startEmailDaemon :: Logger -> IO EmailDaemon
startEmailDaemon logger = do

  -- Shared information between sendmail client and the server
  emailChan <- newTChanIO
  noOfEmails <- newTVarIO (0 :: Int)

  -- How the enqueue emails for the sendmail
  let enqueueEmail email = do
        join $ atomically $ do
          writeTChan emailChan email
          n <- modifyTVar noOfEmails (+1)
          let toAddresses' = toAddresses email
              subject' = subject email
              logMsg = concat [ "EMAIL DAEMON: An email is enqueued to the \"", toAddresses'
                              , "\" with the subject: \"", subject', "\". Number of emails are in the system: "
                              , show n
                              ]
          return (log logger INFO logMsg)

  -- Loop: Read from the queue, try to send the email in a separate thread
  -- wait for the result, log the exception or the result.
  let loop = do join $ atomically $ do
                  email <- readTChan emailChan
                  n <- modifyTVar noOfEmails (\x -> x - 1)
                  return $ do
                    let toAddresses' = toAddresses email
                        subject' = subject email
                    result <- waitCatch =<< async (renderSendMail email)
                    case result of
                      Left someError -> log logger ERROR $ concat
                        [ "EMAIL DAEMON: Sending email to \"", toAddresses'
                        , "\" with the subject: \"", subject', "\". Error: "
                        , show someError
                        ]
                      Right _ -> log logger INFO $ concat
                        [ "EMAIL DAEMON: An email is sent to the \"", toAddresses'
                        , "\" with the subject: \"", subject', "\". Number of emails are in the system: "
                        , show n
                        ]
                    loop

  -- Start the loop
  forkIO loop

  return $! EmailDaemon enqueueEmail
  where
    modifyTVar t f = do
      x <- readTVar t
      let y = f x
      writeTVar t y
      return y

-- * Network.Mail.Mime helpers

-- Returns the first address
toAddresses :: Mail -> String
toAddresses m = case mailTo m of
  [] -> ""
  ts -> concat . List.intersperse ", " $ map (Text.unpack . addressEmail) ts

-- Returns the subject if it is defined otherwise empty string.
subject :: Mail -> String
subject m = case List.lookup "Subject" $ mailHeaders m of
  Nothing -> ""
  Just s  -> Text.unpack s
