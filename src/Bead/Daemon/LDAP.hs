module Bead.Daemon.LDAP (
    LDAPDaemon(..)
  , LDAPDaemonConfig(..)
  , startLDAPDaemon
  , module Bead.Daemon.LDAP.Result
  ) where

import           Prelude hiding (log)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad (join)
import qualified Data.Map as Map
import           Data.Maybe
import           System.FilePath ((</>))

import           Bead.Controller.Logging
import           Bead.Daemon.LDAP.Result
import           Bead.Daemon.LDAP.Auth (AuthSettings(..))
import qualified Bead.Daemon.LDAP.Auth as Auth
import qualified Bead.Domain.Entities as Entity

type Username = String
type Password = String

data LDAPDaemon = LDAPDaemon {
    authenticate :: Username -> Password -> IO (IO LDAPResult)
    -- ^ Authenticates the given username and password, and returns a computation
    -- which blocks until the authentication result is not evaluated.
  }

-- Configuration for the LDAPDaemon
data LDAPDaemonConfig = LDAPDaemonConfig {
    tempDir     :: FilePath
  , timeOut     :: Int
  , noOfWorkers :: Int
  }

startLDAPDaemon :: Logger -> LDAPDaemonConfig -> IO LDAPDaemon
startLDAPDaemon logger config = do

  authQueue <- newTChanIO
  noOfRequests <- newTVarIO (0 :: Int)

  -- Every authentication try creates an MTVar that blocks until
  -- the final result is not calculated
  let auth user pass = do
        resultEnvelope <- newEmptyTMVarIO
        atomically $ do
          n <- modifyTVar add1 noOfRequests
          writeTChan authQueue (user,pass,resultEnvelope)
          return $ do
            log logger INFO $ concat ["There are ", show n, " users waiting in the LDAP queue."]
            atomically $ readTMVar resultEnvelope

  let authOK attrs =
       let attrMap = Map.fromList attrs
       in fromMaybe LDAPAttrMapError $
            do uid   <- fmap Entity.Uid $ Map.lookup "l"  attrMap
               name  <- Map.lookup "cn" attrMap
               email <- fmap Entity.Email $ Map.lookup "mail" attrMap
               return $! LDAPUser (uid, email, name)

  let authInvalid = LDAPInvalidAuth

  let authError _ msg = LDAPError msg

  let loop daemon_id = do
        log logger INFO $ concat ["LDAP Daemon ", daemon_id, " is waiting"]
        join $ atomically $ do
          modifyTVar sub1 noOfRequests
          (user,pass,resultEnvelope) <- readTChan authQueue
          return $ do
            let authSettings = AuthSettings (timeOut config) (tempDir config </> concat ["ticket_", daemon_id, "_%s"])
            let attrs = ["l", "cn", "mail"] -- TODO: Check if these are the needed attributes
            authResult <- waitCatch =<< async (Auth.authenticate authSettings user pass attrs)
            log logger INFO $ concat ["LDAP Daemon ", daemon_id, " authenticates ", user]
            atomically $ putTMVar resultEnvelope $ case authResult of
              Left  someError  -> LDAPError $ show someError
              Right result     -> Auth.authResult authOK authInvalid authError result
            loop daemon_id

  -- Start the workers
  sequence_ $ map (forkIO . loop . show) [1 .. noOfWorkers config]

  return $! LDAPDaemon auth
    where
      sub1 x = x - 1
      add1 x = x + 1
      modifyTVar f var = do
        x <- fmap f $ readTVar var
        writeTVar var x
        return x
