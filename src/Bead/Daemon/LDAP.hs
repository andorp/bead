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
import           Bead.Daemon.LDAP.Query (QuerySettings(..))
import qualified Bead.Daemon.LDAP.Query as Query
import qualified Bead.Domain.Entities as Entity

type Username = String
type Password = String

data LDAPDaemon = LDAPDaemon {
    query :: Username -> IO (IO LDAPResult)
    -- ^ Queries the given username, and returns a computation that blocks
    --   until the authentication result is not evaluated.
  }

-- Configuration for the LDAPDaemon
data LDAPDaemonConfig = LDAPDaemonConfig {
    timeout     :: Int
  , workers     :: Int
  , command     :: String
  , uidKey      :: String
  , nameKey     :: String
  , emailKey    :: String
  }

startLDAPDaemon :: Logger -> LDAPDaemonConfig -> IO LDAPDaemon
startLDAPDaemon logger config = do
  queryQueue <- newTChanIO
  noOfRequests <- newTVarIO (0 :: Int)

  -- Every query tries to create an MTVar that blocks until
  -- the final result is not calculated.
  let query user = do
        resultEnvelope <- newEmptyTMVarIO
        atomically $ do
          n <- modifyTVar add1 noOfRequests
          writeTChan queryQueue (user,resultEnvelope)
          return $ do
            log logger INFO $ concat ["There are ", show n, " queries waiting in the LDAP queue."]
            atomically $ readTMVar resultEnvelope

  let uid_key = uidKey config
  let name_key = nameKey config
  let email_key = emailKey config

  let queryOK attrs =
       let attrMap = Map.fromList attrs
       in fromMaybe LDAPAttrMapError $
            do uid   <- fmap Entity.Uid $ Map.lookup uid_key attrMap
               name  <- Map.lookup name_key attrMap
               email <- fmap Entity.Email $ Map.lookup email_key attrMap
               return $! LDAPUser (uid, email, name)

  let queryInvalid = LDAPInvalidUser

  let queryError msg = LDAPError msg

  let attrs = [uid_key, name_key, email_key]

  let loop daemon_id = do
        log logger INFO $ concat ["LDAP Daemon ", daemon_id, " is waiting"]
        join $ atomically $ do
          modifyTVar sub1 noOfRequests
          (user,resultEnvelope) <- readTChan queryQueue
          return $ do
            let querySettings = QuerySettings
                                  { queryTimeout = timeout config
                                  , queryCommand = command config
                                  }
            queryResult <- waitCatch =<< async (Query.query querySettings user attrs)
            log logger INFO $ concat ["LDAP Daemon ", daemon_id, " queries attributes for ", user]
            atomically $ putTMVar resultEnvelope $ case queryResult of
              Left  someError  -> LDAPError $ show someError
              Right result     -> Query.queryResult queryOK queryInvalid queryError result
            loop daemon_id

  -- Start the workers
  sequence_ $ map (forkIO . loop . show) [1 .. workers config]
  return $! LDAPDaemon query
    where
      sub1 x = x - 1
      add1 x = x + 1
      modifyTVar f var = do
        x <- fmap f $ readTVar var
        writeTVar var x
        return x
