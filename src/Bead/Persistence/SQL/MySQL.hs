{-# LANGUAGE Rank2Types #-}
module Bead.Persistence.SQL.MySQL where

import           Control.Exception
import           Control.Monad.Logger
import           Data.String (fromString)
import           Database.Persist.Sql
import           Database.Persist.MySQL

import qualified Bead.Config as Config
import           Bead.Domain.Types (Erroneous(..))
import           Bead.Persistence.Initialization
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS

import Control.Monad.Trans.Resource

data Config = Config {
    dbName :: String
  , host :: String
  , port :: Int
  , user :: String
  , pass :: String
  } deriving (Eq, Read, Show)

defaultConfig = Config {
    dbName = "bead"
  , host = "localhost"
  , port = 3306
  , user = "root"
  , pass = "password"
  }

configToPersistConfig :: Config.Config -> Config
configToPersistConfig = mysqlConfigToPersistConfig . Config.persistConfig
  where
    mysqlConfigToPersistConfig p = Config {
        dbName = Config.mySQLDbName p
      , host = Config.mySQLHost p
      , port = Config.mySQLPort p
      , user = Config.mySQLUser p
      , pass = Config.mySQLPass p
      }

parseConfig :: String -> Config
parseConfig = read

configToConnectInfo c = defaultConnectInfo {
    connectHost = host c
  , connectPort = fromIntegral $ port c
  , connectUser = user c
  , connectPassword = pass c
  , connectDatabase = dbName c
  }

runMySql connectInfo query = runResourceT . runNoLoggingT . withMySQLConn connectInfo $ runSqlConn query

createPersistInit :: Config -> IO PersistInit
createPersistInit config = do
  let conn = configToConnectInfo config
  let select = runMySql conn $ do
        createTables <- getMigration migrateAll
        fsSetUp <- FS.isSetUpFS
        return (and [null createTables, fsSetUp])
  let initDatabase = do
        runMySql conn . void $ runMigrationSilent migrateAll
        FS.initFS
  let dropDatabase = do
        runMySql conn $ do
          let name = dbName config
          rawExecute (fromString $ ("DROP DATABASE " ++ name)) []
          rawExecute (fromString $ ("CREATE DATABASE " ++ name)) []
        FS.removeFS
  return $! PersistInit {
      isSetUp = select
    , initPersist = initDatabase
    , tearDown = dropDatabase
    }

newtype Interpreter
  = Interpreter { unInt :: forall a . Persist a -> IO (Either [Char] a) }

createPersistInterpreter :: Config -> IO Interpreter
createPersistInterpreter config = do
  let connectInfo = configToConnectInfo config
  let run query = do
        result <- trySomeEx $ runMySql connectInfo query
        return $! either (Left . show) Right result
  return $! Interpreter run
  where
    trySomeEx :: IO a -> IO (Either SomeException a)
    trySomeEx = try

runInterpreter :: Interpreter -> Persist a -> IO (Erroneous a)
runInterpreter (Interpreter run) = run

