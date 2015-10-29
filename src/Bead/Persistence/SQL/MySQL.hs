{-# LANGUAGE OverloadedStrings #-}
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

runMySql pool query = runResourceT . runNoLoggingT $ runSqlPool query pool

runMySqlConn conn query = runResourceT . runNoLoggingT . withMySQLConn conn $ runSqlConn query

createPersistInit :: Config -> IO PersistInit
createPersistInit config = do
  let conn = configToConnectInfo config
  let dbname = dbName config
  let useDatabase = rawExecute (fromString $ ("USE " ++ dbname)) []
  let select = runMySqlConn conn $ do
        useDatabase
        createTables <- fmap (filter (not . isExceptionalMigration))
                             (getMigration migrateAll)
        fsSetUp <- FS.isSetUpFS
        return (and [null createTables, fsSetUp])
  let initDatabase = do
        runMySqlConn conn . void $ do
          useDatabase
          runMigrationSilent migrateAll
        FS.initFS
  let dropDatabase = do
        runMySqlConn conn $ do
          rawExecute (fromString $ ("DROP DATABASE " ++ dbname)) []
          rawExecute (fromString $ ("CREATE DATABASE " ++ dbname)) []
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
  pool <- runResourceT . runNoLoggingT $ createMySQLPool connectInfo 100
  let run query = do
        result <- trySomeEx $ runMySql pool query
        return $! either (Left . show) Right result
  return $! Interpreter run
  where
    trySomeEx :: IO a -> IO (Either SomeException a)
    trySomeEx = try

runInterpreter :: Interpreter -> Persist a -> IO (Erroneous a)
runInterpreter (Interpreter run) = run

-- * Migration fix

isExceptionalMigration cmd = elem cmd migrationCommandExceptions

-- If a field is created with longtext utf8 modifier, the persist migrates
-- with alter table, it is probably an issue.
migrationCommandExceptions =
  [ "ALTER TABLE `assignment` CHANGE `description` `description` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `comment` CHANGE `text` `text` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `evaluation` CHANGE `result` `result` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `evaluation` CHANGE `written` `written` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `feedback` CHANGE `info` `info` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `notification` CHANGE `message` `message` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `submission` CHANGE `simple` `simple` longtext character set utf8 NULL"
  , "ALTER TABLE `test_case` CHANGE `simple_value` `simple_value` longtext character set utf8 NULL"
  , "ALTER TABLE `test_script` CHANGE `notes` `notes` longtext character set utf8 NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `script` `script` longtext character set utf8 NOT NULL"
  ]
