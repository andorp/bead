{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Bead.Persistence.SQL.Init where

import           Control.Exception
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory (removeFile)

import           Database.Persist.Sqlite

import           Bead.Domain.Types (Erroneous(..))
import           Bead.Persistence.Initialization
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS

-- * Persistence initialization

data Config = Config {
    sqliteDatabase :: Text
  } deriving (Show, Read)

parseConfig :: String -> Config
parseConfig = read

defaultConfig = Config "bead.db"

createPersistInit :: Config -> IO PersistInit
createPersistInit cfg = do
  let database = sqliteDatabase cfg
      select = runSqlite database $ do
        createTables <- getMigration migrateAll
        fsSetUp <- FS.isSetUpFS
        return (and [null createTables, fsSetUp])
      initDatabase = do
        runSqlite database (void $ runMigrationSilent migrateAll)
        FS.initFS
      dropDatabase = do
        removeFile (Text.unpack database)
        FS.removeFS
  return $! PersistInit {
      isSetUp = select  -- Try to migrate the database allways
    , initPersist = initDatabase
    , tearDown = dropDatabase
    }

newtype Interpreter
  = Interpreter { unInt :: forall a . Persist a -> IO (Either [Char] a) }

createPersistInterpreter :: Config -> IO Interpreter
createPersistInterpreter cfg = do
  let database = sqliteDatabase cfg
      run command = do
        result <- trySomeEx $ runCmd database command
        return $ case result of
          Left ex -> Left $ show ex
          Right x -> Right x
  return (Interpreter (runCmd (sqliteDatabase cfg)))
  where
    trySomeEx :: IO a -> IO (Either SomeException a)
    trySomeEx = try

    runCmd :: Text -> Persist a -> IO (Either String a)
    runCmd db = fmap Right . runSqlite db

runInterpreter :: Interpreter -> Persist a -> IO (Erroneous a)
runInterpreter (Interpreter run) = run

