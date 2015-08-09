{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Bead.Persistence.SQL.Init where

import           Control.Applicative ((<$>))
import           Control.Exception
import           Data.List (inits)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory (getDirectoryContents, removeFile)

import           Database.Persist.Sqlite

import           Bead.Domain.Types (Erroneous(..))
import           Bead.Persistence.Initialization
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS

-- * Persistence initialization

data Config = Config {
    sqliteDatabase      :: Text
  , sqliteWriteAheadLog :: Bool
  } deriving (Show, Read)

type ConfigAlgebra c = (Text -> Bool -> c)

configAlgebra :: ConfigAlgebra c -> Config -> c
configAlgebra f (Config db wal) = f db wal

connectionString :: Config -> Text
connectionString = configAlgebra $ \db wal ->
  Text.concat [if wal then "WAL=on" else "WAL=off", " ", db]

parseConfig :: String -> Config
parseConfig = read

defaultConfig = Config "bead.db" False

createPersistInit :: Config -> IO PersistInit
createPersistInit cfg = do
  let database = sqliteDatabase cfg
      conn = connectionString cfg
      select = runSqlite conn $ do
        createTables <- getMigration migrateAll
        fsSetUp <- FS.isSetUpFS
        return (and [null createTables, fsSetUp])
      initDatabase = do
        runSqlite conn (void $ runMigrationSilent migrateAll)
        FS.initFS
      dropDatabase = do
        dbfiles <- filter (isInfixOf (Text.unpack database)) <$> getDirectoryContents "."
        mapM_ removeFile dbfiles
        FS.removeFS
  return $! PersistInit {
      isSetUp = select  -- Try to migrate the database allways
    , initPersist = initDatabase
    , tearDown = dropDatabase
    }
  where
    isInfixOf :: (Eq a) => [a] -> [a] -> Bool
    isInfixOf pattern = or . map (pattern ==) . inits

newtype Interpreter
  = Interpreter { unInt :: forall a . Persist a -> IO (Either [Char] a) }

createPersistInterpreter :: Config -> IO Interpreter
createPersistInterpreter cfg = do
  let conn = connectionString cfg
      run command = do
        result <- trySomeEx $ runSqlite conn command
        return $! either (Left . show) Right result
  return $! Interpreter run
  where
    trySomeEx :: IO a -> IO (Either SomeException a)
    trySomeEx = try

runInterpreter :: Interpreter -> Persist a -> IO (Erroneous a)
runInterpreter (Interpreter run) = run

