module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

-- Bead imports

import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDir

-- Utils

import System.Directory (removeDirectoryRecursive)

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_exercise
  , clean_up
  ]

persist = noSqlDirPersist

test_initialize_persistence = testCase "Initialize NoSQLDir persistence layer" $ do
  setUp <- isPersistenceSetUp persist
  assertBool "Persistence was set up" (not setUp)
  initPersistence persist
  setUp <- isPersistenceSetUp persist
  assertBool "Settin up persistence was failed" setUp

test_create_exercise = testCase "Save an exercise" $ do
  e <- saveExercise persist (Exercise "This is an exercise")
  case e of
    Left e -> error e
    Right k -> return ()

clean_up = testCase "Cleaning up" $ do
  -- We use background knowledge, to clean up
  removeDirectoryRecursive "data"
