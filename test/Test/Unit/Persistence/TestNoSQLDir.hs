module Test.Unit.Persistence.TestNoSQLDir where

-- Test imports

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

-- Bead imports

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Persistence.NoSQLDir

-- Utils

import System.Directory (removeDirectoryRecursive)

tests = testGroup "Persistence tests" [
    test_initialize_persistence
  , test_create_exercise
  , test_create_load_exercise
  , test_create_group
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

test_create_load_exercise = testCase "Create and load exercise" $ do
  k <- liftE $ saveExercise persist (Exercise "This is an exercise")
  ks <- liftE $ filterExercises persist (\_ _ -> True)
  assertBool "Readed list of exercises was empty" (length ks > 0)
  assertBool "Written key was not in the list" (elem k (map fst ks))

test_create_group = testCase "Create Course and Group" $ do
  ck <- liftE $ saveCourse persist (Course (CourseCode "code") "name" "desc")
  gk <- liftE $ saveGroup persist ck (Group (GroupCode "gcode") "gname" "gdesc")
  gks <- liftE $ groupKeysOfCourse persist ck
  assertBool "Registered group was not found in the group list" (elem gk gks)

clean_up = testCase "Cleaning up" $ do
  -- We use background knowledge, to clean up
  removeDirectoryRecursive "data"
  return ()

-- * Tools

liftE :: IO (Erroneous a) -> IO a
liftE m = do
  x <- m
  case x of
    Left e -> error e
    Right y -> return y
