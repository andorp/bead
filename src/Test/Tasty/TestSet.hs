{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Tasty.TestSet (
    TestSet
  , buildTestTree
  , runTestSet
  , group
  , shrink
  , add
  , test
  , assertEquals
  , assertProperty
  , assertSatisfy
  , ioTest
  , equals
  , satisfies
  , Partition(..)
  , eqPartitions
  ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State as CMS

import qualified Test.HUnit as HUnit (assertEqual)
import           Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC


newtype TestSet a = TestSet { unTest :: (CMS.StateT [TestTree] Identity a) }
  deriving (Functor, Applicative, Monad, MonadState [TestTree])

buildTestTree :: TestName -> TestSet a -> TestTree
buildTestTree name = testGroup name . runIdentity . flip CMS.execStateT [] . unTest

runTestSet = defaultMain . buildTestTree ""

group :: TestName -> TestSet a -> TestSet ()
group name test = add (buildTestTree name test)

shrink :: TestName -> TestSet a -> TestSet b -> TestSet ()
shrink name tests shrinks = do
  group name tests
  group (name ++ "-shrinks") shrinks

type Message = String

add :: TestTree -> TestSet ()
add t = modify (\ts -> ts ++ [t])

test = add

assertEquals :: (Eq a, Show a) => TestName -> a -> a -> Message -> TestSet ()
assertEquals name expected found msg = add (HU.testCase name (HU.assertEqual msg expected found))

assertSatisfy :: TestName -> (a -> Bool) -> a -> Message -> TestSet ()
assertSatisfy name predicate value msg = add (HU.testCase name (HU.assertBool msg (predicate value)))

assertProperty :: (Show a) => TestName -> (a -> Bool) -> QC.Gen a -> Message -> TestSet ()
assertProperty name predicate generator _msg = add (QC.testProperty name (QC.forAll generator predicate))

-- IO related test suite

ioTest :: TestName -> IO a -> TestSet ()
ioTest name computation = add (HU.testCase name (void computation))

equals :: (Eq a, Show a, MonadIO io) => a -> a -> Message -> io ()
equals expected found msg = liftIO $ HUnit.assertEqual msg expected found

satisfies :: (MonadIO io) => a -> (a -> Bool) -> Message -> io ()
satisfies x p msg = liftIO $ HUnit.assertEqual msg True (p x)

data Partition a b = Partition TestName a b Message

partitionToTestTree f (Partition name x y msg) = assertEquals name y (f x) msg

eqPartitions :: (Eq b, Show b) => (a -> b) -> [Partition a b] -> TestSet ()
eqPartitions function = mapM_ (partitionToTestTree function)
