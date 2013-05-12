module Test.WebDriver.TestRunner where

import Test.WebDriver
import Test.WebDriver.PageObject
import Data.AndOr
import Control.Applicative ((<$>))

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

type TestResultSet = Set (TestCase (Run Result))

data Run a
  = Run a
  | Skipped
  deriving (Show, Eq, Ord)

class IsRun r where
  isSkipped :: r a -> Bool
  isRun     :: r a -> Bool

instance IsRun Run where
  isSkipped Skipped = True
  isSkipped _       = False

  isRun (Run _) = True
  isRun _       = False

instance (IsResult r) => IsResult (Run r) where
  isFailure Skipped = False
  isFailure (Run r) = isFailure r

  isPassed Skipped = False
  isPassed (Run r) = isPassed r

isTcSkipped :: TestCase (Run Result) -> Bool
isTcSkipped = isSkipped . tcValue

isTcFailure :: TestCase (Run Result) -> Bool
isTcFailure = isFailure . tcValue



runTestCase :: TestCase (TWD a) -> WD (TestCase (Run Result))
runTestCase (TestCase (name, t)) = mkTestCase name . Run <$> runT t

skipTestCase :: TestCase (TWD a) -> WD (TestCase (Run Result))
skipTestCase (TestCase (name,_)) = return $ mkTestCase name Skipped

skipTCSet :: TestCase (TWD a) -> WD (Set (TestCase (Run Result)))
skipTCSet t = Set.singleton <$> skipTestCase t



traverse :: AndOr (TestCase (TWD a)) -> WD (TestResultSet)
traverse (Leaf tc) = do
  r <- runTestCase tc
  return (Set.singleton r)

traverse (And tc trees) = do
  rs <- findFirstFailed trees
  r  <- Set.singleton <$> runTestCase tc
  return . Set.unions $ (r:rs)

traverse (Or tc trees) = do
  rs <- runAll trees
  r  <- Set.singleton <$> runTestCase tc
  return . Set.unions $ (r:rs)

-- * Helpers

data Results
  = AllPassed
  | SomeSkipped
  | SomeFailed

setfind :: (Ord a) => (a -> Bool) -> Set a -> Maybe a
setfind f = listToMaybe . Set.toList . Set.filter f

results :: TestResultSet -> Results
results s = case setfind isTcSkipped s of
  Just _  -> SomeSkipped
  Nothing -> case setfind isTcFailure s of
               Just _  -> SomeFailed
               Nothing -> AllPassed

findFirstFailed :: [AndOr (TestCase (TWD a))] -> WD [TestResultSet]
findFirstFailed []     = return []
findFirstFailed (t:ts) = do
  r <- traverse t
  case results r of
    AllPassed   -> (r:) <$> (findFirstFailed ts)
    SomeSkipped -> (r:) <$> (mapM skipAllCases ts)
    SomeFailed  -> (r:) <$> (mapM skipAllCases ts)

runAll :: [AndOr (TestCase (TWD a))] -> WD [TestResultSet]
runAll = mapM traverse

skipAllCases :: AndOr (TestCase (TWD a)) -> WD TestResultSet
skipAllCases (Leaf tc) = skipTCSet tc
skipAllCases (And tc trees) = do
  r  <- skipTCSet tc
  rs <- mapM skipAllCases trees
  return . Set.unions $ (r:rs)
skipAllCases (Or tc trees) = do
  r  <- skipTCSet tc
  rs <- mapM skipAllCases trees
  return . Set.unions $ (r:rs)
