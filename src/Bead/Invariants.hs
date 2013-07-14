module Bead.Invariants where

import Control.Monad (join, mapM_)

-- * Invariants for testing

-- All module can define invariants and unit test in a
-- pure functional form. The convention is to name all the
-- invariants and unit tests in a module to 'invariants'
-- and 'unitTests'

newtype Invariants a = Invariants [(String,a -> Bool)]

newtype Invariants2 a b = Invariants2 [a -> b -> Bool]

newtype InvariantsM m a = InvariantsM [a -> m Bool]

newtype InvariantsM2 m a b = InvariantsM2 [(String, a -> b -> m Bool)]

newtype UnitTests = UnitTests [(String,Bool)]

newtype UnitTestsM m = UnitTestsM [(String,m Bool)]

-- Represents an named assertion for a function that produce a value
-- and the expected value of the result.
data Assertion a = Assertion {
    name     :: String
  , found    :: a
  , expected :: a
  }

assertionMap :: (String -> a -> a -> b) -> Assertion a -> b
assertionMap g (Assertion n f e) = g n f e

testAssertion :: (Eq a, Show a) => Assertion a -> IO ()
testAssertion = putStrLn . assertionMap check where
  check msg f e
    | f == e    = join [msg, ": passed"]
    | otherwise = join [msg, ": failed. Found:", show f, " Expected:", show e]

testAssertions :: (Eq a, Show a) => [Assertion a] -> IO ()
testAssertions = mapM_ testAssertion
