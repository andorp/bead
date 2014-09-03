{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.TestCase where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.Data

data TestCaseValue
  = SimpleTestCase String
  | ZippedTestCase ByteString
  deriving (Eq, Show, Read)

testCaseValue
  simple
  zipped
  t = case t of
    SimpleTestCase x -> simple x
    ZippedTestCase x -> zipped x

withTestCaseValue t simple zipped = testCaseValue simple zipped t

-- Test Cases are for assignment and test script pair, test case
-- has name, description and a piece of code that will be subsctituated
-- during the evaluation of a submission
data TestCase = TestCase {
    tcName :: String -- The name of the test case
  , tcDescription :: String -- The short description of the test case
  , tcValue :: TestCaseValue -- ByteString -- The stored value of test cases
  , tcInfo  :: String -- Additional information which interpretation could change depending on the
                      -- type of the test case
  } deriving (Eq, Show, Read)

-- Template method for test case
testCaseCata
  tc -- Transformation of the TestCaseType
  f
  (TestCase name
            description
            value
            info)
  = f name description (tc value) info

-- | Template method for the test case with flipped arguments
withTestCase t tc f = testCaseCata tc f t

-- Applicative functor based TestCase value creation
testCaseAppAna name desc value info
  = TestCase <$> name <*> desc <*> value <*> info
