module Main where

import Control.Monad (join)
import Test.Framework (defaultMain)

-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories
import qualified Test.Quick.Persistence

tests = join [
     Test.Unit.Invariants.tests
   , [ Test.UserStories.TestStories.tests ,
       Test.Unit.Persistence.TestNoSQLDir.tests
     , Test.Quick.Persistence.tests
     , Test.Quick.Persistence.massTests
     , Test.Quick.Persistence.complexTests
     ]
   ]

main = defaultMain tests
