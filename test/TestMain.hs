module Main where

import Control.Monad (join)
import Test.Framework (defaultMain)

-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories

tests = join
   [ [ Test.Unit.Persistence.TestNoSQLDir.tests
     , Test.UserStories.TestStories.tests ]
   , Test.Unit.Invariants.tests
   ]

main = defaultMain tests
