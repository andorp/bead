module Main where

import Control.Monad (join)

import Test.Framework (defaultMain)


-- Test cases

import qualified Test.Unit.Persistence.TestNoSQLDir
import qualified Test.Unit.Invariants
import qualified Test.UserStories.TestStories
import qualified Test.Quick.Persistence

tests args =
  join [
      (ifPresent "unit" Test.Unit.Invariants.tests)
    , (ifPresent "persist-unit"
         [ Test.UserStories.TestStories.tests
         , Test.Unit.Persistence.TestNoSQLDir.tests
         ])
    , (ifPresent "persist-quick"
         [ Test.Quick.Persistence.tests
         , Test.Quick.Persistence.massTests
         , Test.Quick.Persistence.complexTests
         ])
    ]
  where
    ifPresent a xs =
      if (elem a args)
        then xs
        else []

main = defaultMain (tests ["unit", "persist-unit", "no-persist-quick"])
