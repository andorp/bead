module Bead.Invariants where

-- * Invariants for testing

-- All module can define invariants and unit test in a
-- pure functional form. The convention is to name all the
-- invariants and unit tests in a module to 'invariants'
-- and 'unitTests'

newtype Invariants a = Invariants [(String,a -> Bool)]

newtype Invariants2 a b = Invariants2 [a -> b -> Bool]

newtype InvariantsM m a = InvariantsM [a -> m Bool]

newtype InvariantsM2 m a b = InvariantsM2 [a -> b -> m Bool]

newtype UnitTests = UnitTests [(String,Bool)]

newtype UnitTestsM m = UnitTestsM [(String,m Bool)]

