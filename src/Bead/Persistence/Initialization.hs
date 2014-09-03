{-# LANGUAGE Rank2Types #-}
module Bead.Persistence.Initialization where

-- | Persistence init is a collection of function that checks if the persistence
-- is setted up correctly, inicializes the persistent layer when it is not set,
-- and tears down the database
data PersistInit = PersistInit {
    isSetUp     :: IO Bool
  , initPersist :: IO ()
  , tearDown    :: IO ()
  }
