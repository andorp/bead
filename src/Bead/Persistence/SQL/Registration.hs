{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Registration where

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, equals)
#endif

-- * Registration

-- Save the user registration information which is created at the time, when the
-- user starts a new registration
saveUserReg :: Domain.UserRegistration -> Persist Domain.UserRegKey
saveUserReg reg = do
  key <- insert (fromDomainValue reg)
  return $! toDomainKey key

-- Loads the user registration
loadUserReg :: Domain.UserRegKey -> Persist Domain.UserRegistration
loadUserReg key = do
  value <- get (toEntityKey key)
  return $
    maybe (persistError "loadUserReg" $ "no registration key was found:" ++ show key)
          toDomainValue
          value

#ifdef TEST

userRegistrationTests = do
  ioTest "Save and load user registration" $ runSql $ do
    initDB
    k <- saveUserReg reg
    reg' <- loadUserReg k
    equals reg reg' "User registration was saved and load incorrectly."

#endif

