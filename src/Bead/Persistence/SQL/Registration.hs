{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Registration where

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import           Test.Themis.Test (ioTest)
import           Test.Themis.Keyword.Encaps
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
  let time = read "2014-06-09 12:55:27.959203 UTC"
      reg = Domain.UserRegistration "username" "email" "name" "token" time
  ioTest "Save and load user registration" $ runSql $ do
    dbStep initDB
    k <- dbStep $ saveUserReg reg
    reg' <- dbStep $ loadUserReg k
    assertEquals reg reg' "User registration was saved and load incorrectly."

#endif

