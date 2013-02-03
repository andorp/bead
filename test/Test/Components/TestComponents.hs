module Bead.Invariants.TestComponents where

import Bead.Domain.Entities
import Bead.Persistence.Persist
import Bead.Controller.ServiceContext
import Bead.Controller.Logging

import Prelude hiding (log)
import Data.Maybe (isJust)
import Data.Map (Map(..))
import qualified Data.Map   as Map
import qualified Data.IORef as Ref


mkMemoryPersist :: IO Persist
mkMemoryPersist = do
  p <- Ref.newIORef (Map.empty :: Map (Username, Password) User)

      -- :: Username -> Password -> IO Bool
  let mDoesUserExist uname pwd = do
        users <- Ref.readIORef p
        return $ isJust $ Map.lookup (uname,pwd) users

      -- :: Username -> Password -> IO (Erroneous (Role, String))
      mPersonalInfo uname pwd = do
        users <- Ref.readIORef p
        case Map.lookup (uname,pwd) users of
          Nothing -> return $ Left $ "User not found: " ++ show uname
          Just u  -> return $ Right (u_role u, u_name u)

      -- :: Username -> Password -> Password -> IO (Erroneous ())
      mUpdatePwd uname old new = do
        users <- Ref.readIORef p
        let users' = case Map.lookup (uname,old) users of
              Nothing -> users
              Just u  -> Map.insert (uname,new) u $ Map.delete (uname,old) users
        Ref.writeIORef p users'
        return $ Right ()

      -- :: User -> Password -> IO (Erroneous ())
      mSaveUser user pwd = do
        Ref.modifyIORef p $ Map.insert (u_username user,pwd) user
        return $ Right ()

  return $ Persist {
      saveUser      = mSaveUser
    , doesUserExist = mDoesUserExist
    , personalInfo  = mPersonalInfo
    , updatePwd     = mUpdatePwd

    , saveGroup = undefined
    }

testLog ERROR msg = error msg
testLog _     _   = return ()

testLogger = Logger {
    log = testLog
  }

mkMemoryServiceContext :: IO ServiceContext
mkMemoryServiceContext = do
  p <- mkMemoryPersist
  uc <- ioUserContainer
  return $ serviceContext p uc testLogger
