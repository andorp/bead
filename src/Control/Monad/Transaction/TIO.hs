{-# LANGUAGE DeriveDataTypeable #-}
module Control.Monad.Transaction.TIO (
    TIO
  , step
  , stepM
  , hasNoRollback
  , throwEx
  , T.atomically
  ) where

import Data.Typeable
import Control.Monad.Transaction (Transaction)
import qualified Control.Monad.Transaction as T
import Control.Exception
import Control.Monad.Error
import Control.Exception (IOException)
import qualified Control.Exception as E

type TIO a = Transaction IOException IO a

-- * Connection between Errors and Exception

newtype UserException = UserException { msg :: Maybe String }
  deriving (Show, Typeable)

-- * IO Steps

step :: IO a -> IO () -> TIO a
step action reverse = T.stepEither (exceptionsOn action) reverse

stepM :: IO a -> IO () -> (a -> IO ()) -> TIO a
stepM action reverseBefore reverseAfter =
  T.stepEitherM (exceptionsOn action) reverseBefore reverseAfter

hasNoRollback :: IO a -> TIO a
hasNoRollback a = step a (return ())

throwEx :: IOException -> TIO a
throwEx e = hasNoRollback $ E.throwIO e

-- * Tools

exceptionsOn :: IO a -> IO (Either IOException a)
exceptionsOn a =
  seCatch
    (a >>= (return . Right))
    (return . Left)

seCatch :: IO a -> (IOException -> IO a) -> IO a
seCatch = E.catch


