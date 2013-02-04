{-# LANGUAGE DeriveDataTypeable #-}
module Control.Monad.Transaction.TIO (
    TIO
  , step
  , T.atomically
  ) where

import Data.Typeable
import Control.Monad.Transaction (Transaction)
import qualified Control.Monad.Transaction as T
import Control.Exception
import Control.Monad.Error
import Control.Exception as E

type TIO a = Transaction IOException IO a

-- * Connection between Errors and Exception

newtype UserException = UserException { msg :: Maybe String }
  deriving (Show, Typeable)

-- * IO Step
  
step :: IO a -> IO () -> TIO a
step action reverse = T.stepEither (exceptionsOn action) reverse

-- * Tools

exceptionsOn :: IO a -> IO (Either IOException a)
exceptionsOn a = 
  seCatch
    (a >>= (return . Right))
    (return . Left)

seCatch :: IO a -> (IOException -> IO a) -> IO a
seCatch = E.catch


