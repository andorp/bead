module Control.Monad.Transaction.IO (
    module Control.Exception
  , TRS
  , step
  , atomically
  ) where

import Data.IORef
import Control.Applicative
import Control.Exception as E

-- * Simple IO Transactions

-- Callback on exception
newtype TRS a = TRS (IORef (IO ()) -> IO a)

-- * Helper function and Functor, Applicative, Monad instances

unTRS :: TRS a -> IORef (IO ()) -> IO a
unTRS (TRS f) = f

instance Functor TRS where
  fmap f (TRS t) = TRS (fmap f . t)

instance Applicative TRS where
  pure = TRS . const . pure
  TRS tf <*> TRS tx = TRS $ \r -> tf r <*> tx r

instance Monad TRS where
  return = pure
  TRS m >>= k = TRS $ \r -> do
    x <- m r
    unTRS (k x) r

-- * Step and run functions

step :: IO a -> IO () -> TRS a
step action reverse = TRS $ \r -> do
  modifyIORef r (reverse >>)
  action
  
atomically :: TRS a -> IO (Either SomeException a)
atomically t = do
  ref <- newIORef (return ())
  (unTRS t ref >>= (return . Right)) `seCatch` \e ->
    do rollback <- readIORef ref
       rollback
       return $ Left e

-- * Tools

seCatch :: IO a -> (SomeException -> IO a) -> IO a
seCatch = E.catch
