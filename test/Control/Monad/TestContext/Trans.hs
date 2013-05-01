{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.TestContext.Trans where

import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error

import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.Error  as CME

newtype TestContextT e m a = TestContextT {
    unTC :: ReaderT [ErrorT e m ()] (ErrorT e m) a
  } deriving (
      CMR.MonadReader [ErrorT e m ()],
      CME.MonadError e
    )

instance (Error e, Monad m) => Monad (TestContextT e m) where
  return = TestContextT . return
  m >>= k = TestContextT $ do
    x <- unTC m
    context <- ask
    lift . sequence $ context
    unTC (k x)

liftTC :: (Error e, Monad m) => m a -> TestContextT e m a
liftTC = TestContextT . lift . lift

instance (Error e) => MonadTrans (TestContextT e) where
  lift = liftTC

runTC :: (Error e, Monad m) => TestContextT e m a -> m (Either e a)
runTC = runErrorT . (flip runReaderT []) . unTC
