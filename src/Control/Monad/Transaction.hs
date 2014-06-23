{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Transaction (
    module CME
  , Transaction
  , step
  , stepEither
  , stepEitherM
  , atomically
  ) where

import           Control.Applicative
import qualified Control.Monad.Error as CME
import           Control.Monad.IO.Class
import qualified Control.Monad.State as CMS
import           Control.Monad.Trans

-- * Rollback action

newtype Rollback m = Rollback [m ()]

run :: (Monad m) => Rollback m -> m ()
run (Rollback ms) = foldr1 (>>) ms

-- * Transaction Monad

type TransCMES e m a = CME.ErrorT e (CMS.StateT (Rollback m) m) a

newtype Transaction e m a = Transaction {
    unTrans :: TransCMES e m a
  } deriving (Functor, Applicative, Monad, MonadIO,
              CMS.MonadState (Rollback m),
              CME.MonadError e)

runTransaction :: (CME.Error e, Monad m) => Transaction e m a -> m (Either e a, Rollback m)
runTransaction
  = startStateFrom emptyUndo
  . CME.runErrorT
  . unTrans

startStateFrom :: (Monad m) => s -> CMS.StateT s m a -> m (a,s)
startStateFrom = flip CMS.runStateT

-- * Rollback operation

emptyUndo :: (Monad m) => Rollback m
emptyUndo = Rollback [return ()]

addUndo :: (Monad m) => m () -> Rollback m -> Rollback m
addUndo s (Rollback m) = Rollback (s:m)

-- * Transaction helpers

eitherStep :: (CME.Error e, Monad m) => Either e a -> TransCMES e m a
eitherStep (Left e)  = CME.throwError e
eitherStep (Right v) = return v

transactionStep :: (CME.Error e, Monad m) => CME.ErrorT e m a -> Transaction e m a
transactionStep m = Transaction $ do
  mex <- lift . lift . CME.runErrorT $ m
  eitherStep mex

transactionStepEither :: (CME.Error e, Monad m) => m (Either e a) -> Transaction e m a
transactionStepEither m = Transaction $ do
  mex <- lift . lift $ m
  eitherStep mex

-- * Transactional steps

-- | A transactional step contains a step that can produce an error or calculate
--   a value and a reverse operation for that step. If an @e@ exception occurs
--   it will be undo
stepEither :: (CME.Error e, Monad m) => m (Either e a) -> m () -> Transaction e m a
stepEither m inverse = do
  CMS.modify (addUndo inverse)
  transactionStepEither m

-- | The inverse step of the transactional step, can be calculated when the
--   operational step is done
stepEitherM :: (CME.Error e, Monad m) => m (Either e a) -> m () -> (a -> m ()) -> Transaction e m a
stepEitherM m inverseBefore inverseAfter = do
  CMS.modify (addUndo inverseBefore)
  x <- transactionStepEither m
  CMS.modify (addUndo (inverseAfter x))
  return x

-- | A transactional step contains a step that can produce an error or calculate
--   a value and a reverse operation for that step. If an @e@ exception occurs
--   it will be undone.
step :: (CME.Error e, Monad m) => CME.ErrorT e m a -> m () -> Transaction e m a
step m inverse = do
  CMS.modify (addUndo inverse)
  transactionStep m

-- * Run

-- | It runs atomically a transactional operation. If an exception occurs
--   the rollback steps will be executed in reverse order.
atomically :: (CME.Error e, Monad m) => Transaction e m a -> m (Either e a)
atomically t = do
  x <- runTransaction t
  case x of
    (Right v,_) -> return $ Right v
    (Left e,rollback) -> do
      run rollback
      return $ Left e
