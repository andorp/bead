{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Transaction (
    module CME
  , Transaction
  , step
  , step'
  , atomically
  ) where

import Control.Applicative
import Control.Monad.Trans 
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Error as CME
import Control.Exception

-- * Rollback action

newtype Rollback m = Rollback (m ())

run :: Rollback m -> m ()
run (Rollback m) = m

-- * Transaction Monad

type TransCMES e m a = CME.ErrorT e (CMS.StateT (Rollback m) m) a

newtype Transaction e m a = Transaction {
    unTrans :: TransCMES e m a
  } deriving (Functor, Applicative, Monad,
              CMS.MonadState (Rollback m),
              CME.MonadError e)

runTransaction :: (CME.Error e, Monad m) => Transaction e m a -> m (Either e a, Rollback m)
runTransaction = startStateFrom nullRevStep
               . CME.runErrorT
               . unTrans

startStateFrom :: (Monad m) => s -> CMS.StateT s m a -> m (a,s)
startStateFrom = flip CMS.runStateT

-- * Rollback operation

nullRevStep :: (Monad m) => Rollback m
nullRevStep = Rollback (return ())

nextRevStep :: (Monad m) => m () -> Rollback m -> Rollback m
nextRevStep s (Rollback m) = Rollback (s >> m)

-- * Transaction helpers

eitherStep :: (CME.Error e ,Monad m) => Either e a -> TransCMES e m a
eitherStep (Left e)  = CME.throwError e
eitherStep (Right v) = return v

transStep :: (CME.Error e, Monad m) => CME.ErrorT e m a -> Transaction e m a
transStep action = Transaction $ do
  let me = CME.runErrorT action 
  mex <- lift $ lift $ me
  eitherStep mex

transStep' :: (CME.Error e, Monad m) => m (Either e a) -> Transaction e m a 
transStep' action = Transaction $ do
  mex <- lift $ lift $ action
  eitherStep mex

-- * Transactional steps

-- | A transactional step contains a step that can produce an error or calculate
--   a value and a reverse operation for that step. If an 'e' exception occurs
--   the reverse step will be executed.
step' :: (CME.Error e, Monad m) => m (Either e a) -> m () -> Transaction e m a
step' action reverse = do
  CMS.modify (nextRevStep reverse)
  transStep' action

-- | A transactional step contains a step that can produce an error or calculate
--   a value and a reverse operation for that step. If an 'e' exception occurs
--   the reverse step will be executed.
step :: (CME.Error e, Monad m) => CME.ErrorT e m a -> m () -> Transaction e m a
step action reverse = do
  CMS.modify (nextRevStep reverse)
  transStep action

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
