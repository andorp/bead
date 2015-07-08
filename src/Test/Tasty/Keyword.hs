{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Keyword (
    Keyword
  , runKeyword
  , Context(..)
  , Action
  , safeAction
  , safeActionRollback
  , action
  , actionRollback
  , Interpretation(..)
  , step
  , info
  , assertEquals
  , satisfies
  ) where

import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Error as CME
import qualified Control.Monad.Trans.State as CMS

{-
Every testeable system with test steps can be represented by keywords.
Keywords are composable actions. These actions has optional backup steps.

Keywords represents a script which can be evaluated with the given
context of the interpretation of each keyword. This way multiple
reusable scripts can be written in the haskell do notation.
-}

data Keyword k a where
  KRet    :: a   -> Keyword k a
  KWord   :: k a -> Keyword k a
  KInfo   :: k a -> Keyword k a
  KLog    :: String -> Keyword k ()
  KAssert :: Bool -> String -> Keyword k ()
  KBind   :: Keyword k a -> (a -> Keyword k b) -> Keyword k b

instance Monad (Keyword k) where
  return  = KRet
  m >>= k = KBind m k

-- | The 'k' step that will be interpreted in some action.
step :: k a -> Keyword k a
step = KWord

-- | The 'k' information what has no rollback
info :: k a -> Keyword k a
info = KInfo

-- | Assertion on the first value, the assertion fails if the
-- first value is False. After the failure the revering actions
-- will be computed.
assertion :: Bool -> String -> Keyword k ()
assertion = KAssert

-- | Logs a message
log :: String -> Keyword k ()
log = KLog

-- | The action consits of a computation that can fail
-- and possibly a revert action.
newtype Action m e a = Action (m (Either e a), Maybe (a -> m ()))

-- | The interpretation of a 'k' basic keyword consists of a pair
-- the first is a computation which computes an error result or a
-- unit, and a revert action of the computation.
newtype Interpretation k m e = Interpretation { unInterpret :: forall a . k a -> Action m e a }

safeAction :: (Functor m, Monad m, Error e) => m a -> Action m e a
safeAction action = Action (fmap Right action, Nothing)

safeActionRollback :: (Functor m, Monad m, Error e) => m a -> (a -> m b) -> Action m e a
safeActionRollback action rollback = Action (fmap Right action, Just (void . rollback))

-- The action has no rollback but the action can fail.
action :: (Functor m, Monad m, Error e) => m (Either e a) -> Action m e a
action act = Action (act, Nothing)

-- The action has a given rollback and the action can fail
actionRollback :: (Functor m, Monad m, Error e) => m (Either e a) -> (a -> m b) -> Action m e a
actionRollback action rollback = Action (action, Just (void . rollback))

-- | The keyword evaluation context consists of an interpretation
-- function, which turns every 'k' command into a monadic computation
data Context k m e = Context {
    keywordInterpretation :: Interpretation k m e
  , logInterpretation     :: String -> m ()
  }

newtype Interpreter m e a = KI { unKI :: CME.ErrorT e (CMS.StateT [m ()] m) a }
  deriving (Monad, MonadState [m ()], MonadError e)

evalStage0 :: (Monad m, Error e) => Context k m e -> Keyword k a -> Interpreter m e a
evalStage0 ctx (KRet a)  = return a

evalStage0 ctx (KInfo k) = do
  let Action (step,revert) = (unInterpret $ keywordInterpretation ctx) k
  x <- KI . lift $ lift step
  case x of
    Left e -> throwError e
    Right y -> return y

evalStage0 ctx (KLog m) = KI . lift . lift $ logInterpretation ctx m

evalStage0 ctx (KWord k) = do
  let Action (step,revert) = (unInterpret $ keywordInterpretation ctx) k
  x <- KI . lift $ lift step
  case x of
    Left e -> throwError e
    Right y -> do
      maybe (return ()) (\r -> modify (r y:)) revert
      return y

evalStage0 ctx (KBind m k) = do
  x <- evalStage0 ctx m
  evalStage0 ctx (k x)

evalStage0 ctx (KAssert a msg) = unless a . throwError $ strMsg msg

evalStage1 :: (Functor m, Monad m, Error e) => Interpreter m e a -> m (Either e a)
evalStage1 m = do
  (result, revert) <- flip CMS.runStateT [] . CME.runErrorT $ unKI m
  case result of
    Left err -> do sequence_ revert
                   return (Left err)
    Right a -> return (Right a)

-- | The 'runKeyword' interprets the given keyword in a computational context, and
-- reverts the steps if any error occurs.
runKeyword :: (Functor m, Monad m, Error e) => Context k m e -> Keyword k a -> m (Either e a)
runKeyword ctx k = evalStage1 $ evalStage0 ctx k

-- * Helpers

voide :: (Functor m, Monad m, Error e) => m a -> m (Either e ())
voide m = do m >> return (Right ())



-- * Assertions

-- | Checks if the found value equals to the expected one, if it differs
-- the test will fail
assertEquals :: (Show a, Eq a) => a -> a -> String -> Keyword k ()
assertEquals expected found msg =
  assertion (found == expected) (concat [msg, " found: ", show found, " expected: ", show expected])

-- | Checks if the found value satisfies the given predicate, it not the test will fail
satisfies :: (Show a) => a -> (a -> Bool) -> String -> Keyword k ()
satisfies value pred msg =
  assertion (pred value) (concat [msg, " value: ", show value, " does not satisfies the predicate."])

