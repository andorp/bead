module Test.KeyCheck.Keyword (
    Property(..)
  , Keyword
  , runKeyword
  , keywordCatch
  , mkKeyword
  , mkKeyword'
  , step
  , step'
  , stepCleanUp
  , checkPoint
  , beforeTest
  , afterTest
  , bracketTest
  ) where

import Control.Monad.Trans
import Control.Monad (liftM)

data Property a e
  = Success a
  | Fail String
  | Exception e
  deriving (Show, Eq)

data Keyword e m a = Keyword {
    task        :: m (Property a e)
  , cleanUp     :: m ()
  , onException :: e -> m ()
  }

keyword t = Keyword t (return ()) (\_ -> return ())

runKeyword :: (Monad m) => (Keyword e m a) -> m (Property a e)
runKeyword k = do
  a <- task k
  case a of
    Fail m       -> cleanUp k        >> return (Fail m)
    Exception e' -> onException k e' >> cleanUp k >> return (Fail "")
    Success a'   -> return (Success a')

runKeyword2 :: (Monad m) => (Keyword e m a) -> (a -> Keyword e m b) -> m (Property b e)
runKeyword2 k1 k2 = do
  r <- runKeyword k1
  case r of
    Fail m -> return $ Fail m
    Success a' -> do
      runKeyword (k2 a')

runKeyword3 :: (Monad m) => (Keyword e m a) -> (a -> Keyword e m b) -> Keyword e m b
runKeyword3 k1 k2 = keyword $ runKeyword2 k1 k2

instance (Monad m) => Monad (Keyword e m) where
  return x = keyword $ return (Success x)
  k1 >>= k2 = runKeyword3 k1 k2

instance MonadTrans (Keyword e) where
  lift m = keyword $ liftM Success m

keywordCatch
  :: (Monad m)
  => m a
  -> (a -> Bool)
  -> m ()
  -> (e -> m ())
  -> (m (Property a e) -> (e -> m (Property a e)) -> m (Property a e))
  -> Keyword e m a
keywordCatch action decision clean exHandler catch =
  Keyword {
      task = catch
        (do x <- action
            case decision x of
              True  -> return $ Success undefined
              False -> return $ Fail "")
        (\ex -> return $ Exception ex)
    , cleanUp     = clean
    , onException = exHandler
    }
               
  
mkKeyword :: (Monad m) => m a -> (a -> Bool) -> m () -> (e -> m ()) -> Keyword e m a
mkKeyword m f c e =
  let t = do a <- m
             case f a of
               False -> return $ Fail ""
               True  -> return $ Success a
  in Keyword t c e

mkKeyword' :: (Monad m) => m (Property a e) -> m () -> (e -> m ()) -> Keyword e m a
mkKeyword' t c e = Keyword t c e

step' :: (Monad m) => m a -> (a -> Bool) -> Keyword e m a
step' m f =
  keyword $ do
    a <- m
    case (f a) of
      False -> return $ Fail ""
      True  -> return $ Success a

step :: Monad m => m a -> m Bool -> Keyword e m a
step s c =
  keyword $ do
    x <- s
    valid <- c
    case valid of
      True -> return $ Success x
      False -> return $ Fail ""

stepCleanUp :: Monad m => m a -> m Bool -> m () -> Keyword e m a
stepCleanUp st ch cl = (step st ch) { cleanUp = cl }

checkPoint :: (Monad m) => m Bool -> String -> Keyword e m ()
checkPoint check msg =
  keyword $ do
    c <- check
    case c of
      False -> return $ Fail msg
      True  -> return $ Success ()

beforeTest :: (Monad m) => Keyword e m () -> Keyword e m a -> Keyword e m a
beforeTest b t = b >> t

afterTest :: (Monad m) => Keyword e m a -> Keyword e m () -> Keyword e m a
afterTest t a = do
  x <- t
  a
  return x

bracketTest :: (Monad m) => Keyword e m () -> Keyword e m a -> Keyword e m () -> Keyword e m a
bracketTest b t a = beforeTest b (afterTest t a)

