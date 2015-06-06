module Test.Tasty.Encaps (
    EncapsKeyword
  , key
  , encapsContext
  , module Test.Tasty.Keyword
  ) where

{-
Simply encapsulates the monadic computation, into the keyword framework.
The logging is supressed.
-}

import Control.Monad.Error

import Test.Tasty.Keyword

-- | Encapsulates the monadic computation
newtype EncapsKeyword m a = EncapsKeyword { unEK :: m a }

-- | The context for the encaplu
type EncapsContext m = Context (EncapsKeyword m) m IOError

-- | Encapsulates the monadic computation
key :: (Functor m, Monad m) => m a -> EncapsKeyword m a
key = EncapsKeyword

-- | The interpretation context for the encapsulated keyword,
-- gets the computation out from the encapsulation and forgets
-- all the logging.
encapsContext :: (Functor m, Monad m) => EncapsContext m
encapsContext = Context {
    keywordInterpretation = Interpretation (safeAction . unEK)
  , logInterpretation     = const (return ())
  }

