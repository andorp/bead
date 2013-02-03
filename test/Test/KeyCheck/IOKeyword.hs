module Test.KeyCheck.IOKeyword where

import Test.KeyCheck.Keyword

import Control.Monad.Trans
import Control.Exception as E

ioStep :: IO a -> (a -> Bool) -> IO () -> (SomeException -> IO ()) -> Keyword SomeException IO a
ioStep action decision clean exHandler = keywordCatch action decision clean exHandler E.catch

