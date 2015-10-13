module Test.Tasty.BeadHandler where

import           Control.Monad (when)

import           Snap.Test
import           Snap.Snaplet.Test

import           Bead.View.BeadContext (BeadHandler)
import           Bead.View.MockContextInit
import           Test.Tasty.TestSet
import qualified Test.Tasty.HUnit as HUnit

handlerTest :: TestName
            -> RequestBuilder IO ()
            -> BeadHandler a
            -> String
            -> TestSet ()
handlerTest name request handler message =
  ioTest name $ do
    init <- mockServiceInit
    result <- evalHandler Nothing request handler init
    when (isLeft result) . fail $ show result

loginPostRequest :: RequestBuilder IO ()
loginPostRequest = do
  postUrlEncoded "/login" Map.empty

emptyParams :: Params
emptyParams = Map.empty
