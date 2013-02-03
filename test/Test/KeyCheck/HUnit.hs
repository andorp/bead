module Test.KeyCheck.HUnit where

import Test.KeyCheck.Keyword
import Test.HUnit
import Test.Framework.Providers.HUnit

isTestFailed :: (Property () e) -> IO ()
isTestFailed (Exception _) = fail "Exception occured"
isTestFailed (Fail msg)    = fail msg
isTestFailed (Success _)   = return ()

-- keywordTestCase :: String -> Keyword e IO () -> Test
-- keywordTestCase name keyword = testCase name $ runKeyword keyword >>= isTestFailed

