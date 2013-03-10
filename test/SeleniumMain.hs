module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Network.URI
import Test.Selenium.Server

-- Usage: main localhost:8000 localhost
main = do
  args <- getArgs
  case args of
    [beadAddress, seleniumAddress] -> do
      let beadUri = fromJust . parseURI $ beadAddress
      withSelenium
        (mkSeleniumRCSession seleniumAddress Firefox beadUri)
        simpleTest
      return ()
    _ -> print "Usage: test beadAddress seleniumAddress"

simpleTest :: Selenium ()
simpleTest =
  do open "/"
     return ()
