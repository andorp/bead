module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Network.URI
import Test.WebDriver
import Test.WebDriver.Commands

import Test.WebDriver.PageObject
import Test.WebDriver.SitePages
import Test.WebDriver.UserStories
import Test.WebDriver.Positives

-- import Bead.Domain.Entities (Role(..))

-- Usage: main http://127.0.0.1:8000 127.0.0.1
main = do
  args <- getArgs
  case args of
    [beadAddress, seleniumAddress] -> do
      let beadUri = fromJust . parseURI $ beadAddress
      result <- runSession
        defaultSession
        defaultCaps { browser = firefox { ffBinary = Just "/usr/local/bin/firefox" } } $ do
          setImplicitWait 30000
          simpleTest beadAddress

      print result
      return ()
    _ -> print "Usage: test beadAddress seleniumAddress"

simpleTest :: String -> WD [Result]
simpleTest url = mapM runT (positives url)

