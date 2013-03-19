module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Network.URI
import Test.WebDriver
import Test.WebDriver.Commands

import Test.WebDriver.PageObject

-- Usage: main http://127.0.0.1:8000 127.0.0.1
main = do
  args <- getArgs
  case args of
    [beadAddress, seleniumAddress] -> do
      let beadUri = fromJust . parseURI $ beadAddress
      runSession
        defaultSession
        defaultCaps { browser = firefox { ffBinary = Just "/usr/local/bin/firefox" } } $ do
          setImplicitWait 30000
          simpleTest

      return ()
    _ -> print "Usage: test beadAddress seleniumAddress"

simpleTest :: WD ()
simpleTest = do
  runT $ do
    liftS $ openPage "http://127.0.0.1:8000/"
    pageObject loginPage
    pageObject logoutPage
  return ()
