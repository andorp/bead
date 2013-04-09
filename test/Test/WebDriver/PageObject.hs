{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.PageObject where

-- Haskell imports

import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error (Error(..))

-- Test imports

import Test.WebDriver
import Test.WebDriver.Commands
import Control.Monad.Transaction

-- Bead imports

import Bead.Controller.Pages
import Bead.View.Snap.TemplateAndComponentNames

-- Definitions

data TestException
  = TestException (Maybe String)
  | TestFailure   (Maybe String)

instance Error TestException where
  noMsg  = TestException Nothing
  strMsg = TestException . Just

testResultMsg :: TestException -> String
testResultMsg (TestException  Nothing) = "Exception: No error message"
testResultMsg (TestFailure    Nothing) = "Failure: No error message"
testResultMsg (TestException (Just m)) = m
testResultMsg (TestFailure   (Just m)) = m

type TSelenium a = Transaction TestException WD a

data Result =
    Passed
  | Failed String
  deriving (Show,Eq)

data PageObject = PageObject {
    precondition :: TSelenium Bool
  , failureMsg   :: String
  , test         :: TSelenium ()
  }

failed :: String -> TSelenium a
failed msg = stepEither (return . Left . TestFailure . Just $ msg) (return ())

liftS :: WD a -> TSelenium a
liftS s = stepEither (liftM Right s) (return ())

stepR :: WD a -> WD () -> TSelenium a
stepR s r = stepEither (liftM Right s) r

runT :: TSelenium a -> WD Result
runT t = do
  r <- atomically t
  case r of
    Left  e -> return . Failed . testResultMsg $ e
    Right _ -> return Passed

pageObject :: PageObject -> TSelenium ()
pageObject p = do
  v <- precondition p
  unless v . failed . failureMsg $ p
  test p
