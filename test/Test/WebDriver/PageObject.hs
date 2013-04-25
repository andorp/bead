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

type TWD a = Transaction TestException WD a

data Result =
    Passed
  | Failed String
  deriving (Show,Eq)

class PageObject p where
  precondition :: p -> WD Bool
  failureMsg   :: p -> String
  action       :: p -> WD ()

failed :: String -> TWD a
failed msg = stepEither (return . Left . TestFailure . Just $ msg) (return ())

liftS :: WD a -> TWD a
liftS s = stepEither (liftM Right s) (return ())

stepR :: WD a -> WD () -> TWD a
stepR s r = stepEither (liftM Right s) r

runT :: TWD a -> WD Result
runT t = do
  r <- atomically t
  case r of
    Left  e -> return . Failed . testResultMsg $ e
    Right _ -> return Passed

page :: (PageObject p) => p -> TWD ()
page p = do
  v <- liftS . precondition $ p
  unless v . failed . failureMsg $ p
  liftS . action $ p
