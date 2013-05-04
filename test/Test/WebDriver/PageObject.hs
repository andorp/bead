{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.PageObject where

-- Haskell imports

import Data.Maybe
import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error

-- Test imports

import Test.WebDriver
import Test.WebDriver.Classes
import Test.WebDriver.Commands
import Control.Monad.Transaction

-- Bead imports

import Bead.Controller.Pages
import Bead.View.Snap.TemplateAndComponentNames

import Control.Monad.TestContext.Trans

-- Definitions

data TestException
  = TestException (Maybe String)
  | TestFailure   (Maybe String)
  deriving Show

instance Error TestException where
  noMsg  = TestException Nothing
  strMsg = TestException . Just

testResultMsg :: TestException -> String
testResultMsg (TestException  Nothing) = "Exception: No error message"
testResultMsg (TestFailure    Nothing) = "Failure: No error message"
testResultMsg (TestException (Just m)) = m
testResultMsg (TestFailure   (Just m)) = m

type TWD a = ErrorT TestException WD a

data Result =
    Passed
  | Failed String
  deriving (Show,Eq)

class PageObject p where
  precondition :: p -> TWD Bool
  failureMsg   :: p -> String

class (PageObject p) => PageAction p where
  action :: p -> TWD ()

onPage :: (PageObject p) => p -> TWD a -> TWD a
onPage p a = do
  checkIfPageIs p
  a

failed :: String -> TWD a
failed = throwError . TestFailure . Just

failsOn :: (a -> Bool) -> (a -> b) -> String -> TWD a -> TWD b
failsOn f g msg m = do
  x <- m
  case f x of
    True  -> failed msg
    False -> return . g $ x

failsOnNothing :: String -> TWD (Maybe a) -> TWD a
failsOnNothing = failsOn isNothing fromJust

failsOnFalse :: String -> TWD Bool -> TWD Bool
failsOnFalse = failsOn (==False) id

failsOnTrue :: String -> TWD Bool -> TWD Bool
failsOnTrue = failsOn (==True) id

cleanUp :: TWD a -> TWD b -> TWD a
cleanUp t c =
  catchError
    t
    (\e -> do
       c
       throwError e)

expectToFail :: String -> TWD () -> TWD ()
expectToFail msg e = ErrorT $ do
  x <- runErrorT e
  return $ case x of
    Left _  -> Right ()
    Right _ -> Left . strMsg $ msg

runT :: TWD a -> WD Result
runT k = do
  x <- runErrorT k
  case x of
    Left  e -> return . Failed . show $ e
    Right _ -> return Passed

page :: (PageAction p) => p -> TWD ()
page p = do
  checkIfPageIs p
  action        p

checkIfPageIs :: (PageObject p) => p -> TWD ()
checkIfPageIs p = do
  x <- precondition              $ p
  unless x . failed . failureMsg $ p

