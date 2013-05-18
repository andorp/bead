{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.PageObject where

-- Haskell imports

import Data.Maybe
import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error  as CME
import Control.Monad.Trans.Reader as CMR

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

type TWD = ErrorT TestException WD

type TestFrame = TWD ()

type Test a = ReaderT [TestFrame] TWD a

data Result =
    Passed
  | Failed String
  deriving (Show, Eq, Ord)

class IsResult r where
  isFailure :: r -> Bool
  isPassed  :: r -> Bool

instance IsResult Result where
  isFailure (Failed _) = True
  isFailure _          = False

  isPassed Passed = True
  isPassed _      = False

newtype TestCase a = TestCase (String, a)
  deriving (Show, Eq, Ord)

mkTestCase :: String -> a -> TestCase a
mkTestCase msg tc = TestCase (msg, tc)

tcValue :: TestCase a -> a
tcValue (TestCase (name, a)) = a

instance (IsResult r) => IsResult (TestCase r) where
  isFailure = isFailure . tcValue
  isPassed  = isPassed  . tcValue

class PageObject p where
  precondition :: p -> Test Bool
  failureMsg   :: p -> String

class (PageObject p) => PageAction p where
  action :: p -> Test ()

onPage :: (PageObject p) => p -> Test a -> Test a
onPage p a = do
  checkIfPageIs p
  a

failed :: String -> Test a
failed = lift . throwError . TestFailure . Just

failsOn :: (a -> Bool) -> (a -> b) -> String -> Test a -> Test b
failsOn f g msg m = do
  x <- m
  case f x of
    True  -> failed msg
    False -> return . g $ x

failsOnNothing :: String -> Test (Maybe a) -> Test a
failsOnNothing = failsOn isNothing fromJust

failsOnFalse :: String -> Test Bool -> Test Bool
failsOnFalse = failsOn (==False) id

failsOnTrue :: String -> Test Bool -> Test Bool
failsOnTrue = failsOn (==True) id

cleanUp :: Test a -> Test b -> Test a
cleanUp t c =
  liftCatch
    catchError
    t
    (\e -> do
       c
       lift $ throwError e)

expectToFail :: String -> Test () -> Test ()
expectToFail msg t = ReaderT (expectToFail' msg . runReaderT t)
  where
   expectToFail' :: String -> TWD () -> TWD ()
   expectToFail' msg e = ErrorT $ do
     x <- runErrorT e
     return $ case x of
       Left _  -> Right ()
       Right _ -> Left . strMsg $ msg

runTest :: Test a -> WD Result
runTest t = do
  x <- runErrorT (runReaderT t [])
  case x of
    Left e  -> return . Failed . show $ e
    Right _ -> return Passed

setTestFrames :: [TestFrame] -> Test a -> Test a
setTestFrames fs = local (fs ++)

checkFrames :: Test ()
checkFrames = do
  fs <- ask
  mapM_ lift fs

page :: (PageAction p) => p -> Test ()
page p = do
  checkFrames
  checkIfPageIs p
  checkFrames
  action        p
  checkFrames

checkIfPageIs :: (PageObject p) => p -> Test ()
checkIfPageIs p = do
  x <- precondition              $ p
  unless x . failed . failureMsg $ p

