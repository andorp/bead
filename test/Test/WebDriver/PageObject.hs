{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.PageObject where

-- Haskell imports

import Control.Monad (liftM, unless)
import Control.Monad.Trans.Error (Error(..))

-- Test imports

--import Test.Selen.Server
--import Test.Selenium.Syntax (Locator(Id))
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
    isValid    :: TSelenium Bool
  , failureMsg :: String
  , test       :: TSelenium ()
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
  v <- isValid p
  unless v . failed . failureMsg $ p
  test p

loginPage :: PageObject
loginPage = PageObject {
    isValid = liftS $ do
      u <- findElems . ById . fieldName $ loginUsername
      p <- findElems . ById . fieldName $ loginPassword
      s <- findElems . ById . fieldName $ loginSubmitBtn
      return $ and $ map (not . null) [u,p,s]
  , failureMsg = "Login page"
  , test = liftS $ do
      (findElem . ById . fieldName $ loginUsername) >>= sendKeys "a"
      (findElem . ById . fieldName $ loginPassword) >>= sendKeys "a"
      (findElem . ById . fieldName $ loginSubmitBtn) >>= click
      return ()
  }

logoutPage :: PageObject
logoutPage = PageObject {
    isValid = liftS $ do
      e <- findElems . ById . fieldName $ Logout
      return . not . null $ e
  , failureMsg = "Logout page"
  , test = liftS $ do
      (findElem . ById . fieldName $ Logout) >>= click
  }
