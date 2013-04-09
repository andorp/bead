{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.SitePages where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.PageObject

import Bead.Controller.Pages
import Bead.View.Snap.TemplateAndComponentNames

loginPage :: PageObject
loginPage = PageObject {
    precondition = liftS $ do
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
    precondition = liftS $ do
      e <- findElems . ById . fieldName $ Logout
      return . not . null $ e
  , failureMsg = "Logout page"
  , test = liftS $ do
      (findElem . ById . fieldName $ Logout) >>= click
  }
