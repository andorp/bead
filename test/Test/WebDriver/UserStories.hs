{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.UserStories where

import Test.WebDriver
import Test.WebDriver.Commands

import Test.WebDriver.Tools
import Test.WebDriver.PageObject
import Test.WebDriver.SitePages

registration :: String -> Registration -> TWD ()
registration url reg = do
  liftS $ openPage url
  liftS $ click =<< findElem (ByLinkText "Create new user")
  page reg
  page . loginInfo $ reg
  page LogoutPage

