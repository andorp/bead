{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.SitePages where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.PageObject
import Test.WebDriver.Tools

import Bead.Domain.Entities
import Bead.View.Snap.TemplateAndComponentNames
import Bead.Controller.Pages

import Data.String

data LoginPage = LoginPage {
    lUsername :: String
  , lPassword :: String
  }

instance PageObject LoginPage where
  precondition = const $
    doesFieldExist loginUsername <&&>
    doesFieldExist loginPassword <&&>
    doesFieldExist loginSubmitBtn
  failureMsg = const "Login page"
  action l = do
    (sendKeysStr (lUsername l)) <@> loginUsername
    (sendKeysStr (lPassword l)) <@> loginPassword
    click <@> loginSubmitBtn

data LogoutPage = LogoutPage

instance PageObject LogoutPage where
  precondition = const $ doesFieldExist Logout
  failureMsg   = const "Logout page"
  action = const $ click <@> Logout

data Registration = Registration {
    rUsername :: String
  , rPassword :: String
  , rEmail    :: String
  , rFullName :: String
  }

loginInfo :: Registration -> LoginPage
loginInfo r = LoginPage {
    lUsername = rUsername r
  , lPassword = rPassword r
  }

instance PageObject Registration where
  precondition = const $
    doesFieldExist loginUsername <&&>
    doesFieldExist loginPassword <&&>
    doesFieldExist registrationEmailAddress <&&>
    doesFieldExist registrationFamilyName
  failureMsg = const "Registration page"
  action d = do
    (sendKeysStr (rUsername d)) <@> loginUsername
    (sendKeysStr (rPassword d)) <@> loginPassword
    (sendKeysStr (rEmail    d)) <@> registrationEmailAddress
    (sendKeysStr (rFullName d)) <@> registrationFamilyName
    click <@> regSubmitBtn

