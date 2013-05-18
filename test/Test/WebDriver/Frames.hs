{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.Frames where

import Data.Text
import Control.Monad (when)
import Control.Monad.Trans.Error (throwError, strMsg)
import Control.Monad.IO.Class

import Test.WebDriver
import Test.WebDriver.PageObject

webhandlerException :: TWD ()
webhandlerException = do
  pres <- findElems (ByTag "pre")
  flip mapM_ pres $ \pre -> do
    t <- getText pre
    when (isInfixOf "exception" t) $
      throwError . strMsg $ "Webhandler exception"
