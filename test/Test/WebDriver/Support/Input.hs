{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.Support.Input (
    TextInput, textInput, clearTextInput, enterText
  ) where

import Test.WebDriver
import Test.WebDriver.Classes

import Data.Text (Text)
import Data.Maybe
import Data.String
import Control.Applicative ((<$>))

import qualified Data.Text as T

data TextInput = TextInput {
    rootElement :: Element
  }

textInput :: (WebDriver wd) => Element -> wd (Maybe TextInput)
textInput e = do
  tag <- tagName e
  case tag of
    "input" -> do
      t <- attr e "type"
      case t of
        Just "text" -> return . Just $ TextInput { rootElement = e }
        _           -> return Nothing
    _ -> return Nothing

enterText :: (WebDriver wd) => Text -> TextInput -> wd ()
enterText t = sendKeys t . rootElement

clearTextInput :: (WebDriver wd) => TextInput -> wd ()
clearTextInput = clearInput . rootElement

