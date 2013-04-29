{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.Support.Select (
    Select
  , rootElement
  , isMultiple

  , select
  , options
  , allSelectedOptions
  , firstSelectedOption
  , selectByVisibleText
  , selectByIndex
  , selectByValue
  , deselectAll
  , deselectByValue
  , deselectByIndex
  ) where

import Test.WebDriver
import Test.WebDriver.Tools (ifM)
import Test.WebDriver.Classes

import Data.Text (Text)
import Data.Maybe
import Data.String
import Control.Applicative ((<$>))
import Control.Monad

import qualified Data.Text as T


data Select = Select {
    rootElement :: Element
  , isMultiple  :: Bool
  }

select :: (WebDriver wd) => Element -> wd (Maybe Select)
select e = do
  t <- tagName e
  case t of
    "select" -> do
      m <- attr e "multiple"
      return . Just $ Select {
        rootElement = e
      , isMultiple  = maybe False (/="false") m
      }
    _ -> return Nothing

options :: (WebDriver wd) => Select -> wd [Element]
options s = findElemsFrom (rootElement s) (ByTag "option")

allSelectedOptions :: (WebDriver wd) => Select -> wd [Element]
allSelectedOptions = filterM isSelected <=< options

firstSelectedOption :: (WebDriver wd) => Select -> wd (Maybe Element)
firstSelectedOption = liftM listToMaybe . allSelectedOptions

-- Selection helpers

hasTheIndex i e = maybe False (i==) <$> attr e "index"

valueXPath t = ByXPath (T.concat [".//option[@value = ", escapeQoutes t, "]"])

isSubString t e = (T.isInfixOf t) <$> (getText e)

clickOnSelecteds :: (WebDriver wd) => [Element] -> wd ()
clickOnSelecteds = filterM isSelected >=> mapM_ click

-- Selection

selectBy :: (WebDriver wd) => Select -> (Select -> wd [Element]) -> wd ()
selectBy s enumerate = do
  deselectAll s
  (mapM_ setSelected) =<< (fmap cut (enumerate s))
  where
    cut es = case isMultiple s of
               True -> es
               False -> take 1 es

selectByVisibleText :: (WebDriver wd) => Select -> Text -> wd ()
selectByVisibleText s t = selectBy s (options >=> filterM (isSubString t))

selectByIndex :: (WebDriver wd) => Select -> Int -> wd ()
selectByIndex s i = selectBy s (options >=> filterM (hasTheIndex index))
  where index = fromString . show $ i

selectByValue :: (WebDriver wd) => Select -> Text -> wd ()
selectByValue s t = selectBy s (\s -> findElemsFrom (rootElement s) (valueXPath t))

-- Deselection

deselectAll :: (WebDriver wd) => Select -> wd ()
deselectAll = options >=> clickOnSelecteds

deselectByValue :: (WebDriver wd) => Select -> Text -> wd ()
deselectByValue s t = findElemsFrom (rootElement s) (valueXPath t) >>= clickOnSelecteds

deselectByIndex :: (WebDriver wd) => Select -> Int -> wd ()
deselectByIndex s i = options s >>= (filterM (hasTheIndex index) >=> clickOnSelecteds)
  where index = fromString . show $ i

setSelected :: (WebDriver wd) => Element -> wd ()
setSelected e =
  ifM (isSelected e)
      (return ())
      (click e)

escapeQoutes :: Text -> Text
escapeQoutes t =
  let q1 = isJust $ T.find (=='"') t
      q2 = isJust $ T.find (=='\'') t
  in case (q1,q2) of
       (False, False) -> T.concat ["'", t, "'"]
       (True, False)  -> T.concat ["'", t, "'"]
       (False, True)  -> T.concat ["\"", t, "\""]
       (True, True)   -> id t -- TODO

{-
REFERENCE implementation:
https://code.google.com/p/selenium/source/browse/java/client/src/org/openqa/selenium/support/ui/Select.java
-}
