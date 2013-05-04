{-# LANGUAGE OverloadedStrings  #-}
module Test.WebDriver.Support.Table (
    Table
  , table
  , headerCells
  , rows
  , cells
  , cell
  ) where

import Test.WebDriver
import Test.WebDriver.Tools (ifM, maybeM)
import Test.WebDriver.Classes

import Data.Maybe (maybe)
import Control.Monad (join)
import Control.Applicative ((<$>))

data Table = Table {
    rootElement :: Element
  }
  
table :: (WebDriver wd) => Element -> wd (Maybe Table)
table e = do
  t <- tagName e
  case t of
    "table" -> do
      return . Just $ Table {
        rootElement = e
      }
    _ -> return Nothing

rows :: (WebDriver wd) => Table -> wd [Element]
rows t = findElemsFrom (rootElement t) (ByTag "tr")

headerCells :: (WebDriver wd) => Table -> wd [Element]
headerCells t = findElemsFrom (rootElement t) (ByTag "th")

cells :: (WebDriver wd) => Table -> Int -> wd (Maybe [Element])
cells t r = do
  rs <- rows t
  maybeM (flip findElemsFrom (ByTag "td")) (rs <!!> r)

cell :: (WebDriver wd) => Table -> Int -> Int -> wd (Maybe Element)
cell t r c = (join . fmap (flip at c)) <$> cells t r

-- * Tools


infixl 2 <!!>

(<!!>) :: [a] -> Int -> Maybe a
(<!!>) = at

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (a:as) ~i
  | i < 0  = Nothing
  | i == 0 = Just a
  | otherwise = at as (i-1)
