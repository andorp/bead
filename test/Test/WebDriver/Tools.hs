module Test.WebDriver.Tools where

-- Webdriver imports

import Test.WebDriver
import Test.WebDriver.Classes
import Test.WebDriver.Commands

-- Haskell imports

import Data.String
import Control.Applicative ((<$>))

-- Bead imports

import Bead.View.Snap.TemplateAndComponentNames

-- * Tools

doesFieldExist :: (WebDriver wd, SnapFieldName f) => f -> wd Bool
doesFieldExist f = (not . null) <$> (findElems . ById . fieldName $ f)

doesElementExist :: (WebDriver wd, SnapFieldName f) => f -> wd Bool
doesElementExist = doesFieldExist

findField :: (WebDriver wd, SnapFieldName f) => f -> wd Element
findField = findElem . ById . fieldName

onField :: (WebDriver wd, SnapFieldName f) => f -> (Element -> wd a) -> wd a
onField f w = (findField f) >>= w

infixr 1 <@>

(<@>) :: (WebDriver wd, SnapFieldName f) => (Element -> wd a) -> f -> wd a
w <@> f = onField f w

infixr 1 <&&>

-- Short circuit and operator
(<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
a <&&> b = do
  x <- a
  case x of
    False -> return False
    True  -> b

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM p t e = do
  x <- p
  case x of
    True  -> t
    False -> e

allM :: (Functor m, Monad m) => [m Bool] -> m Bool
allM = (fmap and) . sequence

maybeM :: (Functor m, Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM _ Nothing  = return Nothing
maybeM k (Just x) = Just <$> (k x)

sendKeysStr :: (WebDriver wd) => String -> Element -> wd ()
sendKeysStr s = sendKeys (fromString s)

