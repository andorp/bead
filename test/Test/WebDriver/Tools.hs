module Test.WebDriver.Tools where

-- Webdriver imports

import Test.WebDriver
import Test.WebDriver.Commands

-- Haskell imports

import Data.String
import Control.Applicative ((<$>))

-- Bead imports

import Bead.View.Snap.TemplateAndComponentNames

-- * Tools

doesFieldExist :: (SnapFieldName f) => f -> WD Bool
doesFieldExist f = (not . null) <$> (findElems . ById . fieldName $ f)

findField :: (SnapFieldName f) => f -> WD Element
findField = findElem . ById . fieldName

onField :: (SnapFieldName f) => f -> (Element -> WD a) -> WD a
onField f w = (findField f) >>= w

infixr 1 <@>

(<@>) :: (SnapFieldName f) => (Element -> WD a) -> f -> WD a
w <@> f = onField f w

infixr 1 <&&>

-- Short circuit and operator
(<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
a <&&> b = do
  x <- a
  case x of
    False -> return False
    True  -> b

allM :: (Functor m, Monad m) => [m Bool] -> m Bool
allM = (fmap and) . sequence

sendKeysStr :: String -> Element -> WD ()
sendKeysStr s = sendKeys (fromString s)

