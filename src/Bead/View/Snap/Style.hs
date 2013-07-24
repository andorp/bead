{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Style where

import Control.Monad (join)
import Data.List (intersperse)
import Data.Monoid
import Data.String (fromString)

import Text.Blaze.Html5 (Html, Attribute, (!))
import Text.Blaze.Internal (Attributable)
import qualified Text.Blaze.Html5.Attributes as A

-- Represents CSS style elements
newtype StyleElement = StyleElement [String]
{- E.g:
autoMarginLeft = StyleElement ["margin-left: auto"]
-}

noStyle = StyleElement []

instance Monoid StyleElement where
  mempty = StyleElement []
  mappend (StyleElement a) (StyleElement b) = StyleElement (a ++ b)
  mconcat = StyleElement . join . map (styleElementFold id)

create :: String -> StyleElement
create m = StyleElement [m]

styleElementFold :: ([String] -> a) -> StyleElement -> a
styleElementFold f (StyleElement x) = f x

-- Peroduces HTML style attribute from the given style element
createStyle :: StyleElement -> Attribute
createStyle = styleElementFold (A.style . fromString . join . intersperse "; ")

marginLeftAuto  = create "margin-left: auto"
marginRightAuto = create "margin-right: auto"

borderColor :: String -> StyleElement
borderColor c = create . join $ ["border-color: ", c]

borderWidth :: Int -> StyleElement
borderWidth w = create . join $ ["border-width: ", show w, "px"]

borderStyle :: String -> StyleElement
borderStyle s = create . join $ ["border-style: ", s]

backgroundColor :: String -> StyleElement
backgroundColor c = create . join $ ["background-color: ", c]

-- Produces an HTML element attaching the style element onto it
-- infix notation is handy here
(#) :: Attributable h => h -> StyleElement -> h
html # style = html ! (createStyle style)

-- * Concrete styles

centerTable = mconcat [
    marginLeftAuto
  , marginRightAuto
  ]

informationalTable = mconcat [
    borderColor "gray"
  , borderWidth 1
  , borderStyle "inset"
  , marginLeftAuto
  , marginRightAuto
  ]

informationalCell = mconcat [
    borderColor "black"
  , borderWidth 1
  , borderStyle "inset"
  ]

textAlignLeft  = create $ "text-align: left"
textAlignRight = create $ "text-align: right"
grayBackground = backgroundColor $ "lightgray"
