module Bead.View.Snap.Validators where

import Prelude

{- This module is compiled with Fay and Haskell -}

isUsername :: String -> Bool
isUsername = not . null

isPassword :: String -> Bool
isPassword = (>4) . length

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _   = False

