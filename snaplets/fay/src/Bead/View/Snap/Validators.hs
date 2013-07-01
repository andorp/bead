module Bead.View.Snap.Validators where

import Prelude

{- This module is compiled with Fay and Haskell -}

-- | Validator for an input field
data FieldValidator = FieldValidator {
    validator :: String -> Bool -- Produces True if the String is valid
  , message   :: String         -- The message to shown when the validation fails
  }

validate :: FieldValidator -> String -> a -> (String -> a) -> a
validate f v onValid onFail
  | validator f v = onValid
  | otherwise     = onFail (message f)

isUsername :: FieldValidator
isUsername = FieldValidator {
    validator = not . null
  , message   = "Empty username"
  }

isPassword :: FieldValidator
isPassword = FieldValidator {
    validator = (>4) . length
  , message   = "Less than 4 characters"
  }

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

