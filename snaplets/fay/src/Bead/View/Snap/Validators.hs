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
isDigit c = elem c "0123456789"

toLower :: Char -> Char
toLower c =
  let v = find ((c==).fst) $
            zip "QWERTZUIOPASDFGHJKLYXCVBNM"
                "qwertzuiopasdfghjklyxcvbnm"
  in case v of
       Nothing -> c
       Just c' -> snd c'

isAlpha :: Char -> Bool
isAlpha c = elem (toLower c) "qwertzuiopasdfghjklyxcvbnm"
