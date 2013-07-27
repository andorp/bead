{-# LANGUAGE CPP #-}
module Bead.View.Snap.Validators where

import Prelude

#ifndef FAY
import Data.List (find)

#ifdef TEST
import Bead.Invariants (Assertion(..))
#endif
#endif

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
    validator = (>=4) . length
  , message   = "Less than 4 characters"
  }

isEmailAddress :: FieldValidator
isEmailAddress = FieldValidator {
    validator = emailAddress
  , message   = "Invalid email address"
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

emailAddress :: String -> Bool
emailAddress []     = False
emailAddress (c:cs) = isAlpha c && isEmailBody cs
  where
    isSpecial :: Char -> Bool
    isSpecial c = elem c "._,!"

    isEmailChar :: Char -> Bool
    isEmailChar c = or [isAlpha c, isDigit c, isSpecial c]

    isEmailBody [] = False
    isEmailBody ('@':cs) = isEmailRest cs
    isEmailBody (c:cs)
      | isEmailChar c = isEmailBody cs
      | otherwise     = False

    isEmailRest []    = True
    isEmailRest ['.'] = False
    isEmailRest (c:cs)
      | isEmailChar c = isEmailRest cs
      | otherwise     = False

#ifdef TEST
assertEmailAddress = [
    Assertion "Empty"     (emailAddress "") False
  , Assertion "One char"  (emailAddress "q") False
  , Assertion "One char"  (emailAddress "1") False
  , Assertion "Only user" (emailAddress "q.dfs") False
  , Assertion "Valid"     (emailAddress "q.fd@gma.il.com") True
  , Assertion "Invalid"   (emailAddress "1adf@ga.com.") False
  ]
#endif
