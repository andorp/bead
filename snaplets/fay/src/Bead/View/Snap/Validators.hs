{-# LANGUAGE CPP #-}
module Bead.View.Snap.Validators where

import Prelude

#ifndef FAY
import Data.List (find)
import Bead.Invariants (Assertion(..))
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
    validator = all isAlphaNum
  , message   = "NEPTUN-kódot kell megadni"
  }

isPassword :: FieldValidator
isPassword = FieldValidator {
    validator = (>=4) . length
  , message   = "4 karakternél hosszabbnak kell lennie!"
  }

isEmailAddress :: FieldValidator
isEmailAddress = FieldValidator {
    validator = emailAddress
  , message   = "Nem fogadható el email címnek!"
  }

isDateTime :: FieldValidator
isDateTime = FieldValidator {
    validator = dateTime
  , message   = "Hibás dátum vagy idő!"
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

isAlphaNum :: Char -> Bool
isAlphaNum c =
  if (isAlpha c)
     then True
     else (isDigit c)

emailAddress :: String -> Bool
emailAddress []     = False
emailAddress (c:cs) = isEmailChar c && isEmailBody cs
  where
    isSpecial :: Char -> Bool
    isSpecial c = elem c "._,!-():;<>[\\]"

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

dateTime :: String -> Bool
dateTime s = case s of
  [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2,' ',hr1,hr2,':',mn1,mn2,':',sc1,sc2] ->
    all isDigit [y1,y2,y3,y4,m1,m2,d1,d2,hr1,hr2,mn1,mn2,sc1,sc2]
  _ -> False

#ifndef FAY
assertEmailAddress = [
    Assertion "Empty"     (emailAddress "") False
  , Assertion "One char"  (emailAddress "q") False
  , Assertion "One char"  (emailAddress "1") False
  , Assertion "Only user" (emailAddress "q.dfs") False
  , Assertion "Valid"     (emailAddress "q.fd@gma.il.com") True
  , Assertion "Valid 2"   (emailAddress "1adf@ga.com") True
  , Assertion "Invalid"   (emailAddress "1adf@ga.com.") False
  ]
#endif
