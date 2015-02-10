{-# LANGUAGE CPP #-}
module Bead.View.DataBridge where

import Control.Monad ((>=>), join)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Time (LocalTime(..))
import Text.Printf (printf)

import Bead.Domain.Types (readMaybe)
import Bead.Domain.Entities hiding (name)
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation
import Bead.View.TemplateAndComponentNames
import Bead.View.Fay.Hooks
import Bead.View.Fay.HookIds
import Bead.View.Fay.JSON.ServerSide
import Bead.View.Validators
import Bead.View.RouteOf
import Bead.View.RequestParams

#ifdef TEST
import Control.Applicative ((<$>))

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Themis.Test
import Test.Themis.Test.Asserts
#endif

{-
Parameters are the data bridge between the Server side and the Client side.
Values are encoded and decoded in the request parameters
-}

-- | Represents an encoding/decoding method for the request parameters
data Parameter a = Parameter {
    -- Produces a String representation of the given value
    encode :: a -> String
    -- Produces 'Just a' if the given string is represents a value
    -- otherwise 'Nothing'
  , decode :: String -> Maybe a
    -- The field name in the request
  , name  :: String
    -- The error message when decoding fails
  , decodeError :: String -> String
    -- The error message when the parameter is not present
  , notFound    :: String
  }

parameterFold
  :: ((a -> String) -> (String -> Maybe a) -> String -> (String -> String) -> String -> b)
  -> Parameter a
  -> b
parameterFold f (Parameter encode decode name decodeError notFound) =
  f encode decode name decodeError notFound

-- Creates a request parameter value encoding the given value with the
-- given parameter
requestParameter :: Parameter a -> a -> ReqParam
requestParameter p x = parameterFold createValue p
  where
    createValue encode _ name _ _ = ReqParam (name, encode x)

stringParameter :: String -> String -> Parameter String
stringParameter fieldName paramName = Parameter {
    encode = id
  , decode = Just . id
  , name = fieldName
  , decodeError = \m -> printf "%s nem dekódolható: %s!" paramName m
  , notFound    = printf "%s nem található!" paramName
  }

evaluationValuePrm :: Parameter String
evaluationValuePrm = stringParameter (fieldName evaluationValueField) "Evaluation Value"

customGroupKeyPrm :: String -> Parameter GroupKey
customGroupKeyPrm field = Parameter {
    encode = groupKeyMap id
  , decode = Just . GroupKey
  , name   = field
  , decodeError = \m -> printf "Érvénytelen csoportazonosító: %s!" m
  , notFound    = "A csoportazonosító nem található!"
  }

-- Represents the GroupKey parameter
groupKeyPrm :: Parameter GroupKey
groupKeyPrm = customGroupKeyPrm (fieldName groupKeyName)

jsonGroupKeyPrm :: String -> Parameter GroupKey
jsonGroupKeyPrm field = jsonParameter field "csoportazonosító"

customCourseKeyPrm :: String -> Parameter CourseKey
customCourseKeyPrm field = Parameter {
    encode = courseKeyMap id
  , decode = Just . CourseKey
  , name   = field
  , decodeError = \m -> printf "Érvénytelen tárgyazonosító: %s!" m
  , notFound    = "A tárgyazonosító nem található!"
  }

jsonCourseKeyPrm :: String -> Parameter CourseKey
jsonCourseKeyPrm field = jsonParameter field "tárgyazonosító"

-- Represents the CourseKey parameter
courseKeyPrm :: Parameter CourseKey
courseKeyPrm = customCourseKeyPrm (fieldName courseKeyInfo)

-- Represents the AssignmentKey parameter
assignmentKeyPrm :: Parameter AssignmentKey
assignmentKeyPrm = Parameter {
    encode = assignmentKeyMap id
  , decode = Just . AssignmentKey
  , name   = fieldName assignmentKeyField
  , decodeError = \m -> printf "Érvénytelen feladatazonosító: %s!" m
  , notFound    = "A feladatazonosító nem található!"
  }

-- Represents the SubmissionKey parameter
submissionKeyPrm :: Parameter SubmissionKey
submissionKeyPrm = Parameter {
    encode = submissionKeyMap id
  , decode = Just . SubmissionKey
  , name   = fieldName submissionKeyField
  , decodeError = \m -> printf "Érvénytelen megoldásazonosító: %s!" m
  , notFound    = "A megoldásazonosító nem található!"
  }

-- Represents the TestScriptKey parameter
testScriptKeyPrm :: Parameter TestScriptKey
testScriptKeyPrm = Parameter {
    encode = testScriptKeyCata id
  , decode = Just . TestScriptKey
  , name = fieldName testScriptKeyField
  , decodeError = \m -> printf "Érvénytelen teszt szkript azonosító: %s!" m
  , notFound = "A teszt szkript azonosító nem található"
  }

-- Represents the SubmissionKey parameter
evaluationKeyPrm :: Parameter EvaluationKey
evaluationKeyPrm = Parameter {
    encode = evaluationKeyMap id
  , decode = Just . EvaluationKey
  , name   = fieldName evaluationKeyField
  , decodeError = \m -> printf "Érvénytelen értékelésazonosító: %s!" m
  , notFound    = "Az értékelésazonosító nem található!"
  }

-- JSON encodeable parameter, used in POST request parameters only
jsonParameter :: (Data a, Show a) => String -> String -> Parameter a
jsonParameter field name = Parameter {
    encode = \v -> fromMaybe (error $ concat [name, " jsonParameter: encodeToFay ",show v]) $ encodeToFay v
  , decode = decodeFromFay
  , name = field
  , decodeError = \v -> printf "Hibás json érték: (%s, %s)!" name v
  , notFound    = printf "%s nem található!" name
  }

evalConfigPrm :: EvaluationHook -> Parameter EvConfig
evalConfigPrm hook = Parameter {
    encode = fromMaybe "evalConfigPrm: encodeToFay has failed" . encodeToFay
  , decode = readEvalConfig
  , name   = evHiddenValueId hook
  , decodeError = \m -> printf "Hibás értékelési fajta: %s!" m
  , notFound    = "Nem található az értékelés fajtája!"
  }
  where
    readEvalConfig :: String -> Maybe EvConfig
    readEvalConfig [] = error "evalConfigPrm: Empty string"
    readEvalConfig xs = fmap convert $ decodeFromFay xs
      where
        convert :: EvConfig -> EvConfig
        convert = evaluationDataMap (const binaryConfig) (percentageConfig . guardPercentage) . evConfig

        guardPercentage :: Double -> Double
        guardPercentage x
           | 0 > x || x > 1 = error $ "Invalid percentage value:" ++ show x
           | otherwise = x


#ifdef TEST

evalConfigPrmTest = group "evalConfigPrm" $ do
  let evConfigGen = oneof
        [ return binaryConfig
        , (percentageConfig . toPercentage) <$> arbitrary
        ]
      param = evalConfigPrm undefined

  assertProperty
    "Encode decode"
    (encodeDecodeCompare pctEpsilon param)
    evConfigGen
    "Percentage value is miscalculated"

  eqPartitions (decode param)
    [ ("Binary", encode param binaryConfig, Just binaryConfig, "Parsing failed")
    , ("Percenteage", encode param (percentageConfig 0.96)
      , Just (percentageConfig 0.96), "Parsing failed")
    ]
  where
    -- The percentage calculation is interested only in 3 decimal digits
    toPercentage :: Double -> Double
    toPercentage = (/ scale) . fromIntegral . floor . (scale *) . getDecimal
      where
        scale = 10 ^ 3
        getDecimal x = x - (fromIntegral $ floor x)

    pctEpsilon :: EvConfig -> EvConfig -> Bool
    pctEpsilon c1 c2 =
      withEvConfig c1
        (withEvConfig c2 True (const False))
        (\p1 -> withEvConfig c2 False (\p2 -> abs (p1 - p2) < epsilon))
      where
        epsilon = 0.0001

#endif

rolePrm :: Parameter Role
rolePrm = jsonParameter (fieldName userRoleField) "Szerepkör"

uidPrm :: Parameter Uid
uidPrm = jsonParameter (fieldName userUidField) "User ID"

jsonUsernamePrm :: String -> Parameter Username
jsonUsernamePrm field =
  let prm = jsonParameter field "Felhasználónév"
  in prm { decode = decode prm >=> decodeUsr }
  where
    decodeUsr = usernameCata $ \xs ->
      if (validator isUsername xs)
         then (Just $ transformUsername xs)
         else Nothing

customUsernamePrm :: String -> Parameter Username
customUsernamePrm field = Parameter {
    encode = usernameCata id
  , decode = decodeUsr
  , name = field
  , decodeError = \m -> printf "%s: %s" (message isUsername) m
  , notFound    = "A felhasználónév nem található!"
  } where
    decodeUsr xs =
      if (validator isUsername xs)
         then (Just $ transformUsername xs)
         else Nothing

transformUsername :: String -> Username
transformUsername = Username . map toUpper

usernamePrm :: Parameter Username
usernamePrm = customUsernamePrm (fieldName usernameField)

loginUsernamePrm :: Parameter Username
loginUsernamePrm = customUsernamePrm (fieldName loginUsername)

emailPrm :: String -> Parameter Email
emailPrm field = validateBy isEmailAddress $ Parameter {
    encode = emailFold id
  , decode = parseEmail
  , name = field
  , decodeError = \m -> printf "Hibás email cím: %s!" m
  , notFound    = "Az email cím nem található!"
  }

userEmailPrm :: Parameter Email
userEmailPrm = emailPrm (fieldName userEmailField)

regEmailPrm :: Parameter Email
regEmailPrm = emailPrm (fieldName regEmailAddress)

passwordPrm :: String -> String -> Parameter String
passwordPrm fieldName paramName = Parameter {
    encode = id
  , decode = Just . id
  , name = fieldName
  , decodeError = const "Invalid password"
  , notFound    = join [paramName, " is not found."]
  }

regPasswordPrm :: Parameter String
regPasswordPrm = validateBy isPassword $ passwordPrm (fieldName loginPassword) "Jelszó"

regPasswordAgainPrm :: Parameter String
regPasswordAgainPrm = validateBy isPassword $ passwordPrm (fieldName regPasswordAgain) "Jelszó (ismét)"

loginPasswordPrm :: Parameter String
loginPasswordPrm = validateBy isPassword $ passwordPrm (fieldName loginPassword) "Bejelentkezési jelszó"

oldPasswordPrm :: Parameter String
oldPasswordPrm = validateBy isPassword $ passwordPrm (fieldName oldPasswordField) "Régi jelszó"

newPasswordPrm :: Parameter String
newPasswordPrm = validateBy isPassword $ passwordPrm (fieldName newPasswordField) "Új jelszó"

newPasswordAgainPrm :: Parameter String
newPasswordAgainPrm = validateBy isPassword $ passwordPrm (fieldName newPasswordAgainField) "Új jelszó (ismét)"

studentNewPwdPrm :: Parameter String
studentNewPwdPrm = validateBy isPassword $ passwordPrm (fieldName studentNewPwdField) "Jelszó"

studentNewPwdAgainPrm :: Parameter String
studentNewPwdAgainPrm = validateBy isPassword $ passwordPrm (fieldName studentNewPwdAgainField) "Jelszó (ismét)"

regUsernamePrm :: Parameter Username
regUsernamePrm = validateBy isUsername $ customUsernamePrm (fieldName loginUsername)

regFullNamePrm :: Parameter String
regFullNamePrm = stringParameter (fieldName regFullName) "Teljes név"

regUserRegKeyPrm :: Parameter UserRegKey
regUserRegKeyPrm = Parameter {
    encode = userRegKeyFold id
  , decode = Just . UserRegKey
  , name = (fieldName regUserRegKey)
  , decodeError = \m -> printf "A felhasználó regisztrációs kulcsának értéke hibás: %s!" m
  , notFound    = "Nincs ilyen regisztrációs kulcs a felhasználóhoz!"
  }

regTokenPrm :: Parameter String
regTokenPrm = stringParameter (fieldName regToken) "Regisztrációs token"

regLanguagePrm :: Parameter String
regLanguagePrm = stringParameter (fieldName regLanguage) "Regisztrációs nyelv"

-- Creates a new parameter where the the decode function first
-- validates a parameter with the given validator
validateBy :: FieldValidator -> Parameter a -> Parameter a
validateBy v p = p { decode = attachValidator (decode p) }
  where
    attachValidator :: (String -> Maybe a) -> (String -> Maybe a)
    attachValidator f s
      | validator v s = f s
      | otherwise     = Nothing

-- Produces a Parameter for a read instance for the
-- given parameter and a name. The name is shown
-- when decoding error or absence occurs.
readablePrm :: (Show a, Read a) => String -> String -> Parameter a
readablePrm field name = Parameter {
    encode = show
  , decode = readMaybe
  , name = field
  , decodeError = \v -> printf "Hibás read érték: (%s, %s)!" name v
  , notFound    = printf "%s nem található!" name
  }

localTimeParam :: String -> String -> Parameter LocalTime
localTimeParam field name = Parameter {
    encode = show
  , decode = readMaybe
  , name = field
  , decodeError = \v -> printf "Hibás idő érték: (%s, %s)!" name v
  , notFound    = printf "%s nem található!" name
  }

assignmentStartPrm :: Parameter LocalTime
assignmentStartPrm = localTimeParam (fieldName assignmentStartField) "Beküldés kezdete"

assignmentEndPrm :: Parameter LocalTime
assignmentEndPrm = localTimeParam (fieldName assignmentEndField) "Beküldés vége"

userTimeZonePrm :: Parameter TimeZoneName
userTimeZonePrm = jsonParameter (fieldName userTimeZoneField) "Időzóna"

regTimeZonePrm :: Parameter TimeZoneName
regTimeZonePrm = jsonParameter (fieldName regTimeZoneField) "Időzóna"

languagePrm :: String -> Parameter Language
languagePrm field = Parameter {
    encode = languageCata id
  , decode = Just . Language
  , name = field
  , decodeError = const "Language is not decodeable"
  , notFound = "Language parameter is not found"
  }

changeLanguagePrm :: Parameter Language
changeLanguagePrm = languagePrm (fieldName changeLanguageField)

userLanguagePrm :: Parameter Language
userLanguagePrm = jsonParameter (fieldName userLanguageField) "Language"

delUserFromCoursePrm :: Parameter Username
delUserFromCoursePrm = customUsernamePrm (fieldName delUserFromCourseField)

delUserFromCourseKeyPrm :: Parameter CourseKey
delUserFromCourseKeyPrm = customCourseKeyPrm courseKeyParamName

delUserFromGroupPrm :: Parameter Username
delUserFromGroupPrm = customUsernamePrm (fieldName delUserFromGroupField)

delUserFromGroupKeyPrm :: Parameter GroupKey
delUserFromGroupKeyPrm = customGroupKeyPrm groupKeyParamName

unsubscribeUserGroupKeyPrm :: Parameter GroupKey
unsubscribeUserGroupKeyPrm = customGroupKeyPrm groupKeyParamName

#ifdef TEST

-- Test create Just . id = encode . decode property
encodeDecodeProp :: (Eq a) => Parameter a -> (a -> Bool)
encodeDecodeProp = encodeDecodeCompare (==)

-- Compare the original value with the encoded and decoded value
-- with the given comparator.
encodeDecodeCompare :: (a -> a -> Bool) -> Parameter a -> (a -> Bool)
encodeDecodeCompare comp p x = maybe False (comp x) (decode p $ encode p x)

dataBridgeTests = do
  evalConfigPrmTest

#endif

