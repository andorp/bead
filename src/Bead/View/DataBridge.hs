{-# LANGUAGE CPP #-}
module Bead.View.DataBridge where

import Control.Monad ((>=>), join)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
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

import Test.Tasty.TestSet
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
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

instance SnapFieldName (Parameter a) where
  fieldName = fromString . name

-- Creates a request parameter value encoding the given value with the
-- given parameter
requestParameter :: Parameter a -> a -> ReqParam
requestParameter p x = parameterFold createValue p
  where
    createValue encode _ name _ _ = ReqParam (name, encode x)

mapParameter :: (b -> a) -> (a -> b) -> Parameter a -> Parameter b
mapParameter f g param = param {
    encode = encode param . f
  , decode = fmap g . decode param
  }

stringParameter :: String -> String -> Parameter String
stringParameter fieldName paramName = Parameter {
    encode = id
  , decode = Just . id
  , name = fieldName
  , decodeError = \m -> printf "%s could not be decoded: %s." paramName m
  , notFound    = printf "%s could not be found." paramName
  }

evaluationValuePrm :: Parameter String
evaluationValuePrm = stringParameter (fieldName evaluationValueField) "Evaluation Value"

customGroupKeyPrm :: String -> Parameter GroupKey
customGroupKeyPrm field = Parameter {
    encode = groupKeyMap id
  , decode = Just . GroupKey
  , name   = field
  , decodeError = \m -> printf "Invalid group key: %s." m
  , notFound    = "The given group key could not be found."
  }

-- Represents the GroupKey parameter
groupKeyPrm :: Parameter GroupKey
groupKeyPrm = customGroupKeyPrm (fieldName groupKeyName)

jsonGroupKeyPrm :: String -> Parameter GroupKey
jsonGroupKeyPrm field = jsonParameter field "group key"

customCourseKeyPrm :: String -> Parameter CourseKey
customCourseKeyPrm field = Parameter {
    encode = courseKeyMap id
  , decode = Just . CourseKey
  , name   = field
  , decodeError = \m -> printf "Invalid course key: %s." m
  , notFound    = "The given course key could not be found."
  }

jsonCourseKeyPrm :: String -> Parameter CourseKey
jsonCourseKeyPrm field = jsonParameter field "course key"

-- Represents the CourseKey parameter
courseKeyPrm :: Parameter CourseKey
courseKeyPrm = customCourseKeyPrm (fieldName courseKeyInfo)

-- Represents the AssessmentKey parameter
assessmentKeyPrm :: Parameter AssessmentKey
assessmentKeyPrm = Parameter {
    encode = assessmentKey id
  , decode = Just . AssessmentKey
  , name   = fieldName assessmentKeyField
  , decodeError = \m -> printf "Invalid assessment key: %s." m
  , notFound = "The given assessment key could not be found."
  }

scoreKeyPrm :: Parameter ScoreKey
scoreKeyPrm = Parameter {
    encode = scoreKey id
  , decode = Just . ScoreKey
  , name = fieldName scoreKeyField
  , decodeError = \m -> printf "Invalid score key: %s." m
  , notFound = "The given score key could not be found."
  }

-- Represents the AssignmentKey parameter
assignmentKeyPrm :: Parameter AssignmentKey
assignmentKeyPrm = Parameter {
    encode = assignmentKeyMap id
  , decode = Just . AssignmentKey
  , name   = fieldName assignmentKeyField
  , decodeError = \m -> printf "Invalid assignment key: %s." m
  , notFound    = "The given assignment key could not be found."
  }

-- Represents the SubmissionKey parameter
submissionKeyPrm :: Parameter SubmissionKey
submissionKeyPrm = Parameter {
    encode = submissionKeyMap id
  , decode = Just . SubmissionKey
  , name   = fieldName submissionKeyField
  , decodeError = \m -> printf "Invalid submission key: %s." m
  , notFound    = "The given submission key could not be found."
  }

-- Represents the TestScriptKey parameter
testScriptKeyPrm :: Parameter TestScriptKey
testScriptKeyPrm = Parameter {
    encode = testScriptKeyCata id
  , decode = Just . TestScriptKey
  , name = fieldName testScriptKeyField
  , decodeError = \m -> printf "Invalid test script key: %s." m
  , notFound = "The given test script key could not be found."
  }

-- Represents the SubmissionKey parameter
evaluationKeyPrm :: Parameter EvaluationKey
evaluationKeyPrm = Parameter {
    encode = evaluationKeyMap id
  , decode = Just . EvaluationKey
  , name   = fieldName evaluationKeyField
  , decodeError = \m -> printf "Invalid evaluation key: %s." m
  , notFound    = "The given evaluation key could not be found."
  }

-- JSON encodeable parameter, used in POST request parameters only
jsonParameter :: (Data a, Show a) => String -> String -> Parameter a
jsonParameter field name = Parameter {
    encode = \v -> fromMaybe (error $ concat [name, " jsonParameter: encodeToFay ",show v]) $ encodeToFay v
  , decode = decodeFromFay
  , name = field
  , decodeError = \v -> printf "Invalid JSON value: (%s, %s)" name v
  , notFound    = printf "%s could not be found." name
  }

evalConfigPrm :: EvaluationHook -> Parameter EvConfig
evalConfigPrm = evalConfigParameter . evHiddenValueId

evalConfigParameter :: String -> Parameter EvConfig
evalConfigParameter field = Parameter {
    encode = fromMaybe "evalConfigPrm: encodeToFay has failed" . encodeToFay
  , decode = readEvalConfig
  , name   = field
  , decodeError = \m -> printf "Invalid evaluation type: %s" m
  , notFound    = "The given evaluation type could not be found."
  }
  where
    readEvalConfig :: String -> Maybe EvConfig
    readEvalConfig [] = error "evalConfigPrm: Empty string"
    readEvalConfig xs = fmap convert $ decodeFromFay xs
      where
        convert :: EvConfig -> EvConfig
        convert = evaluationDataMap
          (const binaryConfig)
          (percentageConfig . guardPercentage)
          (const freeFormConfig)
          . evConfig

        guardPercentage :: Double -> Double
        guardPercentage x
           | 0 > x || x > 1 = error $ "Invalid percentage value:" ++ show x
           | otherwise = x

evaluationPercentagePrm :: Parameter Int
evaluationPercentagePrm
  = mapParameter show read
    $ stringParameter (fieldName evaluationPercentageField) "Evaluation percentage"

evaluationCommentOnlyPrm :: Parameter Bool
evaluationCommentOnlyPrm
  = mapParameter show read
    $ stringParameter (fieldName evaluationCommentOnlyField) "Evaluation comment only"

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
    [ Partition "Binary" (encode param binaryConfig) (Just binaryConfig) "Parsing failed"
    , Partition "Percenteage" (encode param (percentageConfig 0.96)) (Just (percentageConfig 0.96)) "Parsing failed"
    , Partition "FreeForm" (encode param freeFormConfig) (Just freeFormConfig) "Parsing failed"
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
        (withEvConfig c2 True (const False) False)
        (\p1 -> withEvConfig c2 False (\p2 -> abs (p1 - p2) < epsilon) False)
        (withEvConfig c2 False (const False) True)
      where
        epsilon = 0.0001

#endif

rolePrm :: Parameter Role
rolePrm = jsonParameter (fieldName userRoleField) "Role"

uidPrm :: Parameter Uid
uidPrm = jsonParameter (fieldName userUidField) "User ID"

jsonUsernamePrm :: String -> Parameter Username
jsonUsernamePrm field =
  let prm = jsonParameter field "Username"
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
  , notFound    = "The username could not be found."
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
  , decodeError = \m -> printf "Invalid email address: %s" m
  , notFound    = "The email address could not be found."
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
regPasswordPrm = validateBy isPassword $ passwordPrm (fieldName loginPassword) "Password"

regPasswordAgainPrm :: Parameter String
regPasswordAgainPrm = validateBy isPassword $ passwordPrm (fieldName regPasswordAgain) "Password (again)"

loginPasswordPrm :: Parameter String
loginPasswordPrm = validateBy isPassword $ passwordPrm (fieldName loginPassword) "Login password"

oldPasswordPrm :: Parameter String
oldPasswordPrm = validateBy isPassword $ passwordPrm (fieldName oldPasswordField) "Old password"

newPasswordPrm :: Parameter String
newPasswordPrm = validateBy isPassword $ passwordPrm (fieldName newPasswordField) "New password"

newPasswordAgainPrm :: Parameter String
newPasswordAgainPrm = validateBy isPassword $ passwordPrm (fieldName newPasswordAgainField) "New password (again)"

studentNewPwdPrm :: Parameter String
studentNewPwdPrm = validateBy isPassword $ passwordPrm (fieldName studentNewPwdField) "Password"

studentNewPwdAgainPrm :: Parameter String
studentNewPwdAgainPrm = validateBy isPassword $ passwordPrm (fieldName studentNewPwdAgainField) "Password (again)"

regUsernamePrm :: Parameter Username
regUsernamePrm = validateBy isUsername $ customUsernamePrm (fieldName loginUsername)

regFullNamePrm :: Parameter String
regFullNamePrm = stringParameter (fieldName regFullName) "Full name"

regUserRegKeyPrm :: Parameter UserRegKey
regUserRegKeyPrm = Parameter {
    encode = userRegKeyFold id
  , decode = Just . UserRegKey
  , name = (fieldName regUserRegKey)
  , decodeError = \m -> printf "Invalid registration key for this user: %s" m
  , notFound    = "There are no such registration key for this user."
  }

regTokenPrm :: Parameter String
regTokenPrm = stringParameter (fieldName regToken) "Registration token"

regLanguagePrm :: Parameter String
regLanguagePrm = stringParameter (fieldName regLanguage) "Registration language"

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
  , decodeError = \v -> printf "Invalid read value: (%s, %s)" name v
  , notFound    = printf "%s could not be found." name
  }

localTimeParam :: String -> String -> Parameter LocalTime
localTimeParam field name = Parameter {
    encode = show
  , decode = readMaybe
  , name = field
  , decodeError = \v -> printf "Invalid time value: (%s, %s)" name v
  , notFound    = printf "%s could not be found." name
  }

assignmentStartPrm :: Parameter LocalTime
assignmentStartPrm = localTimeParam (fieldName assignmentStartField) "Assignment start"

assignmentEndPrm :: Parameter LocalTime
assignmentEndPrm = localTimeParam (fieldName assignmentEndField) "Assignment end"

userTimeZonePrm :: Parameter TimeZoneName
userTimeZonePrm = jsonParameter (fieldName userTimeZoneField) "Time zone"

regTimeZonePrm :: Parameter TimeZoneName
regTimeZonePrm = jsonParameter (fieldName regTimeZoneField) "Time zone"

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

