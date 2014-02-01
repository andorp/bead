module Bead.View.Snap.DataBridge where

import Control.Monad (join)
import Data.Char (toUpper)
import Data.Time (UTCTime(..))
import Text.Printf (printf)

import Bead.Domain.Types (readMaybe)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Validators
import Bead.View.Snap.Dictionary
import Bead.View.Snap.RouteOf
import Bead.View.Snap.RequestParams

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

customCourseKeyPrm :: String -> Parameter CourseKey
customCourseKeyPrm field = Parameter {
    encode = courseKeyMap id
  , decode = Just . CourseKey
  , name   = field
  , decodeError = \m -> printf "Érvénytelen tárgyazonosító: %s!" m
  , notFound    = "A tárgyazonosító nem található!"
  }

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

-- Represents the SubmissionKey parameter
evaluationKeyPrm :: Parameter EvaluationKey
evaluationKeyPrm = Parameter {
    encode = evaluationKeyMap id
  , decode = Just . EvaluationKey
  , name   = fieldName evaluationKeyField
  , decodeError = \m -> printf "Érvénytelen értékelésazonosító: %s!" m
  , notFound    = "Az értékelésazonosító nem található!"
  }

evalConfigPrm :: EvaluationHook -> Parameter EvaluationConfig
evalConfigPrm hook = Parameter {
    encode = show
  , decode = readEvalConfig
  , name   = evHiddenValueId hook
  , decodeError = \m -> printf "Hibás értékelési fajta: %s!" m
  , notFound    = "Nem található az értékelés fajtája!"
  }
  where
    readEvalConfig :: String -> Maybe (EvaluationData () PctConfig)
    readEvalConfig = fmap convert . readMaybe
      where
        convert :: EvaluationData () Double -> EvaluationData () PctConfig
        convert = evaluationDataMap (BinEval . id) (PctEval . PctConfig)

rolePrm :: Parameter Role
rolePrm = Parameter {
    encode = show
  , decode = parseRole
  , name   = fieldName userRoleField
  , decodeError = \m -> printf "Érvénytelen szerepkör: %s!" m
  , notFound    = "A szerepkör nem található!"
  }

customUsernamePrm :: String -> Parameter Username
customUsernamePrm field = Parameter {
    encode = usernameCata id
  , decode = decodeUsr
  , name = field
  , decodeError = \m -> printf "%s: %s" (message isUsername) m
  , notFound    = "A felhasználó nem található!"
  } where
    decodeUsr xs =
      if (validator isUsername xs)
         then (Just $ Username $ map toUpper xs)
         else Nothing

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
  , decodeError = \v -> printf "Hibás érték: (%s, %s)!" name v
  , notFound    = printf "%s nem található!" name
  }

assignmentTypePrm :: Parameter AssignmentType
assignmentTypePrm = readablePrm (fieldName assignmentTypeField) "Feladat típusa"

utcTimeParam :: TimeZone -> String -> String -> Parameter UTCTime
utcTimeParam timezone field name = Parameter {
    encode = show
  , decode = readMaybe . addTimePostFix
  , name = field
  , decodeError = \v -> printf "Hibás érték: (%s, %s)!" name v
  , notFound    = printf "%s nem található!" name
  } where
      addTimePostFix s = (s++(timeZoneCata " UTC" " CET" " CEST" timezone))

assignmentStartPrm :: TimeZone -> Parameter UTCTime
assignmentStartPrm t = utcTimeParam t (fieldName assignmentStartField) "Beküldés kezdete"

assignmentEndPrm :: TimeZone -> Parameter UTCTime
assignmentEndPrm t = utcTimeParam t (fieldName assignmentEndField) "Beküldés vége"

userTimeZonePrm :: Parameter TimeZone
userTimeZonePrm = readablePrm (fieldName userTimeZoneField) "Időzóna"

regTimeZonePrm :: Parameter TimeZone
regTimeZonePrm = readablePrm (fieldName regTimeZoneField) "Időzóna"

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
userLanguagePrm = languagePrm (fieldName userLanguageField)

delUserFromCoursePrm :: Parameter Username
delUserFromCoursePrm = customUsernamePrm (fieldName delUserFromCourseField)

delUserFromCourseKeyPrm :: Parameter CourseKey
delUserFromCourseKeyPrm = customCourseKeyPrm courseKeyParamName
