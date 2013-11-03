module Bead.View.Snap.DataBridge where

import Control.Monad (join)
import Data.Time (UTCTime(..))

import Bead.Domain.Types (readMaybe)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaulation
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Validators
import Bead.View.Snap.RouteOf

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
  , decodeError = \m -> join [paramName, " is not decoded: ", m]
  , notFound    = join [paramName, " is not found."]
  }

evaluationValuePrm :: Parameter String
evaluationValuePrm = stringParameter (fieldName evaulationValueField) "Evaluation Value"

customGroupKeyPrm :: String -> Parameter GroupKey
customGroupKeyPrm field = Parameter {
    encode = groupKeyMap id
  , decode = Just . GroupKey
  , name   = field
  , decodeError = ("Invalid Group Key is given: "++)
  , notFound    = "Group Key is not found"
  }

-- Represents the GroupKey parameter
groupKeyPrm :: Parameter GroupKey
groupKeyPrm = customGroupKeyPrm (fieldName groupKeyName)

customCourseKeyPrm :: String -> Parameter CourseKey
customCourseKeyPrm field = Parameter {
    encode = courseKeyMap id
  , decode = Just . CourseKey
  , name   = field
  , decodeError = ("Invalid Course Key is given: "++)
  , notFound    = "Course Key is not found"
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
  , decodeError = ("Invalid Assignment Key is given: "++)
  , notFound    = "Assignment Key is not found"
  }

-- Represents the SubmissionKey parameter
submissionKeyPrm :: Parameter SubmissionKey
submissionKeyPrm = Parameter {
    encode = submissionKeyMap id
  , decode = Just . SubmissionKey
  , name   = fieldName submissionKeyField
  , decodeError = ("Invalid Submission Key is given: "++)
  , notFound    = "Submission Key is not found"
  }

-- Represents the SubmissionKey parameter
evaluationKeyPrm :: Parameter EvaulationKey
evaluationKeyPrm = Parameter {
    encode = evaluationKeyMap id
  , decode = Just . EvaulationKey
  , name   = fieldName evaulationKeyField
  , decodeError = ("Invalid Evaluation Key is given: "++)
  , notFound    = "Evaluation Key is not found"
  }

evalConfigPrm :: EvaulationHook -> Parameter EvaulationConfig
evalConfigPrm hook = Parameter {
    encode = show
  , decode = readEvalConfig
  , name   = evHiddenValueId hook
  , decodeError = ("Invalid evaulation config is given: "++)
  , notFound = "Evaluation config is not found"
  }
  where
    readEvalConfig :: String -> Maybe (EvaulationData () PctConfig)
    readEvalConfig = fmap convert . readMaybe
      where
        convert :: EvaulationData () Double -> EvaulationData () PctConfig
        convert = evaluationDataMap (BinEval . id) (PctEval . PctConfig)

rolePrm :: Parameter Role
rolePrm = Parameter {
    encode = show
  , decode = parseRole
  , name   = fieldName userRoleField
  , decodeError = ("Invalid role is given: "++)
  , notFound    = "Role is not found"
  }

customUsernamePrm :: String -> Parameter Username
customUsernamePrm field = Parameter {
    encode = usernameCata id
  , decode = Just . Username
  , name = field
  , decodeError = ("Invalid username is given: "++)
  , notFound    = "Username is not found"
  }

usernamePrm :: Parameter Username
usernamePrm = customUsernamePrm (fieldName usernameField)

emailPrm :: String -> Parameter Email
emailPrm field = validateBy isEmailAddress $ Parameter {
    encode = emailFold id
  , decode = parseEmail
  , name = field
  , decodeError = ("Invalid email is given: "++)
  , notFound = "Email is not found"
  }

userEmailPrm :: Parameter Email
userEmailPrm = emailPrm (fieldName userEmailField)

regEmailPrm :: Parameter Email
regEmailPrm = emailPrm (fieldName regEmailAddress)

regPasswordPrm :: Parameter String
regPasswordPrm = validateBy isPassword $ stringParameter (fieldName loginPassword) "Password"

regPasswordAgainPrm :: Parameter String
regPasswordAgainPrm = validateBy isPassword $ stringParameter (fieldName regPasswordAgain) "Password Again"

oldPasswordPrm :: Parameter String
oldPasswordPrm = validateBy isPassword $ stringParameter (fieldName oldPasswordField) "Old password"

newPasswordPrm :: Parameter String
newPasswordPrm = validateBy isPassword $ stringParameter (fieldName newPasswordField) "New password"

newPasswordAgainPrm :: Parameter String
newPasswordAgainPrm = validateBy isPassword $ stringParameter (fieldName newPasswordAgainField) "New password "

regUsernamePrm :: Parameter Username
regUsernamePrm = validateBy isUsername $ customUsernamePrm (fieldName loginUsername)

regFullNamePrm :: Parameter String
regFullNamePrm = stringParameter (fieldName regFullName) "Full Name"

regUserRegKeyPrm :: Parameter UserRegKey
regUserRegKeyPrm = Parameter {
    encode = userRegKeyFold id
  , decode = Just . UserRegKey
  , name = (fieldName regUserRegKey)
  , decodeError = ("Invalid value is given for User RegistrationKey: "++)
  , notFound = "User Registration Key is not found"
  }

regTokenPrm :: Parameter String
regTokenPrm = stringParameter (fieldName regToken) "Registration Token"

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
  , decodeError = (\v -> "Invalid value is given for, " ++ name ++ " " ++ v)
  , notFound = name ++ " is not found"
  }

assignmentTypePrm :: Parameter AssignmentType
assignmentTypePrm = readablePrm (fieldName assignmentTypeField) "Assignment Type"

utcTimeParam :: TimeZone -> String -> String -> Parameter UTCTime
utcTimeParam timezone field name = Parameter {
    encode = show
  , decode = readMaybe . addTimePostFix
  , name = field
  , decodeError = (\v -> "Invalid value is given for, " ++ name ++ " " ++ v)
  , notFound = name ++ " is not found"
  } where
      addTimePostFix s = (s++(timeZoneCata " UTC" " CET" " CEST" timezone))

assignmentStartPrm :: TimeZone -> Parameter UTCTime
assignmentStartPrm t = utcTimeParam t (fieldName assignmentStartField) "Assignment Start"

assignmentEndPrm :: TimeZone -> Parameter UTCTime
assignmentEndPrm t = utcTimeParam t (fieldName assignmentEndField) "Assignment End"

userTimeZonePrm :: Parameter TimeZone
userTimeZonePrm = readablePrm (fieldName userTimeZoneField) "User time zone"
