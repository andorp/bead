{-# LANGUAGE CPP #-}
module Bead.Domain.Entities where

import Bead.Domain.Types
import Bead.Domain.Evaluation
#ifdef TEST
import Bead.Invariants (Invariants(..), UnitTests(..))
#endif

import Data.Char (toLower)
import Data.List (findIndex)
import Data.Time (UTCTime(..), LocalTime, timeZoneName)
import Data.Time.Format (formatTime)
import qualified Data.Time as Time
import Control.Monad (join)
import Control.Applicative ((<$>), (<*>), (<|>))
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

-- * Course, exams, exercises, solutions

data AssignmentType
  = Normal
  | Urn
  deriving (Show, Eq, Read, Enum)

assignmentTypeCata
  normal
  urn
  a = case a of
    Normal -> normal
    Urn    -> urn

assignmentTypes = [Normal, Urn]

-- | Assignment for the student
data Assignment = Assignment {
    assignmentName :: String
  , assignmentDesc :: String
  , assignmentType :: AssignmentType
  , assignmentStart :: UTCTime
  , assignmentStartTimeZone :: TimeZone
  , assignmentEnd   :: UTCTime
  , assignmentEndTimeZone :: TimeZone
  -- TODO: Number of maximum tries
  } deriving (Eq, Show)

assignmentCata f (Assignment name desc type_ start starttz end endtz) =
  f name desc type_ start starttz end endtz

assignmentAna name desc type_ start starttz end endtz =
  Assignment <$> name <*> desc <*> type_ <*> start <*> starttz <*> end <*> endtz

-- | Produces True if the given time is between the start-end time of the assignment
isActivePeriod :: Assignment -> UTCTime -> Bool
isActivePeriod a t = and [start <= t, t <= end]
  where
    start = assignmentStart a
    end   = assignmentEnd   a

-- | Solution for one exercise
data Submission = Submission {
    solution         :: String
  , solutionPostDate :: UTCTime
  } deriving (Eq, Show)

-- | Comment on the text of exercise, on the evaluation
data Comment = Comment {
    comment     :: String
  , commentDate :: UTCTime
  } deriving (Eq, Show)

type CourseName = String

type UsersFullname = String

type EvaluationResult = EvaluationData Binary Percentage

type EvaluationConfig = EvaluationData () PctConfig

binaryEvalConfig :: EvaluationConfig
binaryEvalConfig = BinEval ()

percentageEvalConfig :: PctConfig -> EvaluationConfig
percentageEvalConfig p = PctEval p

class IsEvaluationResult e where
  mkEvalResult :: e -> EvaluationResult

instance IsEvaluationResult Binary where
  mkEvalResult = BinEval

instance IsEvaluationResult Percentage where
  mkEvalResult = PctEval

allBinaryEval :: [EvaluationData b p] -> Maybe [b]
allBinaryEval = sequence . map binaryEval

allPercentEval :: [EvaluationData b p] -> Maybe [p]
allPercentEval = sequence . map percentEval

evaluateResults :: EvaluationConfig -> [EvaluationResult] -> Maybe Result
evaluateResults (BinEval cfg) = fmap (flip calculateEvaluation cfg) . allBinaryEval
evaluateResults (PctEval cfg) = fmap (flip calculateEvaluation cfg) . allPercentEval

-- | Evaluation of a submission
data Evaluation = Evaluation {
    evaluationResult  :: EvaluationResult
  , writtenEvaluation :: String
  } deriving (Eq, Show)

resultString :: EvaluationResult -> String
resultString (BinEval (Binary Passed)) = "Passed"
resultString (BinEval (Binary Failed)) = "Failed"
resultString (PctEval p) =
  case point p of
    Nothing -> "No evaluation result, some internal error happened!"
    Just q  -> printf "%3.2f%%" (100.0 * q)

evaluationComment :: UTCTime -> Evaluation -> Comment
evaluationComment t e = Comment {
    comment = c
  , commentDate = t
  } where
     c = join [
           writtenEvaluation e, "\r\n"
         , resultString . evaluationResult $ e
         ]

newtype CourseCode = CourseCode String
  deriving (Eq, Ord, Show)

instance Str CourseCode where
  str (CourseCode s) = s

-- | A course represent a university course
data Course = Course {
    courseName :: String
  , courseDesc :: String
  , courseEvalConfig :: EvaluationConfig
  } deriving (Eq, Show)

courseCata course config (Course name desc cfg)
  = course name desc (config cfg)

-- | Groups are registered under the courses
data Group = Group {
    groupName  :: String
  , groupDesc  :: String
  , groupEvalConfig :: EvaluationConfig
  } deriving (Eq, Show)

groupCata group config (Group name desc cfg)
  = group name desc (config cfg)

-- | Workflows can happen to exams
data Workflow
  = W_Created
  | W_Open
  | W_Closed
  | W_Expired
  deriving (Eq, Show)

-- * Authorization and authentication

-- | Login roles
data Role
  = Student
  | GroupAdmin
  | CourseAdmin
  | Admin
  deriving (Eq, Ord, Enum)

roleCata
  student
  groupAdmin
  courseAdmin
  admin
  r = case r of
    Student     -> student
    GroupAdmin  -> groupAdmin
    CourseAdmin -> courseAdmin
    Admin       -> admin

roles = [Student, GroupAdmin, CourseAdmin, Admin]

-- Decides if the given role can admninstrate groups
-- Returns True if yes, otherwise False
groupAdmin = roleCata
  False
  True
  True
  False

data OutsideRole
  = EmptyRole
  | RegRole
  deriving (Eq, Ord)

parseRole :: String -> Maybe Role
parseRole "Student"      = Just Student
parseRole "Group Admin"  = Just GroupAdmin
parseRole "Course Admin" = Just CourseAdmin
parseRole "Admin"        = Just Admin
parseRole _              = Nothing

instance Show Role where
  show Student     = "Student"
  show GroupAdmin  = "Group Admin"
  show CourseAdmin = "Course Admin"
  show Admin       = "Admin"

atLeastCourseAdmin Admin       = True
atLeastCourseAdmin CourseAdmin = True
atLeastCourseAdmin _           = False

class InRole r where
  isAdmin       :: r -> Bool
  isCourseAdmin :: r -> Bool
  isGroupAdmin  :: r -> Bool
  isStudent     :: r -> Bool

instance InRole Role where
  isAdmin       = (== Admin)
  isCourseAdmin = (>= CourseAdmin)
  isGroupAdmin  = (>= GroupAdmin)
  isStudent     = (== Student)

-- * Permissions

-- | Granted permission on a given operation
data Permission
  = P_Open
  | P_Create
  | P_Modify
  | P_Delete
  deriving (Show, Eq, Enum)

canOpen, canCreate, canModify, canDelete :: Permission -> Bool

canOpen   = flip elem [P_Open, P_Create, P_Modify, P_Delete]
canCreate = flip elem [P_Create, P_Modify, P_Delete]
canModify = flip elem [P_Modify, P_Delete]
canDelete = flip elem [P_Delete]

-- | Permissions are allowed on the following objects
data PermissionObject
  = P_Assignment
  | P_UserReg
  | P_Submission
  | P_Evaluation
  | P_Comment
  | P_Statistics
  | P_Password
  | P_GroupAdmin
  | P_User
  | P_Course
  | P_Group
  | P_CourseAdmin
  | P_AdminPage
  | P_PlainPage
  deriving (Eq, Ord, Show, Enum)

-- Permission Objects are dynamically associated with values
class PermissionObj p where
  permissionObject :: p -> PermissionObject

newtype ObjectPermissions = ObjectPermissions { permissions :: [(Permission, PermissionObject)] }

data Authentication = Authentication
  deriving (Eq, Show)

data Authorization = Authorization {
    authoritation :: User -> Permission -> Erroneous [PermissionObject]
  , authorizated  :: User -> PermissionObject -> Erroneous [Permission]
  }

newtype Username = Username String
  deriving (Eq, Read, Ord)

usernameCata :: (String -> a) -> Username -> a
usernameCata f (Username u) = f u

class AsUsername c where
  asUsername :: c -> Username

instance Str Username where
  str (Username u) = u

-- | Username 'factory'
type FormatedUsername = String -> Erroneous Username

type Password = String

class AsPassword p where
  asPassword :: p -> Password

passwordCata :: (String -> a) -> Password -> a
passwordCata f p = f p

newtype Email = Email String
  deriving (Eq, Ord, Read)

emailFold :: (String -> a) -> Email -> a
emailFold f (Email e) = f e

parseEmail :: String -> Maybe Email
parseEmail = Just . Email

instance Show Email where
  show (Email e) = e

instance Str Email where
  str (Email e) = e

-- | Only accept normally formated email values
email :: String -> Erroneous Email
email = Right . Email

-- TODO: throw exception if email string is unacceptable
email' :: String -> Email
email' = Email

emailCata :: (String -> a) -> Email -> a
emailCata f (Email e) = f e

data TimeZone
  = UTC
  | CET
  | CEST
  deriving (Show, Read, Eq, Ord, Enum)

timeZoneCata utc cet cest t = case t of
  UTC -> utc
  CET -> cet
  CEST -> cest

-- Produces a TimeZone value defined in the
-- Data.Time module
dataTimeZone = timeZoneCata
  Time.utc
  (hoursToNamedTimeZone 1 "CET")
  (hoursToNamedTimeZone 2 "CEST")
  where
    hoursToNamedTimeZone hours name =
      (\t -> t { timeZoneName = name }) $ Time.hoursToTimeZone hours

showDate :: LocalTime -> String
showDate = formatTime defaultTimeLocale "%F, %T"

-- | Logged in user
data User = User {
    u_role     :: Role
  , u_username :: Username
  , u_email    :: Email
  , u_name     :: String
  , u_timezone :: TimeZone
  } deriving (Eq, Ord, Show)

userCata f (User role username email name timezone) =
  f role username email name timezone

userAna role username email name timezone = User
  <$> role
  <*> username
  <*> email
  <*> name
  <*> timezone

newtype PersonalInfo = PersonalInfo (Role, String, TimeZone)

personalInfoCata f (PersonalInfo (role, name, timezone))
  = f role name timezone

data UserDesc = UserDesc {
    ud_username :: Username
  , ud_fullname :: String
  } deriving (Eq, Ord, Show)

mkUserDescription :: User -> UserDesc
mkUserDescription u = UserDesc {
    ud_username = u_username u
  , ud_fullname = u_name u
  }

-- | User awaiting for registration
data UserRegistration = UserRegistration {
    reg_username :: String
  , reg_email    :: String
  , reg_name     :: String -- User's full name
  , reg_token    :: String -- Token for identification
  , reg_timeout  :: UTCTime
  } deriving (Eq, Show, Read)

userRegistrationFold
  :: (String -> String -> String -> String -> UTCTime -> a)
  -> UserRegistration
  -> a
userRegistrationFold f (UserRegistration u e n t d) = f u e n t d

-- * Data storage

newtype Stored key value = Stored (key, value)

storedKey   (Stored (k,_)) = k
storedValue (Stored (_,v)) = v

-- * PermObjs instance

instance PermissionObj Course where
  permissionObject _ = P_Course

instance PermissionObj Assignment where
  permissionObject _ = P_Assignment

instance PermissionObj UserRegistration where
  permissionObject _ = P_UserReg

-- * Show instances

instance Show Username where
  show (Username u) = u

-- * Ordering

-- Hungarian related charecter comparing, for special characters
-- uses the given list otherwise the normal comparism is called
-- capitals and non capitals are different characters
class CompareHun c where
  compareHun :: c -> c -> Ordering

instance CompareHun Char where
  compareHun c c' = maybe (compare c c') id
    ((compare <$> idxSmall   c <*> idxSmall   c') <|>
     (compare <$> idxCapital c <*> idxCapital c'))
    where
      idxSmall   x = findIndex (x==) hunSmall
      idxCapital x = findIndex (x==) hunCapital
      hunSmall   = "aábcdeéfghiíjklmnoóöőpqrstuúüűvwxyz"
      hunCapital = "AÁBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ"

instance CompareHun c => CompareHun [c] where
  compareHun [] []    = EQ
  compareHun [] (_:_) = LT
  compareHun (_:_) [] = GT
  compareHun (x:xs) (y:ys)
    = case compareHun x y of
        EQ -> compareHun xs ys
        other -> other

instance CompareHun Username where
  compareHun (Username u) (Username u') = compareHun u u'

instance CompareHun UserDesc where
  compareHun (UserDesc u n) (UserDesc u' n') =
    case compareHun n n' of
      EQ -> compareHun u u'
      other -> other

#ifdef TEST

-- * Invariants

compareHunTests = UnitTests [
    ("Small normal letters a-a", EQ == compareHun 'a' 'a')
  , ("Small normal letters d-z", LT == compareHun 'd' 'z')
  , ("Small normal letters z-a", GT == compareHun 'z' 'a')
  , ("Capital normal letters A-A", EQ == compareHun 'A' 'A')
  , ("Capital normal letters D-Z", LT == compareHun 'D' 'Z')
  , ("Capital normal letters Z-A", GT == compareHun 'Z' 'A')
  , ("Small accented letters á-á", EQ == compareHun 'á' 'á')
  , ("Small accented letters é-ú", LT == compareHun 'é' 'ú')
  , ("Small accented letters ű-á", GT == compareHun 'ű' 'á')
  , ("Capital accented letters Á-Á", EQ == compareHun 'á' 'á')
  , ("Capital accented letters É-Ú", LT == compareHun 'É' 'Ú')
  , ("Capital accented letters Ű-Á", GT == compareHun 'Ű' 'Á')
  ]

roleInvariants = Invariants [
    ( "Showing roles must generate string parseable by parseRole",
      \r -> ((Just r) ==) . parseRole . show $ r
    )
  ]

assignmentTests =
  let a = Assignment {
          assignmentName = "name"
        , assignmentDesc = "desc"
        , assignmentType = Normal
        , assignmentStart = read "2010-10-10 12:00:00 UTC"
        , assignmentStartTimeZone = UTC
        , assignmentEnd   = read "2010-11-10 12:00:00 UTC"
        , assignmentEndTimeZone = UTC
        }
      before  = read "2010-09-10 12:00:00 UTC"
      between = read "2010-10-20 12:00:00 UTC"
      after   = read "2010-12-10 12:00:00 UTC"
  in UnitTests [
    ("Time before active period", isFalse $ isActivePeriod a before)
  , ("Time in active period"    , isTrue  $ isActivePeriod a between)
  , ("Time after active period" , isFalse $ isActivePeriod a after)
  ]
  where
    isFalse = not
    isTrue  = id
#endif
