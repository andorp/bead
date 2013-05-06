module Bead.Domain.Entities where

import Bead.Domain.Types
import Bead.Invariants (Invariants(..))

import Data.Char (toLower)
import Data.Time (UTCTime(..))
import Control.Monad (join)

-- * Course, exams, exercises, solutions

data AssignmentType
  = Normal
  | Urn
  deriving (Show, Eq, Read, Enum)

assignmentTypes = [Normal, Urn]

-- | Assignment for the student
data Assignment = Assignment {
    assignmentName :: String
  , assignmentDesc :: String
  , assignmentTCs  :: String
  , assignmentType :: AssignmentType
  , assignmentStart :: UTCTime
  , assignmentEnd   :: UTCTime
  , evaulationType  :: EvaulationType
  -- TODO: Number of maximum tries
  } deriving (Eq)

-- | Solution for one exercise
data Submission = Submission {
    solution         :: String
  , solutionPostDate :: UTCTime
  } deriving (Eq, Show)

-- | Comment on the text of exercise, on the evaulation
data Comment = Comment {
    comment     :: String
  , commentDate :: UTCTime
  } deriving (Eq, Show)

data EvaulationState
  = Passed Int
  | Failed Int
  deriving (Eq, Show, Read)

-- | Evaulation of a submission
data Evaulation = Evaulation {
    evaulationState   :: EvaulationState
  , writtenEvaulation :: String
  } deriving (Eq)

data EvaulationType
  = Scale
  deriving (Eq, Ord, Show, Read, Enum)

evaulationComment :: UTCTime -> Evaulation -> Comment
evaulationComment t e = Comment {
    comment = c
  , commentDate = t
  } where
     result (Passed points) = "Passed, points: " ++ show points
     result (Failed points) = "Failed, points: " ++ show points
     c = join [
           writtenEvaulation e, ", "
         , result . evaulationState $ e
         ]

newtype CourseCode = CourseCode String
  deriving (Eq, Ord, Show)

instance Str CourseCode where
  str (CourseCode s) = s

-- | A course represent a university course
data Course = Course {
    courseName :: String
  , courseDesc :: String
  } deriving (Eq, Show)

-- | Groups are registered under the courses
data Group = Group {
    groupName  :: String
  , groupDesc  :: String
  } deriving (Eq, Show)

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
  | Professor
  | CourseAdmin
  | Admin
  deriving (Eq, Ord, Enum)

roles = [Student, Professor, CourseAdmin, Admin]

data OutsideRole
  = EmptyRole
  | RegRole
  deriving (Eq, Ord)

parseRole :: String -> Maybe Role
parseRole "Student"      = Just Student
parseRole "Professor"    = Just Professor
parseRole "Course Admin" = Just CourseAdmin
parseRole "Admin"        = Just Admin
parseRole _              = Nothing

instance Show Role where
  show Student     = "Student"
  show Professor   = "Professor"
  show CourseAdmin = "Course Admin"
  show Admin       = "Admin"

atLeastCourseAdmin Admin       = True
atLeastCourseAdmin CourseAdmin = True
atLeastCourseAdmin _           = False

class InRole r where
  isAdmin       :: r -> Bool
  isCourseAdmin :: r -> Bool
  isProfessor   :: r -> Bool
  isStudent     :: r -> Bool

instance InRole Role where
  isAdmin       = (== Admin)
  isCourseAdmin = (>= CourseAdmin)
  isProfessor   = (>= Professor)
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
  | P_Submission
  | P_Evaulation
  | P_Comment
  | P_Statistics
  | P_Password
  | P_Professor
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
  deriving (Eq, Ord)

class AsUsername c where
  asUsername :: c -> Username

instance Str Username where
  str (Username u) = u

-- | The username is case insensitive because of practical reasons
username :: String -> Username
username = Username . (map toLower)

-- | Username 'factory'
type FormatedUsername = String -> Erroneous Username

type Password = String

class AsPassword p where
  asPassword :: p -> Password

newtype Email = Email String
  deriving (Eq, Ord)

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

-- | Logged in user
data User = User {
    u_role     :: Role
  , u_username :: Username
  , u_email    :: Email
  , u_name     :: String
  } deriving (Eq, Ord, Show)

data UserDesc = UserDesc {
    ud_username :: Username
  , ud_fullname :: String
  } deriving (Eq, Ord, Show)

mkUserDescription :: User -> UserDesc
mkUserDescription u = UserDesc {
    ud_username = u_username u
  , ud_fullname = u_name u
  }

-- * Data storage

newtype Stored key value = Stored (key, value)

storedKey   (Stored (k,_)) = k
storedValue (Stored (_,v)) = v

-- * PermObjs instance

instance PermissionObj Course where
  permissionObject _ = P_Course

instance PermissionObj Assignment where
  permissionObject _ = P_Assignment

-- * Read instances

instance Read Username where
  readsPrec _ s = [(username s,[])]

-- * Show instances

instance Show Username where
  show (Username u) = u

-- * Mock user

mockUser = User {
    u_role = Admin
  , u_username = username "andor2"
  , u_email = Email "mpegdj@gmail.com"
  , u_name = "Penzes Andor"
  }

mockCourse = Course {
    courseName = "Functional Programming"
  , courseDesc = "Course about how and why we would create programs by the definitive ways"
  }

mockGroup = Group {
    groupName = "Esti"
  , groupDesc = "Group description"
  }

-- * Invariants

roleInvariants = Invariants [
    ( "Showing roles must generate string parseable by parseRole",
      \r -> ((Just r) ==) . parseRole . show $ r
    )
  ]
