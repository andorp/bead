module Bead.Domain.Entities where

import Bead.Domain.Types

import Data.Char (toLower)

-- * Course, exams, exercises, solutions

-- | Exercise, that needs to be solved
data Exercise = Exercise {
    exercise :: String
  } deriving (Eq, Show)

-- | Test against an exercise
data Test = Test {
    testDesc :: String
  } deriving (Eq, Show)

-- | Solution for one exercise
data Solution = Solution {
    solution         :: String
  , solutionPostDate :: Date
  } deriving (Eq, Show)

-- | Comment on the text of exercise, on the evaulation
data Comment = Comment {
    comment :: String
  } deriving (Eq, Show)

-- | Evaulation of an exercise or an exam
data Evaulation
  = Failed
  | Passed Integer
  deriving (Eq, Ord, Show)

data EvaulationType
  = Scale
  deriving (Eq, Show)

newtype CourseCode = CourseCode String
  deriving (Eq, Ord, Show)

instance Str CourseCode where
  str (CourseCode s) = s

-- | A course represent a university course
data Course = Course {
    courseCode :: CourseCode
  , courseName :: String
  , courseDesc :: String
  } deriving (Eq, Show)

-- | Groups are registered under the courses
data Group = Group {
    groupCode  :: String
  , groupName  :: String
  , groupDesc  :: String
  , groupUsers :: [Username]
  } deriving (Eq, Show)

-- | The type of the exam
data ExamType
  = Submition
  | Zh
  | Exam
  deriving (Eq, Show)

newtype ExamInfo = ExamInfo (String, Date)
  deriving (Eq, Ord, Show)

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
  deriving (Read, Eq, Ord)

instance Show Role where
  show Student     = "Student"
  show Professor   = "Professor"
  show CourseAdmin = "Course Admin"
  show Admin       = "Admin"

-- * Permissions

-- | Granted permission on a given operation
data Permission
  = P_Open
  | P_Create
  | P_Modify
  | P_Delete
  deriving (Show, Eq)

canOpen, canCreate, canModify, canDelete :: Permission -> Bool

canOpen   = flip elem [P_Open, P_Create, P_Modify, P_Delete]
canCreate = flip elem [P_Create, P_Modify, P_Delete]
canModify = flip elem [P_Modify, P_Delete]
canDelete = flip elem [P_Delete]

-- | Permissions are allowed on the following objects
data PermissionObject
  = P_Exercise
  | P_Solution
  | P_Statistics
  | P_Password
  | P_Professor
  | P_Course
  | P_CourseAdmin
  | P_AdminPage
  | P_PlainPage
  deriving (Eq, Ord, Show)

-- Permission Objects are dynamically associated with values
class PermissionObj p where
  permissionObject :: p -> PermissionObject

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

-- * Data storage

newtype Stored key value = Stored (key, value)

storedKey   (Stored (k,_)) = k
storedValue (Stored (_,v)) = v

-- * PermObjs instance

instance PermissionObj Course where
  permissionObject _ = P_Course

instance PermissionObj Exercise where
  permissionObject _ = P_Exercise

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
    courseCode = CourseCode "FP-001"
  , courseName = "Functional Programming"
  , courseDesc = "Course about how and why we would create programs by the definitive ways"
  }

mockGroup = Group {
    groupCode = "G010"
  , groupName = "Esti"
  , groupDesc = "Group description"
  , groupUsers = [Username "andor", Username "zsolt"]
  }
