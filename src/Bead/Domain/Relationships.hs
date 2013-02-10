module Bead.Domain.Relationships where

import Bead.Domain.Types
import Bead.Domain.Entities

-- * Relations

type RolePermissions = [(Role,[(Permission, PermissionObject)])]

data Exam = Exam {
    e_type      :: ExamType
  , e_course    :: Course
  , e_group     :: Group
  , e_postDate  :: Date
  , e_exercises :: [(Stored ExerciseKey Exercise, [Stored TestKey Test], [Stored CommentKey Comment])]
  , e_comments  :: [Stored CommentKey Comment]
  }

data UserSolutions = UserSolutions {
    us_exercise  ::  Stored ExerciseKey Exercise
  , us_user      ::  Stored UserKey User
  , us_comments  :: [Stored CommentKey Comment]
  , us_solutions :: [Stored SolutionKey Solution]
  }

type ExamSolutions = (Exam, [UserSolutions])

type StateTransition s = s -> [s]

-- * Test runner

type TestRunner = Exercise -> Solution -> Test -> Evaulation

type TestRunnerIO = Exercise -> Solution -> Test -> IO Evaulation

-- * Entity keys

newtype ExerciseKey = ExerciseKey String
  deriving (Eq, Ord, Show)

newtype UserKey = UserKey String
  deriving (Eq, Ord, Show)

newtype CommentKey = CommentKey String
  deriving (Eq, Ord, Show)

newtype SolutionKey = SolutionKey String
  deriving (Eq, Ord, Show)

newtype TestKey = TestKey String
  deriving (Eq, Ord, Show)

newtype CourseKey = CourseKey String
  deriving (Eq, Ord, Show)

data GroupKey = GroupKey CourseKey String
  deriving (Eq, Ord, Show)

-- * Str instances

instance Str ExerciseKey where
  str (ExerciseKey s) = s

-- * Authentication

authTable :: [(User, Permission, PermissionObject)] -> Authorization
authTable = undefined

