module Bead.Domain.Relationships where

import Bead.Domain.Types
import Bead.Domain.Entities

-- * Relations

type RolePermissions = [(Role,[(Permission, PermissionObject)])]

data AssignmentDesc = AssignmentDesc {
    aActive :: Bool
  , aTitle  :: String
  , aDesc   :: String
  , aOk     :: Int
  , aNew    :: Int
  , aBad    :: Int
  }

-- * Entity keys

newtype AssignmentKey = AssignmentKey String
  deriving (Eq, Ord, Show)

newtype UserKey = UserKey String
  deriving (Eq, Ord, Show)

newtype CommentKey = CommentKey String
  deriving (Eq, Ord, Show)

newtype SubmissionKey = SubmissionKey String
  deriving (Eq, Ord, Show)

newtype TestKey = TestKey String
  deriving (Eq, Ord, Show)

newtype CourseKey = CourseKey String
  deriving (Eq, Ord, Show)

data GroupKey = GroupKey String
  deriving (Eq, Ord, Show)

-- * Str instances

instance Str AssignmentKey where
  str (AssignmentKey s) = s

instance Str CourseKey where
  str (CourseKey c) = c

instance Str GroupKey where
  str (GroupKey g) = g


