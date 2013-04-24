module Bead.Domain.Relationships where

-- Bead imports

import Bead.Domain.Types
import Bead.Domain.Entities

-- Haskell imports

import Data.Time (UTCTime(..))

-- * Relations

type RolePermissions = [(Role,[(Permission, PermissionObject)])]

data AssignmentDesc = AssignmentDesc {
    aActive   :: Bool
  , aTitle    :: String
  , aGroup    :: String
  , aTeachers :: [String]
  , aOk       :: Int
  , aNew      :: Int
  , aBad      :: Int
  }

data GroupDesc = GroupDesc {
    gName   :: String
  , gAdmins :: [String]
  } deriving (Show)

data SubmissionDesc = SubmissionDesc {
    eGroup    :: String
  , eStudent  :: String
  , eSolution :: String
  , eType     :: EvaulationType
  , eAssignmentTitle :: String
  }

type Status = String
type EvaulatedBy = String

data SubmissionListDesc = SubmissionListDesc {
    slGroup   :: String
  , slTeacher :: [String]
  , slSubmissions :: [(SubmissionKey, UTCTime, Status, EvaulatedBy)]
  , slAssignmentText :: String
  }

data SubmissionDetailsDesc = SubmissionDetailsDesc {
    sdGroup :: String
  , sdTeacher :: [String]
  , sdAssignment :: String
  , sdStatus :: String
  , sdSubmission :: String
  , sdComments :: [String]
  }

data SubmissionInfo
  = Submission_Not_Found
  | Submission_Unevaulated
  | Submission_Passed EvaulationKey
  | Submission_Failed EvaulationKey

isPassed :: SubmissionInfo -> Bool
isPassed (Submission_Passed _) = True
isPassed _                     = False

siEvaulationKey :: SubmissionInfo -> Maybe EvaulationKey
siEvaulationKey Submission_Not_Found   = Nothing
siEvaulationKey Submission_Unevaulated = Nothing
siEvaulationKey (Submission_Failed ek) = Just ek
siEvaulationKey (Submission_Passed ek) = Just ek

type NoOfPassed = Int

data SubmissionTableInfo = SubmissionTableInfo {
    stCourse   :: String
  , stNumberOfAssignments :: Int
  , stAssignments :: [AssignmentKey] -- Cronologically ordered list of assignments
  , stUsers       :: [Username]      -- Alphabetically ordered list of usernames
  , stUserLines   :: [(UserDesc, NoOfPassed, [(AssignmentKey, SubmissionInfo)])]
  }

-- TODO
checkSubmissionTableInfo :: SubmissionTableInfo -> Bool
checkSubmissionTableInfo _ = True

data EvaulatedWith
  = EvHand

data UserSubmissionDesc = UserSubmissionDesc {
    usCourse         :: String
  , usAssignmentName :: String
  , usStudent        :: String
  , usSubmissions :: [(SubmissionKey, UTCTime, SubmissionInfo, EvaulatedWith)]
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

newtype GroupKey = GroupKey String
  deriving (Eq, Ord, Show)

newtype EvaulationKey = EvaulationKey String
  deriving (Eq, Ord, Show)

-- * Str instances

instance Str AssignmentKey where
  str (AssignmentKey s) = s

instance Str CourseKey where
  str (CourseKey c) = c

instance Str GroupKey where
  str (GroupKey g) = g


