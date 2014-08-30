{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Relationships where

import Data.Data
import Data.Function (on)
import Data.List as List
import Data.Map (Map)
import Data.Time (UTCTime(..))

import Bead.Domain.Entities
import Bead.Domain.Entity.Assignment
import Bead.Domain.Entity.Comment
import Bead.Domain.Evaluation
import Bead.Domain.Shared.Evaluation
import Bead.Domain.Types


-- * Relations

data AssignmentDesc = AssignmentDesc {
    aActive   :: Bool
  , aTitle    :: String
  , aGroup    :: String
  , aTeachers :: [String]
  -- DeadLine for the assignment in UTC
  , aEndDate  :: UTCTime
  }

assignmentDescPermissions = ObjectPermissions [
    (P_Open, P_Assignment), (P_Open, P_Course)
  , (P_Open, P_Course)
  ]


data GroupDesc = GroupDesc {
    gName   :: String
  , gAdmins :: [String]
  } deriving (Show)

groupDescFold :: (String -> [String] -> a) -> GroupDesc -> a
groupDescFold f (GroupDesc n a) = f n a

groupDescPermissions = ObjectPermissions [
    (P_Open, P_Group)
  ]

data SubmissionDesc = SubmissionDesc {
    eCourse   :: String
  , eGroup    :: Maybe String
  , eStudent  :: String
  , eUsername :: Username
  , eSolution :: String
  , eConfig   :: EvConfig
  , eAssignmentKey   :: AssignmentKey
  , eAssignmentDate  :: UTCTime
  , eSubmissionDate  :: UTCTime
  , eAssignmentTitle :: String
  , eAssignmentDesc  :: String
  , eComments :: [Comment]
  , eFeedbacks :: [Feedback]
  }

submissionDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Submission), (P_Open, P_Assignment)
  , (P_Open, P_Comment)
  ]

-- Sets of the submission which are not evaluated yet.
data OpenedSubmissions = OpenedSubmissions {
    osAdminedCourse :: [(SubmissionKey, SubmissionDesc)]
    -- ^ Submissions by the users which are in the set of the users which attends on a course
    -- which is related to the user's registered group, and attends one of the user's group
  , osAdminedGroup  :: [(SubmissionKey, SubmissionDesc)]
    -- ^ Submissions by the users which are in the set of the users which attends on the user's groups
  , osRelatedCourse :: [(SubmissionKey, SubmissionDesc)]
    -- ^ Submissions by the users which are in the set of the users which attends on a course
    -- which is related to the user's registered group, and does not attend one of the user's group
  }

openedSubmissionsCata f (OpenedSubmissions admincourse admingroup relatedcourse)
  = f admincourse admingroup relatedcourse

type EvaluatedBy = String

-- List of the submissions made by a student for a given assignment
type UserSubmissionInfo = [(SubmissionKey, UTCTime, SubmissionInfo, EvaluatedBy)]

userSubmissionInfoCata
  :: ([a] -> b)
  -> ((SubmissionKey, UTCTime, SubmissionInfo, EvaluatedBy) -> a)
  -> UserSubmissionInfo
  -> b
userSubmissionInfoCata list info us = list $ map info us

-- List of the submission times made by a student for a given assignment
type UserSubmissionTimes = [UTCTime]

userSubmissionTimesCata
  :: ([a] -> b)
  -> (UTCTime -> a)
  -> UserSubmissionTimes
  -> b
userSubmissionTimesCata list time s = list $ map time s

data SubmissionListDesc = SubmissionListDesc {
    slGroup   :: String
  , slTeacher :: [String]
  , slSubmissions :: Either UserSubmissionTimes UserSubmissionInfo
  , slAssignment :: Assignment
  }

-- Sorts the given submission list description into descending order, by
-- the times of the given submissions
sortSbmListDescendingByTime :: SubmissionListDesc -> SubmissionListDesc
sortSbmListDescendingByTime s = s { slSubmissions = slSubmissions' }
  where
    userSubmissionTime (_submissionKey,time,_status,_evalatedBy) = time
    sortSubmissionTime = reverse . List.sort
    sortUserSubmissionInfo = reverse . List.sortBy (compare `on` userSubmissionTime)
    slSubmissions' = either (Left . sortSubmissionTime)
                            (Right . sortUserSubmissionInfo)
                            (slSubmissions s)

submissionListDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Submission), (P_Open, P_Assignment)
  ]

data SubmissionDetailsDesc = SubmissionDetailsDesc {
    sdGroup :: String
  , sdTeacher :: [String]
  , sdAssignment :: Assignment
  , sdStatus :: Maybe String
  , sdSubmission :: String
  , sdComments :: [Comment]
  , sdFeedbacks :: [Feedback]
  }

submissionDetailsDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Assignment), (P_Open, P_Submission)
  , (P_Open, P_Comment)
  ]

-- | Information about a submission for a given assignment
data SubmissionInfo
  = Submission_Not_Found
    -- ^ There is no submission.
  | Submission_Unevaluated
    -- ^ There is at least one submission which is not evaluated yet.
  | Submission_Tested Bool
    -- ^ There is at least one submission which is tested by the automation testing framework.
    -- The parameter is True if the submission has passed the tests, and False if has failed
    -- the tests.
  | Submission_Result EvaluationKey EvResult
    -- ^ There is at least submission with the evaluation.
  deriving (Show)

submissionInfoCata
  notFound
  unevaluated
  tested
  result
  s = case s of
    Submission_Not_Found   -> notFound
    Submission_Unevaluated -> unevaluated
    Submission_Tested r    -> tested r
    Submission_Result k r  -> result k r

siEvaluationKey :: SubmissionInfo -> Maybe EvaluationKey
siEvaluationKey = submissionInfoCata
  Nothing -- notFound
  Nothing -- unevaluated
  (const Nothing) -- tested
  (\key _result -> Just key) -- result

-- Information to display on the UI
data TestScriptInfo = TestScriptInfo {
    tsiName :: String
  , tsiDescription :: String
  , tsiType :: TestScriptType
  }

data SubmissionTableInfo
  = CourseSubmissionTableInfo {
      stiCourse :: String
    , stiUsers       :: [Username]      -- Alphabetically ordered list of usernames
    , stiAssignments :: [AssignmentKey] -- Cronologically ordered list of assignments
    , stiUserLines   :: [(UserDesc, Maybe Result, Map AssignmentKey SubmissionInfo)]
    , stiAssignmentInfos :: Map AssignmentKey Assignment
    , stiCourseKey :: CourseKey
    }
  | GroupSubmissionTableInfo {
      stiCourse :: String
    , stiUsers      :: [Username] -- Alphabetically ordered list of usernames
    , stiCGAssignments :: [CGInfo AssignmentKey] -- Cronologically ordered list of course and group assignments
    , stiUserLines :: [(UserDesc, Maybe Result, Map AssignmentKey SubmissionInfo)]
    , stiAssignmentInfos :: Map AssignmentKey Assignment
    , stiCourseKey :: CourseKey
    , stiGroupKey :: GroupKey
    }
  deriving (Show)

submissionTableInfoCata
  course
  group
  ti = case ti of
    CourseSubmissionTableInfo crs users asgs lines ainfos key ->
                       course crs users asgs lines ainfos key
    GroupSubmissionTableInfo  crs users asgs lines ainfos ckey gkey ->
                       group  crs users asgs lines ainfos ckey gkey

submissionTableInfoPermissions = ObjectPermissions [
    (P_Open, P_Course), (P_Open, P_Assignment)
  ]

data UserSubmissionDesc = UserSubmissionDesc {
    usCourse         :: String
  , usAssignmentName :: String
  , usStudent        :: String
  , usSubmissions :: [(SubmissionKey, UTCTime, SubmissionInfo)]
  } deriving (Show)

userSubmissionDescPermissions = ObjectPermissions [
    (P_Open, P_Course), (P_Open, P_Assignment), (P_Open, P_Submission)
  ]

data TCCreation
  = NoCreation
  | FileCreation TestScriptKey UsersFile
  | TextCreation TestScriptKey String
  deriving (Eq)

tcCreationCata
  noCreation
  fileCreation
  textCreation
  t = case t of
    NoCreation -> noCreation
    FileCreation tsk uf -> fileCreation tsk uf
    TextCreation tsk t  -> textCreation tsk t

data TCModification
  = NoModification
  | FileOverwrite TestScriptKey UsersFile
  | TextOverwrite TestScriptKey String
  | TCDelete
  deriving (Eq)

tcModificationCata
  noModification
  fileOverwrite
  textOverwrite
  delete
  t = case t of
    NoModification -> noModification
    FileOverwrite tsk uf -> fileOverwrite tsk uf
    TextOverwrite tsk t  -> textOverwrite tsk t
    TCDelete -> delete

-- * Entity keys

newtype AssignmentKey = AssignmentKey String
  deriving (Eq, Ord, Show)

assignmentKeyMap :: (String -> a) -> AssignmentKey -> a
assignmentKeyMap f (AssignmentKey x) = f x

newtype UserRegKey = UserRegKey String
  deriving (Eq, Ord, Show)

userRegKeyFold :: (String -> a) -> UserRegKey -> a
userRegKeyFold f (UserRegKey x) = f x

newtype CommentKey = CommentKey String
  deriving (Eq, Ord, Show)

newtype SubmissionKey = SubmissionKey String
  deriving (Eq, Ord, Show)

submissionKeyMap :: (String -> a) -> SubmissionKey -> a
submissionKeyMap f (SubmissionKey s) = f s

withSubmissionKey s f = submissionKeyMap f s

-- Key for a given Test Script in the persistence layer
newtype TestScriptKey = TestScriptKey String
  deriving (Data, Eq, Ord, Show, Read, Typeable)

-- Template function for the TestScriptKey value
testScriptKeyCata f (TestScriptKey x) = f x

-- Key for a given Test Case in the persistence layer
newtype TestCaseKey = TestCaseKey String
  deriving (Eq, Ord, Show)

-- Template function for the TestCaseKey value
testCaseKeyCata f (TestCaseKey x) = f x

-- Key for the Test Job that the test daemon will consume
newtype TestJobKey = TestJobKey String
  deriving (Eq, Ord, Show)

-- Template function for the TestJobKey value
testJobKeyCata f (TestJobKey x) = f x

-- Converts a TestJobKey to a SubmissionKey
testJobKeyToSubmissionKey = testJobKeyCata SubmissionKey

-- Converts a SubmissionKey to a TestJobKey
submissionKeyToTestJobKey = submissionKeyMap TestJobKey

newtype CourseKey = CourseKey String
  deriving (Data, Eq, Ord, Show, Typeable)

courseKeyMap :: (String -> a) -> CourseKey -> a
courseKeyMap f (CourseKey g) = f g

newtype GroupKey = GroupKey String
  deriving (Data, Eq, Ord, Show, Typeable)

groupKeyMap :: (String -> a) -> GroupKey -> a
groupKeyMap f (GroupKey g) = f g

newtype EvaluationKey = EvaluationKey String
  deriving (Eq, Ord, Show)

evaluationKeyMap :: (String -> a) -> EvaluationKey -> a
evaluationKeyMap f (EvaluationKey e) = f e

newtype FeedbackKey = FeedbackKey String
  deriving (Eq, Ord, Show)

feedbackKey f (FeedbackKey x) = f x

newtype ScoreKey = ScoreKey String
  deriving (Eq, Ord, Show)

scoreKey f (ScoreKey x) = f x

newtype AssessmentKey = AssessmentKey String
  deriving (Eq, Ord, Show)

assessmentKey f (AssessmentKey x) = f x

-- | The scoreboard summarizes the information for a course or group related
-- assesments and the evaluation for the assesment.
newtype ScoreBoard = ScoreBoard (Map (AssessmentKey, Username) EvaluationKey)
  deriving (Eq, Show)
