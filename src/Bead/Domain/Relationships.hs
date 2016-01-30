{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
module Bead.Domain.Relationships where

import Data.Data
import Data.Function (on)
import Data.List as List hiding (group)
import Data.Map (Map)
import Data.Time (UTCTime(..))

import Bead.Domain.Entities
import Bead.Domain.Evaluation

#ifdef TEST
import Test.Tasty.Arbitrary
import Test.Tasty.TestSet
#endif

-- * Relations

-- The submission limitation for an assignment
data SubmissionLimitF a
  = Unlimited a -- Unlimited submissions are allowed
  | Remaining Int a -- Positive number of the remaning submission
  | Reached a -- The submission limit is already reached
  deriving (Eq, Functor, Show, Ord)

type SubmissionLimit = SubmissionLimitF ()

unlimited   = Unlimited ()
remaining x = Remaining x ()
reached     = Reached ()

submissionLimit
  unlimited
  remaining
  reached
  sl = case sl of
    Unlimited x   -> unlimited x
    Remaining n x -> remaining n x
    Reached x     -> reached x

-- Calc the Submission Limit for the assignment and the given number of submissions
calcSubLimit :: Assignment -> Int -> SubmissionLimit
calcSubLimit assignment noOfSubmissions = noOfTries unlimited limited $ aspects assignment
  where
    limited limit =
      let rest = limit - noOfSubmissions
      in if rest > 0 then (remaining rest) else reached

#ifdef TEST
calcSubLimitTests = group "calcSubLimit" $ do
  assertProperty
      "No no of tries given"
      (==unlimited)
      (do asg <- fmap clear arbitrary
          sbm <- choose (-100, 100)
          return $ calcSubLimit asg sbm)
      "No of tries is recognized"
  assertProperty
      "No of tries is given and exceeds the limit"
      (==reached)
      (do lmt <- choose (1,100)
          asg <- fmap (set lmt) arbitrary
          sbm <- choose (lmt,lmt + 100)
          return $ calcSubLimit asg sbm)
      "Limit is not reached"
  assertProperty
      "Submissions are not reached the limit"
      (\(lmt,sbm,sbl) -> remaining (lmt - sbm) == sbl)
      (do lmt <- choose (1,100)
          asg <- fmap (set lmt) arbitrary
          sbm <- choose (0,lmt-1)
          return $ (lmt,sbm,calcSubLimit asg sbm))
      "Remaining is not calculated properly"
  where
    clear a = a {aspects = clearNoOfTries (aspects a)}
    set n a = a {aspects = setNoOfTries n (aspects a)}
#endif

data AssignmentDesc = AssignmentDesc {
    aActive   :: Bool
  , aIsolated :: Bool
  , aLimit    :: SubmissionLimit
  , aTitle    :: String
  , aGroup    :: String
  , aTeachers :: [String]
  -- DeadLine for the assignment in UTC
  , aEndDate  :: UTCTime
  } deriving (Eq, Ord, Show)

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
  , eUid      :: Uid
  , eSolution :: String
  , eAssignmentKey   :: AssignmentKey
  , eAssignment      :: Assignment
  , eAssignmentDate  :: UTCTime
  , eSubmissionDate  :: UTCTime
  , eComments :: [Comment]
  , eFeedbacks :: [Feedback]
  }

submissionDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Submission), (P_Open, P_Assignment)
  , (P_Open, P_Comment)
  ]

-- Sets of the submission which are not evaluated yet.
data OpenedSubmissionsS a = OpenedSubmissions {
    osAdminedCourse :: [(SubmissionKey, a)]
    -- ^ Submissions by the users which are in the set of the users which attends on a course
    -- which is related to the user's registered group, and attends one of the user's group
  , osAdminedGroup  :: [(SubmissionKey, a)]
    -- ^ Submissions by the users which are in the set of the users which attends on the user's groups
  , osRelatedCourse :: [(SubmissionKey, a)]
    -- ^ Submissions by the users which are in the set of the users which attends on a course
    -- which is related to the user's registered group, and does not attend one of the user's group
  }

type OpenedSubmissions = OpenedSubmissionsS SubmissionDesc

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
  deriving (Eq, Show)

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

withSubmissionInfo s notFound unevaluated tested result
  = submissionInfoCata notFound unevaluated tested result s

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

submissionTableInfoToCourseGroupKey :: SubmissionTableInfo -> Either CourseKey GroupKey
submissionTableInfoToCourseGroupKey t@(CourseSubmissionTableInfo {}) = Left $ stiCourseKey t
submissionTableInfoToCourseGroupKey t@(GroupSubmissionTableInfo {}) = Right $ stiGroupKey t

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

newtype NotificationKey = NotificationKey String
  deriving (Eq, Ord, Show)

notificationKey f (NotificationKey x) = f x

-- | Information about a score for a given assessment
data ScoreInfo
  = Score_Not_Found
    -- ^ There is no score.
  | Score_Result EvaluationKey EvResult
    -- ^ There is a score for a given assessment and user.
  deriving (Eq, Show)

scoreInfoAlgebra
  notFound
  result
  s = case s of
    Score_Not_Found   -> notFound
    Score_Result ek r -> result ek r

-- | The scoreboard summarizes the information for a course or group related
-- assesments and the evaluation for the assessment.
data ScoreBoard = 
    CourseScoreBoard {
      sbScores :: Map (AssessmentKey,Username) ScoreKey
    , sbScoreInfos :: Map ScoreKey ScoreInfo
    , sbCourseKey :: CourseKey
    , sbCourseName :: String
    , sbAssessments :: [AssessmentKey]
    , sbAssessmentInfos :: Map AssessmentKey Assessment
    , sbUsers :: [UserDesc]
    }
  | GroupScoreBoard {
      sbScores :: Map (AssessmentKey,Username) ScoreKey
    , sbScoreInfos :: Map ScoreKey ScoreInfo
    , sbGroupKey :: GroupKey
    , sbGroupName :: String
    , sbAssessments :: [AssessmentKey]
    , sbAssessmentInfos :: Map AssessmentKey Assessment
    , sbUsers :: [UserDesc]
    }
  deriving (Eq, Show)

data AssessmentDesc = AssessmentDesc {
    adCourse        :: String
  , adGroup         :: Maybe String
  , adAssessmentKey :: AssessmentKey
  , adAssessment    :: Assessment
  }

scoreBoardPermissions = ObjectPermissions
  [ (P_Open, P_Group), (P_Open, P_Assessment) ]

#ifdef TEST
relationshipTests = group "Bead.Domain.Relationships" $ do
  calcSubLimitTests
#endif
