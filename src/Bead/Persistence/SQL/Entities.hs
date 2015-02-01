{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bead.Persistence.SQL.Entities where

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time hiding (TimeZone)

import           Database.Persist.Sqlite
import           Database.Persist.TH

import qualified Bead.Domain.Entities as Domain

#ifdef TEST
import           Test.Themis.Keyword.Encaps
#endif

-- String represents a JSON value
type JSONText = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Assessment
  description Text
  evalConfig  JSONText
  deriving Show

Assignment
  name        Text
  description Text
  type        JSONText
  start       UTCTime
  end         UTCTime
  created     UTCTime
  evalConfig  JSONText
  deriving Show

Comment
  text   Text
  author Text
  date   UTCTime
  type   JSONText
  deriving Show

Course
  name        Text
  description Text
  testScriptType JSONText
  deriving Show

Evaluation
  result  JSONText
  written Text
  deriving Show

Feedback
  info JSONText
  date UTCTime
  deriving Show

Group
  name        Text
  description Text
  deriving Show

Score
  score JSONText
  deriving Show

Submission
  simple   Text       Maybe
  zipped   ByteString Maybe
  postDate UTCTime
  deriving Show

TestCase
  name         Text
  description  Text
  simpleValue  Text       Maybe
  zippedValue  ByteString Maybe
  info         Text
  deriving Show

TestScript
  name        Text
  description Text
  notes       Text
  script      Text
  testScriptType JSONText
  deriving Show

User
  role     JSONText
  username Text
  email    Text
  name     Text
  timeZone JSONText
  language Text
  uid      Text
  UniqueUsername username
  deriving Show

UserRegistration
  username Text
  email    Text
  name     Text
  token    Text
  timeout  UTCTime
  deriving Show

-- Connections between objects

-- Submission -> [Feedback]
-- Feedback -> Submission
FeedbacksOfSubmission
  submission SubmissionId
  feedback   FeedbackId
  UniqueSubmissionFeedbackPair submission feedback
  UniqueSubmisisonFeedback feedback
  deriving Show

-- Assignment -> [Submission]
SubmissionsOfAssignment
  assignment AssignmentId
  submission SubmissionId
  UniqueSubmissionsOfAssignmentPair assignment submission
  deriving Show

-- Assignment -> TestCase
-- Only one assignment is allowed for the test case
TestCaseOfAssignment
  assignment AssignmentId
  testCase   TestCaseId
  UniqueTestCaseToAssignment assignment testCase
  UniqueAssignmentOfTestCase assignment
  deriving Show

-- Course -> [User]
AdminsOfCourse
  course CourseId
  admin  UserId
  UniqueAdminsOfCourse course admin
  deriving Show

-- Course -> [Assignment]
AssignmentsOfCourse
  course CourseId
  assignment AssignmentId
  UniqueAssignmentsOfCoursePair course assignment
  deriving Show

-- Course -> [Assesment]
AssessmentsOfCourse
  course CourseId
  assessment AssessmentId
  UniqueAssessmentsOfCoursePair course assessment
  deriving Show

-- Group -> [Assesment]
AssessmentsOfGroup
  group      GroupId
  assessment AssessmentId
  UniqueAssessmentsOfGroupPair group assessment
  deriving Show

-- Score -> (Username, Assessment)
-- (Username, Assessment) -> [Score]
ScoresOfUsernameAssessment
  score ScoreId
  user  UserId
  assessment AssessmentId
  UniqueScoresOfUsernameAssessment score user assessment
  UniqueScoreOfUsernameAssessment score

-- Course -> [Group]
-- Group -> Course
GroupsOfCourse
  course CourseId
  group  GroupId
  UniqueGroupCoursePair course group
  UniqueGroupCourseGroup group
  deriving Show

-- Course -> [TestScript]
TestScriptsOfCourse
  course     CourseId
  testScript TestScriptId
  UniqueTestScriptsOfCourse course testScript
  deriving Show

-- Course -> [User]
UnsubscribedUsersFromCourse
  course CourseId
  user   UserId
  UniqueUnsubscribedUsersFromCourse course user
  deriving Show

-- Course -> [User]
UsersOfCourse
  course CourseId
  user   UserId
  UniqueUsersOfCoursePair course user
  deriving Show

-- Group -> [User]
AdminsOfGroup
  group GroupId
  admin UserId
  UniqueAdminsOfGroupPair group admin
  deriving Show

-- Group -> [Assignment]
AssignmentsOfGroup
  group      GroupId
  assignment AssignmentId
  UniqueAssignmentsOfGroupPair group assignment
  deriving Show

-- Group -> [User]
UsersOfGroup
  group GroupId
  user  UserId
  UniqueUsersOfGroupPair group user
  deriving Show

-- Group -> [User]
UnsubscribedUsersFromGroup
  group GroupId
  user  UserId
  UniqueUnsubscribedUsersFromGroup group user
  deriving Show

-- Submission -> [Comment]
-- Comment -> Submission
CommentsOfSubmission
  submission SubmissionId
  comment    CommentId
  UniqueCommentsOfSubmissionPair submission comment
  UniqueCommentsOfSubmissionComment comment
  deriving Show

-- Submission -> User
UserOfSubmission
  submission SubmissionId
  user       UserId
  UniqueUserOfSubmission user submission
  deriving Show

-- Assignment -> User -> [Submission]
UserSubmissionOfAssignment
  submission SubmissionId
  assignment AssignmentId
  user       UserId
  UniqueUserSubmissionOfAssignmentTriplet submission assignment user
  deriving Show

-- Assignment -> User -> [Submission]
OpenedSubmission
  submission SubmissionId
  assignment AssignmentId
  user       UserId
  UniqueOpenedSubmissionTriplet submission assignment user
  deriving Show

-- TestCase -> TestScript
TestScriptOfTestCase
  testCase   TestCaseId
  testScript TestScriptId
  UniqueTestScriptOfTestCase testCase testScript
  UniqueTestScriptOfTestCaseTestCase testCase
  deriving Show

-- Evaluation -> Submission
SubmissionOfEvaluation
  submission SubmissionId
  evaluation EvaluationId
  UniqueSubmissionOfEvaluationPair submission evaluation
  UniqueSubmissionOfEvaluation evaluation
  deriving Show

-- Evaluation -> Score
ScoreOfEvaluation
  score      ScoreId
  evaluation EvaluationId
  UniqueScoreOfEvaluationPair score evaluation
  UniqueScoreOfEvaluation evaluation

|]

-- * Persist

type Persist = SqlPersistT (NoLoggingT (ResourceT IO))

-- * Helpers

entity f (Entity key value) = f key value

withEntity e f = entity f e

-- Forgets the result of a given computation
void :: Monad m => m a -> m ()
void = (>> return ())

-- Throws an error indicating this module as the source of the error
persistError function msg = error (concat ["Bead.Persistent.SQL.", function, ": ", msg])

getByUsername username =
  fmap (fromMaybe (persistError "getByUsername" $ "User is not found" ++ show username))
       (getBy (Domain.usernameCata (UniqueUsername . Text.pack) username))

-- Selects a user from the database with the given user, if the
-- user is not found runs the nothing computation, otherwise
-- the just computation with the user as a parameter.
withUser username nothing just =
  getBy (UniqueUsername $ Domain.usernameCata Text.pack username) >>= maybe nothing just

#ifdef TEST

-- * Test helpers

runSql :: Keyword (EncapsKeyword Persist) a -> IO (Either IOError a)
runSql = runSqlite ":memory:" . runKeyword encapsContext

initDB :: Persist ()
initDB = void $ runMigrationSilent migrateAll

dbStep :: Persist a -> Keyword (EncapsKeyword Persist) a
dbStep = step . key

dbInfo :: Persist a -> Keyword (EncapsKeyword Persist) a
dbInfo = info . key

#endif
