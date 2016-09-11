{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Notification where

import Data.Data
import Data.Text
import Data.Time (UTCTime)
import Data.Typeable

import Bead.Domain.Relationships

-- The notifications can come from different sources
data NotificationType
  = Comment    CommentKey
  | Evaluation EvaluationKey
  | Assignment AssignmentKey
  | Assessment AssessmentKey
  | System
  deriving (Eq, Show, Read, Data, Typeable)

notificationType
  comment
  evaluation
  assignment
  assessment
  system
  n = case n of
    Comment    ck -> comment ck
    Evaluation ek -> evaluation ek
    Assignment ak -> assignment ak
    Assessment ak -> assessment ak
    System        -> system

data NotificationState = New | Seen
  deriving (Eq, Show)

data NotificationProcessed = Unprocessed | Processed
  deriving (Eq, Show)

data NotificationReference
  = NRefComment AssignmentKey SubmissionKey CommentKey
  | NRefSubmissionEvaluation AssignmentKey SubmissionKey EvaluationKey
  | NRefScoreEvaluation ScoreKey EvaluationKey
  | NRefAssignment AssignmentKey
  | NRefAssessment AssessmentKey
  | NRefSystem
  deriving (Eq, Show)

notificationReference
  comment
  submission
  score
  assignment
  assessment
  system
  r = case r of
    NRefComment ak sk ck -> comment ak sk ck
    NRefSubmissionEvaluation ak sk ek -> submission ak sk ek
    NRefScoreEvaluation sk ek -> score sk ek
    NRefAssignment ak -> assignment ak
    NRefAssessment ak -> assessment ak
    NRefSystem -> system

-- Events that might trigger notifications in the system.  This ADT
-- introduces another level of indirection in message resolution,
-- but that is needed for implementing a flexible way of
-- internationalization.
data NotificationEvent
  = NE_CourseAdminCreated String
    -- course name
  | NE_CourseAdminAssigned String String
    -- course name, assignee
  | NE_TestScriptCreated String String
    -- creator, course name
  | NE_TestScriptUpdated String String String
    -- editor, test script name, course name
  | NE_RemovedFromGroup String String
    -- group name, deletor
  | NE_GroupAdminCreated String String String
    -- course name, creator, group name
  | NE_GroupAssigned String String String String
    -- group name, course name, assignor, assignee
  | NE_GroupCreated String String String
    -- course name, creator, group name
  | NE_GroupAssignmentCreated String String String String
    -- creator, group name, course name, assignment name
  | NE_CourseAssignmentCreated String String String
    -- creator, course name, assignment name
  | NE_GroupAssessmentCreated String String String String
    -- creator, group name, course name, assessment title
  | NE_CourseAssessmentCreated String String String
    -- creator, course name, assessment title
  | NE_AssessmentUpdated String String
    -- editor, assessment title
  | NE_AssignmentUpdated String String
    -- editor, assignment name
  | NE_EvaluationCreated String String
    -- evaluator, submission id
  | NE_AssessmentEvaluationUpdated String String
    -- editor, assessment id
  | NE_AssignmentEvaluationUpdated String String
    -- editor, submission id
  | NE_CommentCreated String String String
    -- commenter, submission id, body
  deriving (Eq, Show, Read, Data, Typeable)

-- The notification is rendered for the user on some informational
-- page or send via email.
data Notification = Notification {
    notifEvent   :: NotificationEvent
  , notifDate    :: UTCTime
  , notifType    :: NotificationType
  } deriving (Eq, Show)

notification f (Notification event date typ) = f event date typ
