module Bead.View.UserActions where

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Logging as L
import           Bead.Domain.Entities
import           Bead.Domain.Relationships
import           Bead.View.Translation

import           Data.Map (Map)
import           Control.Monad (void)

-- | The user can preform the following actions on the user interface
data UserAction
  -- Navigation
  = Logout
  | LogMessage String
  | StatusMessage (Translation String)
  | ErrorMessage (Translation String)

  -- Profiling
  | ChangeUserDetails String TimeZoneName Language

  -- Group
  | CreateGroup CourseKey Group
  | SubscribeToGroup GroupKey
  | CreateGroupAdmin Username GroupKey
  | DeleteUsersFromGroup GroupKey [Username]
  | UnsubscribeFromCourse GroupKey -- Student wants to unsibscribe from a course which he is subscribed to

  -- Course
  | CreateCourse Course
  | CreateCourseAdmin Username CourseKey
  | DeleteUsersFromCourse CourseKey [Username]

  -- Test Script
  | CreateTestScript CourseKey TestScript
  | ModifyTestScript TestScriptKey TestScript

  -- Assignment
  | CreateGroupAssignment GroupKey Assignment TCCreation
  | CreateCourseAssignment CourseKey Assignment TCCreation
  | ModifyAssignment AssignmentKey Assignment TCModification

  -- Assessment
  | CreateGroupAssessment GroupKey Assessment
  | CreateCourseAssessment CourseKey Assessment

  -- Scores
  | SaveUserScore Username AssessmentKey Evaluation
  | SaveScoresOfGroupAssessment GroupKey Assessment (Map Username Evaluation)
  | SaveScoresOfCourseAssessment CourseKey Assessment (Map Username Evaluation)

  -- Submission
  | NewSubmission AssignmentKey Submission

  -- Evaluation
  | NewEvaluation SubmissionKey Evaluation
  | ModifyEvaluation EvaluationKey Evaluation

  -- Comment
  | SubmissionComment SubmissionKey Comment

  -- Administration
  | CreateUser User
  | UpdateUser User
  -- etc
  | NoUserAction
  deriving (Eq)

-- TODO: I18N
-- | UserStory correspondence to the given action
userStoryFor :: UserAction -> Story.UserStory ()
userStoryFor Logout             = Story.logout
userStoryFor (CreateUser u)     = Story.createUser u
userStoryFor (LogMessage m)     = Story.logErrorMessage m
userStoryFor (StatusMessage m)  = Story.putStatusMessage m
userStoryFor (ErrorMessage m)   = Story.putErrorMessage m
userStoryFor (CreateCourse c)   = Story.createCourse c >> return ()
userStoryFor (CreateGroup ck g) = Story.createGroup ck g >> return ()
userStoryFor (UpdateUser u)     = Story.updateUser u
userStoryFor (CreateCourseAdmin u c) = Story.createCourseAdmin u c
userStoryFor (CreateGroupAdmin u g)   = Story.createGroupAdmin u g
userStoryFor (SubscribeToGroup g)    = Story.subscribeToGroup g
userStoryFor (CreateGroupAssignment gk a tc)  = Story.createGroupAssignment gk a tc >> return ()
userStoryFor (CreateCourseAssignment ck a tc) = Story.createCourseAssignment ck a tc >> return ()
userStoryFor (CreateGroupAssessment gk a) = Story.createGroupAssessment gk a >> return ()
userStoryFor (CreateCourseAssessment ck a) = Story.createCourseAssessment ck a >> return ()
userStoryFor (ModifyAssignment ak a tm) = Story.modifyAssignment ak a tm
userStoryFor (SaveUserScore u ak evaluation) = void $ Story.saveUserScore u ak evaluation
userStoryFor (SaveScoresOfCourseAssessment ck a evaluations) = Story.saveScoresOfCourseAssessment ck a evaluations
userStoryFor (SaveScoresOfGroupAssessment gk a evaluations) = Story.saveScoresOfGroupAssessment gk a evaluations
userStoryFor (NewSubmission ak s)    = Story.submitSolution ak s >> return ()
userStoryFor (NewEvaluation sk e)    = Story.newEvaluation sk e
userStoryFor (ModifyEvaluation ek e) = Story.modifyEvaluation ek e
userStoryFor (SubmissionComment sk c) = Story.createComment sk c
userStoryFor (DeleteUsersFromCourse ck us) = Story.deleteUsersFromCourse ck us
userStoryFor (DeleteUsersFromGroup gk us) = Story.deleteUsersFromGroup gk us
userStoryFor (UnsubscribeFromCourse gk) = Story.unsubscribeFromCourse gk
userStoryFor (CreateTestScript ck s) = Story.saveTestScript ck s
userStoryFor (ModifyTestScript tsk s) = Story.modifyTestScript tsk s

-- Saves the email, fullname and timezone in the persistence layer
-- and set the user's timezone in the service context
userStoryFor (ChangeUserDetails n t l) =
  do Story.changeUserDetails n t l
     Story.setTimeZone t
     Story.putStatusMessage $ msg_UserActions_ChangedUserDetails "The user's settings has been changed."

userStoryFor NoUserAction = return ()
userStoryFor _            = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
