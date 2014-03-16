module Bead.View.UserActions where

import Bead.Domain.Entities
import Bead.Domain.Types
import Bead.Domain.Relationships
import Bead.Controller.ServiceContext (UserState(..))

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.Logging as L
import Bead.View.Snap.Translation

-- | The user can preform the following actions on the user interface
data UserAction
  -- Navigation
  = Logout
  | LogMessage String
  | StatusMessage (Translation String)

  -- Profiling
  | ChangeUserDetails String TimeZone Language

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
  deriving (Eq)

-- TODO: I18N
-- | UserStory correspondence to the given action
userStoryFor :: UserAction -> Story.UserStory ()
userStoryFor Logout             = Story.logout
userStoryFor (CreateUser u)     = Story.createUser u
userStoryFor (LogMessage m)     = Story.logErrorMessage m
userStoryFor (StatusMessage m)  = Story.putStatusMessage m
userStoryFor (CreateCourse c)   = Story.createCourse c >> return ()
userStoryFor (CreateGroup ck g) = Story.createGroup ck g >> return ()
userStoryFor (UpdateUser u)     = Story.updateUser u
userStoryFor (CreateCourseAdmin u c) = Story.createCourseAdmin u c
userStoryFor (CreateGroupAdmin u g)   = Story.createGroupAdmin u g
userStoryFor (SubscribeToGroup g)    = Story.subscribeToGroup g
userStoryFor (CreateGroupAssignment gk a tc)  = Story.createGroupAssignment gk a tc >> return ()
userStoryFor (CreateCourseAssignment ck a tc) = Story.createCourseAssignment ck a tc >> return ()
userStoryFor (ModifyAssignment ak a tm) = Story.modifyAssignment ak a tm
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
     Story.putStatusMessage $ Msg_UserActions_ChangedUserDetails "Settings of the user are changed."

userStoryFor _                      = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
