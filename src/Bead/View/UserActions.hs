module Bead.View.UserActions where

import Bead.Domain.Entities
import Bead.Domain.Types
import Bead.Domain.Relationships
import Bead.Controller.ServiceContext (UserState(..))

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.Logging as L

-- | The user can preform the following actions on the user interface
data UserAction
  -- Navigation
  = Logout
  | LogMessage String
  | StatusMessage String

  -- Profiling
  | ChangeUserDetails String TimeZone

  -- Group
  | CreateGroup CourseKey Group
  | SubscribeToGroup GroupKey
  | CreateProfessor Username GroupKey

  -- Course
  | CreateCourse Course
  | CreateCourseAdmin Username CourseKey

  -- Assignment
  | CreateGroupAssignment GroupKey Assignment
  | CreateCourseAssignment CourseKey Assignment
  | ModifyAssignment AssignmentKey Assignment

  -- Submission
  | NewSubmission AssignmentKey Submission

  -- Evaulation
  | NewEvaulation SubmissionKey Evaulation
  | ModifyEvaulation EvaulationKey Evaulation

  -- Comment
  | SubmissionComment SubmissionKey Comment

  -- Administration
  | CreateUser User
  | UpdateUser User
  -- etc
  deriving (Eq)

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
userStoryFor (CreateProfessor u g)   = Story.createGroupProfessor u g
userStoryFor (SubscribeToGroup g)    = Story.subscribeToGroup g
userStoryFor (CreateGroupAssignment gk a)  = Story.createGroupAssignment gk a >> return ()
userStoryFor (CreateCourseAssignment ck a) = Story.createCourseAssignment ck a >> return ()
userStoryFor (ModifyAssignment ak a) = Story.modifyAssignment ak a
userStoryFor (NewSubmission ak s)    = Story.submitSolution ak s >> return ()
userStoryFor (NewEvaulation sk e)    = Story.newEvaulation sk e
userStoryFor (ModifyEvaulation ek e) = Story.modifyEvaulation ek e
userStoryFor (SubmissionComment sk c) = Story.createComment sk c

-- Saves the email, fullname and timezone in the persistence layer
-- and set the user's timezone in the service context
userStoryFor (ChangeUserDetails n t) =
  do Story.changeUserDetails n t
     Story.setTimeZone t

userStoryFor _                      = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
