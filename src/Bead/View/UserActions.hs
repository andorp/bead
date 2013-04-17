module Bead.View.UserActions where

import Bead.Domain.Entities
import Bead.Domain.Types
import Bead.Domain.Relationships
import Bead.Controller.ServiceContext (UserState(..))

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.Logging as L

type Action = UserState -> IO UserAction

-- | The user can preform the following actions on the user interface
data UserAction
  -- Navigation
  = Logout
  | SelectCourse Encrypted
  | SelectGroup Encrypted
  | Login Username Password
  | ChangePage P.Page
  | LogMessage String

  -- Profiling
  | Profile
  | ChangePwd Password Password Password

  -- Group
  | CreateGroup CourseKey Group
  | DeleteGroup Encrypted
  | SubscribeToGroup GroupKey
  | CreateProfessor Username GroupKey

  -- Course
  | CreateCourse Course
  | DeleteCourse Encrypted
  | CreateCourseAdmin Username CourseKey

  -- Assignment
  | CreateGroupAssignment GroupKey Assignment
  | CreateCourseAssignment CourseKey Assignment

  -- Submission
  | NewSubmission AssignmentKey Submission

  -- Evaulation
  | NewEvaulation SubmissionKey Evaulation
  | ModifyEvaulation EvaulationKey Evaulation

  -- Comment
  | SubmissionComment SubmissionKey Comment
  
  -- Administration
  | CreateUser User Password
  | UpdateUser User
  -- etc
  deriving (Eq)

-- | UserStory correspondence to the given action
userStoryFor :: UserAction -> Story.UserStory ()
userStoryFor Logout             = Story.logout
userStoryFor Profile            = Story.changePage P.Profile
userStoryFor (ChangePage p)     = Story.changePage p
userStoryFor (ChangePwd o n n') = Story.changePassword o n n'
userStoryFor (CreateUser u p)   = Story.createUser u p
userStoryFor (LogMessage m)     = Story.logErrorMessage m
userStoryFor (CreateCourse c)   = Story.createCourse c >> return ()
userStoryFor (CreateGroup ck g) = Story.createGroup ck g >> return ()
userStoryFor (UpdateUser u)     = Story.updateUser u
userStoryFor (CreateCourseAdmin u c) = Story.createCourseAdmin u c
userStoryFor (CreateProfessor u g)   = Story.createGroupProfessor u g
userStoryFor (SubscribeToGroup g)    = Story.subscribeToGroup g
userStoryFor (CreateGroupAssignment gk a)  = Story.createGroupAssignment gk a >> return ()
userStoryFor (CreateCourseAssignment ck a) = Story.createCourseAssignment ck a >> return ()
userStoryFor (NewSubmission ak s)    = Story.submitSolution ak s >> return ()
userStoryFor (NewEvaulation sk e)    = Story.newEvaulation sk e
userStoryFor (SubmissionComment sk c) = Story.createComment sk c
userStoryFor _                      = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
