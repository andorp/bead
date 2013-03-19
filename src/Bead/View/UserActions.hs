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
  | SubscribeToGroup CourseKey GroupKey

  -- Cource
  | CreateCourse Course
  | DeleteCourse Encrypted

  -- Exercise
  | CreateExercise Exercise
  | DeleteExercise String

  -- Solution
  | SubmitSolution ExerciseKey String

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
userStoryFor (CreateExercise e) = Story.createExercise e >> return ()
userStoryFor (UpdateUser u)     = Story.updateUser u
userStoryFor (SubscribeToGroup c g) = Story.subscribeToGroup c g
userStoryFor _                      = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
