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
  | CreateGroup String
  | DeleteGroup Encrypted

  -- Cource
  | CreateCourse Course
  | DeleteCourse Encrypted

  -- Exercise
  | CreateExercise String
  | DeleteExercise String

  -- Solution
  | SubmitSolution ExerciseKey String

  -- Administration
  | CreateUser User Password
  -- etc
  deriving (Eq)

-- | UserStory correspondence to UserState and the given action
userStoryFor :: UserState -> UserAction -> Story.UserStory ()
userStoryFor state Logout             = Story.logout (user state)
userStoryFor state Profile            = Story.changePage P.Profile
userStoryFor state (ChangePage p)     = Story.changePage p
userStoryFor state (ChangePwd o n n') = Story.changePassword o n n'
userStoryFor state (CreateUser u p)   = Story.createUser u p
userStoryFor state (LogMessage m)     = Story.logErrorMessage m
userStoryFor state (CreateCourse c)   = Story.createCourse c >> return ()
userStoryFor _     _                  = Story.logMessage L.DEBUG "No story was selected"
-- etc ...
