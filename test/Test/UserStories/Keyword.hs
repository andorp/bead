module Test.UserStories.Keyword where

import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Controller.Pages

import Test.KeyCheck.Keyword

{-
class (Monad k) => UserStoryKeyword k where
  login            :: Username -> Password -> Keyword E k ()
  logout           :: Username -> Keyword E k ()
  submitSolution   :: Stored ExerciseKey Exercise -> Solution -> Keyword E k SolutionKey
  changePage       :: Page -> Keyword E k ()
  changePassword   :: Password -> Password -> Password -> Keyword E k ()
  createUser       :: User -> Password -> Keyword E k ()
  deleteUser       :: Username -> Keyword E k ()
  createCourse     :: Course -> Keyword E k CourseKey
  deleteCourse     :: CourseKey -> Keyword E k ()
  updateCourse     :: CourseKey -> Course -> Keyword E k ()
  createGroup      :: CourseKey -> Group -> Keyword E k GroupKey
  registerInAGroup :: User -> GroupKey -> Keyword E k ()
  deleteGroup      :: GroupKey -> Keyword E k ()
  updateGroup      :: GroupKey -> Group -> Keyword E k ()
  createExercise   :: Exercise -> Keyword E k ExerciseKey
  updateExercise   :: ExerciseKey -> Exercise -> Keyword E k ()
  deleteExercise   :: ExerciseKey -> Keyword E k ()
  errorPage        :: String -> Keyword E k ()
  authorize        :: Permission -> PermissionObject -> Keyword E k ()
  isAuthorized     :: Permission -> PermissionObject -> Keyword E k Bool
  noOperation      :: Keyword E k ()
  logErrorMessage  :: String -> Keyword E k ()
  renderPageData   :: Page -> Keyword E k ()
  loadUserData     :: Username -> Password -> Page -> Keyword E k ()
-}
