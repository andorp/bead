module Bead.Persistence.Persist where

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Relationships

data Persist = Persist {
  -- User Persistence
    saveUser      :: User -> Password -> IO (Erroneous ())
  , canUserLogin  :: Username -> Password -> IO (Erroneous Bool)
  , personalInfo  :: Username -> Password -> IO (Erroneous (Role, String))
  , updatePwd     :: Username -> Password -> Password -> IO (Erroneous ())
  , filterUsers   :: (User -> Bool) -> IO (Erroneous [User])
  , loadUser      :: Username -> IO (Erroneous User)
  , updateUser    :: User -> IO (Erroneous ())
  , doesUserExist :: Username -> IO (Erroneous Bool)
  , administratedCourses :: Username -> IO (Erroneous [(CourseKey, Course)])
  , administratedGroups  :: Username -> IO (Erroneous [(GroupKey, Group)])

  -- Course Persistence
  , saveCourse  :: Course -> IO (Erroneous CourseKey)
  , courseKeys :: IO (Erroneous [CourseKey])
  , filterCourses :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
  , loadCourse :: CourseKey -> IO (Erroneous Course)
  , groupKeysOfCourse :: CourseKey -> IO (Erroneous [GroupKey])
  , isUserInCourse :: Username -> CourseKey -> IO (Erroneous Bool)
  , createCourseAdmin :: Username -> CourseKey -> IO (Erroneous ())
  
  -- Group Persistence
  , saveGroup   :: CourseKey -> Group -> IO (Erroneous GroupKey)
  , loadGroup   :: GroupKey -> IO (Erroneous Group)
  , isUserInGroup :: Username -> GroupKey -> IO (Erroneous Bool)
  , subscribe :: Username -> CourseKey -> GroupKey -> IO (Erroneous ())
  , createGroupProfessor :: Username -> GroupKey -> IO (Erroneous ())

  -- Assignment Persistence
  , filterExercises :: (AssignmentKey -> Assignment -> Bool) -> IO (Erroneous [(AssignmentKey, Assignment)])
  , exerciseKeys :: IO (Erroneous [AssignmentKey])
  , saveExercise :: Assignment -> IO (Erroneous AssignmentKey)
  , loadExercise :: AssignmentKey -> IO (Erroneous Assignment)
  , saveCourseAssignment :: CourseKey -> Assignment -> IO (Erroneous AssignmentKey)
  , saveGroupAssignment  :: GroupKey  -> Assignment -> IO (Erroneous AssignmentKey)
  , courseAssignments :: CourseKey -> IO (Erroneous [AssignmentKey])
  , groupAssignments :: GroupKey -> IO (Erroneous [AssignmentKey])
  
  -- Persistence initialization
  , isPersistenceSetUp :: IO Bool
  , initPersistence    :: IO ()
  }
