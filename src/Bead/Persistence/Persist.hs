module Bead.Persistence.Persist where

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Relationships

data Persist = Persist {
  -- User Persistence
    saveUser      :: User -> Password -> IO (Erroneous ())
  , doesUserExist :: Username -> Password -> IO (Erroneous Bool)
  , personalInfo  :: Username -> Password -> IO (Erroneous (Role, String))
  , updatePwd     :: Username -> Password -> Password -> IO (Erroneous ())

  -- Course Persistence
  , saveCourse  :: Course -> IO (Erroneous CourseKey)
  , courseKeys :: IO (Erroneous [CourseKey])
  , filterCourses :: (CourseKey -> Course -> Bool) -> IO (Erroneous [(CourseKey, Course)])
  , loadCourse :: CourseKey -> IO (Erroneous Course)
  , groupKeysOfCourse :: CourseKey -> IO (Erroneous [GroupKey])
  
  -- Group Persistence
  , saveGroup   :: CourseKey -> Group -> IO (Erroneous GroupKey)
  , loadGroup   :: GroupKey -> IO (Erroneous Group)

  -- Exersice Persistence
  , filterExercises :: (ExerciseKey -> Exercise -> Bool) -> IO (Erroneous [(ExerciseKey, Exercise)])
  , exerciseKeys :: IO (Erroneous [ExerciseKey])
  , saveExercise :: Exercise -> IO (Erroneous ExerciseKey)
  , loadExercise    :: ExerciseKey -> IO (Erroneous Exercise)
  
  -- Persistence initialization
  , isPersistenceSetUp :: IO Bool
  , initPersistence    :: IO ()
  }

mockPersist :: Persist
mockPersist = Persist {

    saveUser      = \_ _  -> return $ Right ()
  , doesUserExist = \_ _ -> return $ Right $ True
  , personalInfo  = \_ _ -> return $ Right (Admin, "Ulien Mokk")
  , updatePwd     = \_ _ _ -> return $ Right ()

  , saveCourse = \_ -> return $ Right $ CourseKey "course_key"
  , courseKeys = return $ Right $ []
  , filterCourses = \_ -> return $ Right $ []
  , loadCourse = \_ -> return $ Right (Course {})
  , groupKeysOfCourse = \_ -> return $ Right []
  
  , saveGroup = \_ _ -> return $ Right $ GroupKey "group_key"
  , loadGroup = \_ -> return $ Right $ Group {}
  
  , saveExercise = \_ -> return $ Right $ ExerciseKey "exercise_key"
  , loadExercise = \_ -> return $ Right $ Exercise "exercise"
  , filterExercises = \_ -> return $ Right $ []
  , exerciseKeys = return $ Right $ []

  , isPersistenceSetUp = return True
  , initPersistence    = return ()
  }

