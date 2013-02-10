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
  
  -- Group Persistence
  , saveGroup   :: CourseKey -> Group -> IO (Erroneous GroupKey)

  -- Exersice Persistence
  , saveExercise :: Exercise -> IO (Erroneous ExerciseKey)
  
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
  
  , saveGroup = \_ _ -> return $ Right $ GroupKey (CourseKey "course_key") "group_key"
  
  , saveExercise = \_ -> return $ Right $ ExerciseKey "exercise_key"

  , isPersistenceSetUp = return True
  , initPersistence    = return ()
  }

