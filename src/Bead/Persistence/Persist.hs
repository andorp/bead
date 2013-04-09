module Bead.Persistence.Persist (
    Persist(..)
  , runPersist
  , userAssignmentKeys
  ) where

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Data.List (nub)
import Control.Applicative ((<$>))
import Control.Monad (mapM, liftM)
import Control.Exception (IOException)
import Control.Monad.Transaction.TIO

data Persist = Persist {
  -- User Persistence
    saveUser      :: User -> Password -> TIO ()
  , canUserLogin  :: Username -> Password -> TIO Bool
  , personalInfo  :: Username -> Password -> TIO (Role, String)
  , updatePwd     :: Username -> Password -> Password -> TIO ()
  , filterUsers   :: (User -> Bool) -> TIO [User]
  , loadUser      :: Username -> TIO User
  , updateUser    :: User -> TIO ()
  , doesUserExist :: Username -> TIO Bool
  , userDescription :: Username -> TIO UserDesc
  , administratedCourses :: Username -> TIO [(CourseKey, Course)]
  , administratedGroups  :: Username -> TIO [(GroupKey, Group)]

  -- Course Persistence
  , saveCourse        :: Course -> TIO CourseKey
  , courseKeys        :: TIO [CourseKey]
  , filterCourses     :: (CourseKey -> Course -> Bool) -> TIO [(CourseKey, Course)]
  , loadCourse        :: CourseKey -> TIO Course
  , groupKeysOfCourse :: CourseKey -> TIO [GroupKey]
  , isUserInCourse    :: Username -> CourseKey -> TIO Bool
  , userCourses       :: Username -> TIO [CourseKey]
  , createCourseAdmin :: Username -> CourseKey -> TIO ()

  -- Group Persistence
  , saveGroup     :: CourseKey -> Group -> TIO GroupKey
  , loadGroup     :: GroupKey -> TIO Group
  , courseOfGroup :: GroupKey -> TIO CourseKey
  , filterGroups  :: (GroupKey -> Group -> Bool) -> TIO [(GroupKey, Group)]
  , isUserInGroup :: Username -> GroupKey -> TIO Bool
  , userGroups    :: Username -> TIO [GroupKey]
  , subscribe     :: Username -> CourseKey -> GroupKey -> TIO ()
  , groupAdmins   :: GroupKey -> TIO [Username]
  , createGroupProfessor :: Username -> GroupKey -> TIO ()

  -- Assignment Persistence
  , filterAssignment  :: (AssignmentKey -> Assignment -> Bool) -> TIO [(AssignmentKey, Assignment)]
  , assignmentKeys    :: TIO [AssignmentKey]
  , saveAssignment    :: Assignment -> TIO AssignmentKey
  , loadAssignment    :: AssignmentKey -> TIO Assignment
  , courseAssignments :: CourseKey -> TIO [AssignmentKey]
  , groupAssignments  :: GroupKey -> TIO [AssignmentKey]
  , saveCourseAssignment :: CourseKey -> Assignment -> TIO AssignmentKey
  , saveGroupAssignment  :: GroupKey  -> Assignment -> TIO AssignmentKey
  , courseOfAssignment   :: AssignmentKey -> TIO (Maybe CourseKey)
  , groupOfAssignment    :: AssignmentKey -> TIO (Maybe GroupKey)

  -- Submission
  , saveSubmission :: AssignmentKey -> Username -> Submission -> TIO SubmissionKey
  , loadSubmission :: SubmissionKey -> TIO Submission

  -- Persistence initialization
  , isPersistenceSetUp :: IO Bool
  , initPersistence    :: IO ()
  }

-- * Combined Persistence Tasks

userAssignmentKeys :: Persist -> Username -> TIO [AssignmentKey]
userAssignmentKeys p u = do
  gs <- userGroups p u
  cs <- userCourses p u
  asg <- concat <$> (mapM (groupAssignments p)  (nub gs))
  asc <- concat <$> (mapM (courseAssignments p) (nub cs))
  return . nub $ (asg ++ asc)

-- * Runner Tools
  
reason :: Either IOException a -> (Erroneous a)
reason (Left e)  = Left . show $ e
reason (Right x) = Right x

runPersist :: TIO a -> IO (Erroneous a)
runPersist = liftM reason . atomically
