module Test.Persistence.Persist where

import Bead.Persistence.Persist
import Bead.Domain.Relationships
import Control.Monad.Transaction.TIO (TIO)

type Assert = String -> TIO ()

testDecoratedPersist :: Assert -> Persist -> Persist
testDecoratedPersist assert q = Persist {
    saveUser     = saveUser q
  , canUserLogin = canUserLogin q
  , personalInfo = personalInfo q
  , updatePwd    = updatePwd q
  , filterUsers  = filterUsers q
  , loadUser     = loadUser q
  , updateUser   = updateUser q
  , doesUserExist = doesUserExist q
  , userDescription = userDescription q

  , administratedCourses = \u -> do
      cs <- administratedCourses q u
      return cs

  , administratedGroups = \u -> do
      gs <- administratedGroups q u
      return gs

  , saveCourse = \c -> do
      ck <- saveCourse q c
      return ck

  , courseKeys = do
      ck <- courseKeys q
      return ck

  , filterCourses = \f -> do
      cs <- filterCourses q f
      return cs

  , loadCourse = \ck -> do
      c <- loadCourse q ck
      return c

  , groupKeysOfCourse = \ck -> do
      gs <- groupKeysOfCourse q ck
      return gs

  , isUserInCourse = isUserInCourse q
  , userCourses = \u -> do
      cs <- userCourses q u
      return cs

  , createCourseAdmin = createCourseAdmin q

  , saveGroup = \ck g -> do
      gk <- saveGroup q ck g
      return gk

  , loadGroup = loadGroup q
  , courseOfGroup = \gk -> do
      ck <- courseOfGroup q gk
      return ck

  , filterGroups = \f -> do
      gs <- filterGroups q f
      return gs

  , isUserInGroup = isUserInGroup q

  , userGroups = \u -> do
      gs <- userGroups q u
      return gs

  , subscribe = subscribe q
  , groupAdmins = \gk -> do
      us <- groupAdmins q gk
      return us

  , createGroupProfessor = createGroupProfessor q

  , filterAssignment = filterAssignment q
  , assignmentKeys = do
      as <- assignmentKeys q
      return as

  , saveAssignment = \a -> do
      ak <- saveAssignment q a
      return ak

  , loadAssignment = loadAssignment q
  , courseAssignments = \ck -> do
      as <- courseAssignments q ck
      return as

  , groupAssignments = \gk -> do
      as <- groupAssignments q gk
      return as

  , saveCourseAssignment = \ck a -> do
     k <- saveCourseAssignment q ck a
     return k

  , saveGroupAssignment = \gk a -> do
      ak <- saveGroupAssignment q gk a
      return ak

  , courseOfAssignment = \ak -> do
      ck <- courseOfAssignment q ak
      return ck

  , groupOfAssignment = \ak -> do
      gk <- groupOfAssignment q ak
      return gk

  -- Submission
  , saveSubmission = \ak u s -> do
      sk <- saveSubmission q ak u s
      return sk

  , loadSubmission = \sk -> do
      s <- loadSubmission q sk
      return s

  , isPersistenceSetUp = isPersistenceSetUp q
  , initPersistence = initPersistence q

  }

-- TODO: Keys must be constructed from one piece.
-- E.g Passed: CourseKey "ck3820", Failed: CourseKey "data\cg2312"
testCourseKey :: Assert -> CourseKey -> TIO ()
testCourseKey = undefined

testGroupKey :: Assert -> GroupKey -> TIO ()
testGroupKey = undefined

testAssignmentKey :: Assert -> AssignmentKey -> TIO ()
testAssignmentKey = undefined

testSubmissionKey :: Assert -> SubmissionKey -> TIO ()
testSubmissionKey = undefined
