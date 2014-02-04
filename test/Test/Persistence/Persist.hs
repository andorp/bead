module Test.Persistence.Persist where

import System.FilePath (splitPath)
import Bead.Persistence.Persist
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Control.Monad (when)
import Control.Monad.Transaction.TIO (TIO)
import Data.Maybe

type Assert = String -> TIO ()

testDecoratedPersist :: Assert -> Persist -> Persist
testDecoratedPersist a q = Persist {
    saveUser     = saveUser q
  , personalInfo = personalInfo q
  , filterUsers  = filterUsers q
  , loadUser     = loadUser q
  , updateUser   = updateUser q
  , doesUserExist = doesUserExist q
  , userDescription = userDescription q
  , saveUserReg = saveUserReg q
  , loadUserReg = loadUserReg q
  , userSubmissions = \u ak -> do
      ks <- userSubmissions q u ak
      mapM (testSubmissionKey a) ks
      return ks

  , administratedCourses = \u -> do
      cs <- administratedCourses q u
      mapM (testCourseKey a . fst) cs
      return cs

  , administratedGroups = \u -> do
      gs <- administratedGroups q u
      mapM (testGroupKey a . fst) gs
      return gs

  , saveCourse = \c -> do
      ck <- saveCourse q c
      testCourseKey a ck
      return ck

  , courseKeys = do
      ck <- courseKeys q
      return ck

  , filterCourses = \f -> do
      cs <- filterCourses q f
      mapM (testCourseKey a . fst) cs
      return cs

  , loadCourse = \ck -> do
      testCourseKey a ck
      c <- loadCourse q ck
      return c

  , groupKeysOfCourse = \ck -> do
      testCourseKey a ck
      gs <- groupKeysOfCourse q ck
      mapM (testGroupKey a) gs
      return gs

  , subscribedToCourse = \ck -> do
      testCourseKey a ck
      subscribedToCourse q ck

  , unsubscribedFromCourse = \ck -> do
      testCourseKey a ck
      unsubscribedFromCourse q ck

  , isUserInCourse = isUserInCourse q
  , userCourses = \u -> do
      cs <- userCourses q u
      mapM (testCourseKey a) cs
      return cs

  , createCourseAdmin = createCourseAdmin q

  , courseAdmins = \ck -> do
      testCourseKey a ck
      us <- courseAdmins q ck
      return us


  , saveGroup = \ck g -> do
      testCourseKey a ck
      gk <- saveGroup q ck g
      testGroupKey a gk
      return gk

  , loadGroup = loadGroup q
  , courseOfGroup = \gk -> do
      testGroupKey a gk
      ck <- courseOfGroup q gk
      testCourseKey a ck
      return ck

  , filterGroups = \f -> do
      gs <- filterGroups q f
      mapM (testGroupKey a . fst) gs
      return gs

  , isUserInGroup = isUserInGroup q

  , userGroups = \u -> do
      gs <- userGroups q u
      mapM (testGroupKey a) gs
      return gs

  , subscribe = subscribe q
  , unsubscribe = unsubscribe q
  , groupAdmins = \gk -> do
      testGroupKey a gk
      us <- groupAdmins q gk
      return us

  , createGroupAdmin = createGroupAdmin q

  , subscribedToGroup = \gk -> do
      testGroupKey a gk
      subscribedToGroup q gk

  , unsubscribedFromGroup = \gk -> do
      testGroupKey a gk
      unsubscribedFromGroup q gk


  , filterAssignment = filterAssignment q
  , assignmentKeys = do
      as <- assignmentKeys q
      mapM (testAssignmentKey a) as
      return as

  , saveAssignment = \s -> do
      ak <- saveAssignment q s
      testAssignmentKey a ak
      return ak

  , loadAssignment = \ak -> do
      testAssignmentKey a ak
      loadAssignment q ak

  , modifyAssignment = \ak s -> do
      testAssignmentKey a ak
      modifyAssignment q ak s

  , courseAssignments = \ck -> do
      testCourseKey a ck
      as <- courseAssignments q ck
      mapM (testAssignmentKey a) as
      return as

  , groupAssignments = \gk -> do
      testGroupKey a gk
      as <- groupAssignments q gk
      return as

  , saveCourseAssignment = \ck ca -> do
     testCourseKey a ck
     k <- saveCourseAssignment q ck ca
     testAssignmentKey a k
     return k

  , saveGroupAssignment = \gk ga -> do
      ak <- saveGroupAssignment q gk ga
      testAssignmentKey a ak
      return ak

  , courseOfAssignment = \ak -> do
      ck <- courseOfAssignment q ak
      case ck of
        Nothing -> return ()
        Just ck' -> testCourseKey a ck'
      return ck

  , groupOfAssignment = \ak -> do
      gk <- groupOfAssignment q ak
      case gk of
        Nothing -> return ()
        Just gk' -> testGroupKey a gk'
      return gk

  , submissionsForAssignment = \ak -> do
      testAssignmentKey a ak
      ks <- submissionsForAssignment q ak
      mapM (testSubmissionKey a) ks
      return ks

  , assignmentCreatedTime = \ak -> do
      testAssignmentKey a ak
      assignmentCreatedTime q ak

  -- Submission
  , saveSubmission = \ak u s -> do
      testAssignmentKey a ak
      sk <- saveSubmission q ak u s
      testSubmissionKey a sk
      return sk

  , loadSubmission = \sk -> do
      testSubmissionKey a sk
      s <- loadSubmission q sk
      return s

  , assignmentOfSubmission = \sk -> do
      testSubmissionKey a sk
      ak <- assignmentOfSubmission q sk
      testAssignmentKey a ak
      return ak

  , usernameOfSubmission = \sk -> do
      testSubmissionKey a sk
      u <- usernameOfSubmission q sk
      testUsername a u
      return u

  , commentsOfSubmission = \sk -> do
      testSubmissionKey a sk
      cs <- commentsOfSubmission q sk
      mapM (testCommentKey a) cs
      return cs

  , filterSubmissions = \f -> do
      ks <- filterSubmissions q f
      mapM (testSubmissionKey a . fst) ks
      return ks

  , removeFromOpened = \ak u sk -> do
      testAssignmentKey a ak
      testSubmissionKey a sk
      removeFromOpened q ak u sk

  , openedSubmissions = openedSubmissions q

  , usersOpenedSubmissions = \ak u -> do
      testAssignmentKey a ak
      usersOpenedSubmissions q ak u

  , evaluationOfSubmission = \sk -> do
      testSubmissionKey a sk
      mek <- evaluationOfSubmission q sk
      when (isJust mek) $ testEvaluationKey a (fromJust mek)
      return mek

  , saveEvaluation = \sk c -> do
      testSubmissionKey a sk
      ek <- saveEvaluation q sk c
      testEvaluationKey a ek
      return ek

  , loadEvaluation = \ek -> do
      testEvaluationKey a ek
      e <- loadEvaluation q ek
      return e

  , modifyEvaluation = \ek e -> do
      testEvaluationKey a ek
      modifyEvaluation q ek e

  , submissionOfEvaluation = \ek -> do
      testEvaluationKey a ek
      sk <- submissionOfEvaluation q ek
      testSubmissionKey a sk
      return sk

  , lastSubmission = \ak u -> do
      testAssignmentKey a ak
      sk <- lastSubmission q ak u
      when (isJust sk) . testSubmissionKey a . fromJust $ sk
      return sk

  , saveComment = \sk c -> do
      testSubmissionKey a sk
      ck <- saveComment q sk c
      testCommentKey a ck
      return ck

  , loadComment = \ck -> do
      testCommentKey a ck
      loadComment q ck

  , submissionOfComment = \ck -> do
      testCommentKey a ck
      sk <- submissionOfComment q ck
      testSubmissionKey a sk
      return sk

  , isPersistenceSetUp = isPersistenceSetUp q
  , initPersistence = initPersistence q

  }

-- TODO: Keys must be constructed from one piece.
-- E.g Passed: CourseKey "ck3820", Failed: CourseKey "data\cg2312"
testCourseKey :: Assert -> CourseKey -> TIO ()
testCourseKey a (CourseKey ck) = checkKeyString ck a $ "CourseKey was invalid: " ++ ck

testUsername :: Assert -> Username -> TIO ()
testUsername a (Username u) = checkKeyString u a $ "Username was invalid: " ++ u

testGroupKey :: Assert -> GroupKey -> TIO ()
testGroupKey a (GroupKey gk) = checkKeyString gk a $ "GroupKey was invalid: " ++ gk

testAssignmentKey :: Assert -> AssignmentKey -> TIO ()
testAssignmentKey a (AssignmentKey ak) = checkKeyString ak a $ "AssignmentKey was invalid: " ++ ak

testSubmissionKey :: Assert -> SubmissionKey -> TIO ()
testSubmissionKey a (SubmissionKey sk) = checkKeyString sk a $ "SubmissionKey was invalid: " ++ sk

testEvaluationKey :: Assert -> EvaluationKey -> TIO ()
testEvaluationKey a (EvaluationKey ek) = checkKeyString ek a $ "EvaluationKey was invalid: " ++ ek

testCommentKey :: Assert -> CommentKey -> TIO ()
testCommentKey a (CommentKey ck) = checkKeyString ck a $ "CommentKey was invalid: " ++ ck

checkKeyString :: String -> Assert -> String -> TIO ()
checkKeyString str assert msg = when (length (splitPath str) /= 1) $ assert msg
