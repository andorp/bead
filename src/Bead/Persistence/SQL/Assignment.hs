{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.Assignment where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Time hiding (TimeZone)

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON

#ifdef TEST
import qualified Data.Set as Set

import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.Group

import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, shrink)
import           Test.Tasty.Encaps
#endif

-- * Assignment

toDomainAssignmentValue ent = Domain.Assignment
  (Text.unpack $ assignmentName ent)
  (Text.unpack $ assignmentDescription ent)
  (decodeAssignmentType $ assignmentType ent)
  (assignmentStart ent)
  (assignmentEnd ent)
  (decodeEvalConfig $ assignmentEvalConfig ent)

fromDomainAssignmentValue createdTime = Domain.assignmentCata
  $ \name desc type_ start end cfg -> Assignment
      (Text.pack name)
      (Text.pack desc)
      (encodeAssignmentType type_)
      start
      end
      createdTime
      (encodeEvalConfig cfg)

-- Lists all the assignments in the database
assignmentKeys :: Persist [Domain.AssignmentKey]
assignmentKeys = map toDomainKey <$> selectAssignmentKeys
  where
    selectAssignmentKeys :: Persist [Key Assignment]
    selectAssignmentKeys = selectKeysList [] []

-- Save the assignment into the database
saveAssignment :: Domain.Assignment -> Persist Domain.AssignmentKey
saveAssignment assignment = do
  now <- liftIO $ getCurrentTime
  key <- insert (fromDomainAssignmentValue now assignment)
  return $! toDomainKey key

-- Load the assignment from the database
loadAssignment :: Domain.AssignmentKey -> Persist Domain.Assignment
loadAssignment key = do
  mAsg <- get (toEntityKey key)
  return $!
    maybe (persistError "loadAssignment" $ "No assignment is found. " ++ show key)
          toDomainAssignmentValue
          mAsg

-- Modify the assignment in the database for the given key
modifyAssignment :: Domain.AssignmentKey -> Domain.Assignment -> Persist ()
modifyAssignment key assignment = do
  update (toEntityKey key) $ Domain.withAssignment assignment
    $ \name desc type_ start end cfg ->
        [ AssignmentName        =. Text.pack name
        , AssignmentDescription =. Text.pack desc
        , AssignmentType        =. encodeAssignmentType type_
        , AssignmentStart       =. start
        , AssignmentEnd         =. end
        , AssignmentEvalConfig  =. encodeEvalConfig cfg
        ]

-- Lists all the assignment that are created for the given course
courseAssignments :: Domain.CourseKey -> Persist [Domain.AssignmentKey]
courseAssignments courseKey = do
  assignments <- selectList [AssignmentsOfCourseCourse ==. toEntityKey courseKey] []
  return $! map (toDomainKey . assignmentsOfCourseAssignment . entityVal) assignments

-- Lists all the assignment that are created for the given group
groupAssignments :: Domain.GroupKey -> Persist [Domain.AssignmentKey]
groupAssignments groupKey = do
  assignments <- selectList [AssignmentsOfGroupGroup ==. toEntityKey groupKey] []
  return $! map (toDomainKey . assignmentsOfGroupAssignment . entityVal) assignments

-- Save the assignment for the given course
saveCourseAssignment :: Domain.CourseKey -> Domain.Assignment -> Persist Domain.AssignmentKey
saveCourseAssignment courseKey assignment = do
  now <- liftIO $ getCurrentTime
  key <- insert (fromDomainAssignmentValue now assignment)
  insertUnique (AssignmentsOfCourse (toEntityKey courseKey) key)
  return $! toDomainKey key

-- Save the assignment for the given group
saveGroupAssignment :: Domain.GroupKey  -> Domain.Assignment -> Persist Domain.AssignmentKey
saveGroupAssignment groupKey assignment = do
  now <- liftIO $ getCurrentTime
  key <- insert (fromDomainAssignmentValue now assignment)
  insertUnique (AssignmentsOfGroup (toEntityKey groupKey) key)
  return $! toDomainKey key

-- Returns (Just courseKey) the course key of the assignment if the assignment
-- is a course assignment otherwise Nothing
courseOfAssignment :: Domain.AssignmentKey -> Persist (Maybe Domain.CourseKey)
courseOfAssignment key = do
  courses <- selectList [AssignmentsOfCourseAssignment ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . assignmentsOfCourseCourse . entityVal)
         (listToMaybe courses)

-- Returns (Just groupKey) the group key of the assignment if the assignment
-- is a group assignment otherwise Nothing
groupOfAssignment :: Domain.AssignmentKey -> Persist (Maybe Domain.GroupKey)
groupOfAssignment key = do
  groups <- selectList [AssignmentsOfGroupAssignment ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . assignmentsOfGroupGroup . entityVal)
         (listToMaybe groups)

-- Returns all the submissions for the given assignment
-- TODO: Test
submissionsForAssignment :: Domain.AssignmentKey -> Persist [Domain.SubmissionKey]
submissionsForAssignment key = do
  submissions <- selectList [SubmissionsOfAssignmentAssignment ==. toEntityKey key] []
  return $! map (toDomainKey . submissionsOfAssignmentSubmission . entityVal) submissions

-- Returns when the assignment was saved first, the modification of an assignment
-- does not change the time stamp
assignmentCreatedTime :: Domain.AssignmentKey -> Persist UTCTime
assignmentCreatedTime key = do
  mAsg <- get (toEntityKey key)
  return $!
    maybe (persistError "assignmentCreatedTime" $ "no assignment is found" ++ show key)
          assignmentCreated
          mAsg

-- Returns the test case of the assignment is if there is any attached.
-- returns (Just key) if there is, otherwise Nothing
testCaseOfAssignment :: Domain.AssignmentKey -> Persist (Maybe Domain.TestCaseKey)
testCaseOfAssignment key = do
  testCases <- selectList [TestCaseOfAssignmentAssignment ==. toEntityKey key] []
  return $! fmap (toDomainKey . testCaseOfAssignmentTestCase . entityVal) (listToMaybe testCases)

#ifdef TEST

assignmentTests = do
  shrink "Assignment end-to-end story"
    (do ioTest "Assignment end-to-end test" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          g  <- dbStep $ saveGroup c group
          ca <- dbStep $ saveCourseAssignment c asg
          ga <- dbStep $ saveGroupAssignment g asg

          casg' <- dbStep $ loadAssignment ca
          assertEquals asg casg' "The saved and loaded course assignment were different."
          cca <- dbStep $ courseOfAssignment ca
          assertEquals (Just c) cca "The course assignment has no appropiate course"
          cga <- dbStep $ groupOfAssignment ca
          assertEquals Nothing cga "The course assignment had a group"
          t1 <- dbStep $ assignmentCreatedTime ca
          dbStep $ modifyAssignment ca asg2
          t2 <- dbStep $ assignmentCreatedTime ca
          casg2 <- dbStep $ loadAssignment ca
          assertEquals asg2 casg2 "The course assignment modification has failed"
          assertEquals t1 t2 "The creation time of the course assignment has changed"

          gasg' <- dbStep $ loadAssignment ga
          assertEquals asg gasg' "The saved and loaded group assignment were different."
          cga <- dbStep $ courseOfAssignment ga
          assertEquals Nothing cga "The group assignment had course"
          gga <- dbStep $ groupOfAssignment ga
          assertEquals (Just g) gga "The group assignment had no group"
          t1 <- dbStep $ assignmentCreatedTime ga
          dbStep $ modifyAssignment ga asg2
          t2 <- dbStep $ assignmentCreatedTime ga
          gasg2 <- dbStep $ loadAssignment ga
          assertEquals asg2 gasg2 "The course assignment modification has failed"
          assertEquals t1 t2 "The creation time of the group assignment has changed"
    )
    (do ioTest "Save and load course assignment" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          casg' <- dbStep $ loadAssignment ca
          assertEquals asg casg' "The saved and loaded course assignment were different."
          cca <- dbStep $ courseOfAssignment ca
          assertEquals (Just c) cca "The course assignment has no appropiate course"
          cga <- dbStep $ groupOfAssignment ca
          assertEquals Nothing cga "The course assignment had a group"
        ioTest "Save and load group assignment" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          g  <- dbStep $ saveGroup c group
          ga <- dbStep $ saveGroupAssignment g asg
          gasg' <- dbStep $ loadAssignment ga
          assertEquals asg gasg' "The saved and loaded group assignment were different."
          cca <- dbStep $ courseOfAssignment ga
          assertEquals Nothing cca "The group assignment had a course"
          cga <- dbStep $ groupOfAssignment ga
          assertEquals (Just g) cga "The group assignment had no appropiate group"
        ioTest "Modify course assignment" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          t1 <- dbStep $ assignmentCreatedTime ca
          dbStep $ modifyAssignment ca asg2
          t2 <- dbStep $ assignmentCreatedTime ca
          asg' <- dbStep $ loadAssignment ca
          assertEquals asg2 asg' "The modification of the course assignment has failed"
          assertEquals t1 t2 "The creation time of the course assignment has changed"
        ioTest "Modify group assignment" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          g  <- dbStep $ saveGroup c group
          ga <- dbStep $ saveGroupAssignment g asg
          t1 <- dbStep $ assignmentCreatedTime ga
          dbStep $ modifyAssignment ga asg2
          t2 <- dbStep $ assignmentCreatedTime ga
          asg' <- dbStep $ loadAssignment ga
          assertEquals asg2 asg' "The modification of the group assignment has failed"
          assertEquals t1 t2 "The creation time of the group assignment has changed"
    )

  ioTest "List course assignments" $ runSql $ do
    dbStep initDB
    c  <- dbStep $ saveCourse course
    as <- dbStep $ courseAssignments c
    assertEquals [] as "The course had some assignment after the creation"
    a1 <- dbStep $ saveCourseAssignment c asg
    as <- dbStep $ courseAssignments c
    assertEquals
      (Set.fromList [a1])
      (Set.fromList as) "The course had different assignment set"
    a2 <- dbStep $ saveCourseAssignment c asg
    as <- dbStep $ courseAssignments c
    assertEquals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The course had different assignment set"

  ioTest "List group assignments" $ runSql $ do
    dbStep initDB
    c  <- dbStep $ saveCourse course
    g  <- dbStep $ saveGroup c group
    as <- dbStep $ groupAssignments g
    assertEquals [] as "The group had some assignment after the creation"
    a1 <- dbStep $ saveGroupAssignment g asg
    as <- dbStep $ groupAssignments g
    assertEquals
      (Set.fromList [a1])
      (Set.fromList as) "The group had different assignment set"
    a2 <- dbStep $ saveGroupAssignment g asg
    as <- dbStep $ groupAssignments g
    assertEquals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The group had different assignment set"

#endif
