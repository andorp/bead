{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.Assessment where

import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sql
import           Text.JSON.Generic (encodeJSON)

import qualified Bead.Domain.Entities          as Domain
import qualified Bead.Domain.Relationships     as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON

#ifdef TEST
import qualified Data.Set as Set

import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.Group
import           Bead.Persistence.SQL.MySQLTestRunner
import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, shrink, equals)
#endif

-- * Assessment

saveCourseAssessment :: Domain.CourseKey -> Domain.Assessment -> Persist Domain.AssessmentKey
saveCourseAssessment ck assessment = do
  key <- insert (fromDomainValue assessment)
  insertUnique (AssessmentsOfCourse (toEntityKey ck) key)
  return $! toDomainKey key

saveGroupAssessment :: Domain.GroupKey -> Domain.Assessment -> Persist Domain.AssessmentKey
saveGroupAssessment gk assessment = do
  key <- insert (fromDomainValue assessment)
  insertUnique (AssessmentsOfGroup (toEntityKey gk) key)
  return $! toDomainKey key

loadAssessment :: Domain.AssessmentKey -> Persist Domain.Assessment
loadAssessment key = do
 mAsg <- get (toEntityKey key)
 return $!
   maybe (persistError "loadAssessment" $ "No assessment is found. " ++ show key)
         toDomainValue
         mAsg

modifyAssessment :: Domain.AssessmentKey -> Domain.Assessment -> Persist ()
modifyAssessment key assessment = do
  update (toEntityKey key) $ Domain.withAssessment assessment
    $ \title desc _created cfg ->
        [ AssessmentDescription =. (Text.pack . encodeJSON $ (title,desc))
        , AssessmentEvalConfig  =. encodeEvalConfig cfg
        ]

courseOfAssessment :: Domain.AssessmentKey -> Persist (Maybe Domain.CourseKey)
courseOfAssessment key = do
  courses <- selectList [AssessmentsOfCourseAssessment ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . assessmentsOfCourseCourse . entityVal)
         (listToMaybe courses)

groupOfAssessment :: Domain.AssessmentKey -> Persist (Maybe Domain.GroupKey)
groupOfAssessment key = do
  groups <- selectList [AssessmentsOfGroupAssessment ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . assessmentsOfGroupGroup . entityVal)
         (listToMaybe groups)

scoresOfAssessment :: Domain.AssessmentKey -> Persist [Domain.ScoreKey]
scoresOfAssessment key = do
  scores <- selectList [ScoresOfUsernameAssessmentAssessment ==. toEntityKey key] []
  return $! fmap (toDomainKey . scoresOfUsernameAssessmentScore . entityVal) scores

assessmentsOfCourse :: Domain.CourseKey -> Persist [Domain.AssessmentKey]
assessmentsOfCourse key = do
  assessments <- selectList [AssessmentsOfCourseCourse ==. toEntityKey key] []
  return $! map (toDomainKey . assessmentsOfCourseAssessment . entityVal)
                assessments

assessmentsOfGroup :: Domain.GroupKey -> Persist [Domain.AssessmentKey]
assessmentsOfGroup key = do
  assessments <- selectList [AssessmentsOfGroupGroup ==. toEntityKey key] []
  return $! map (toDomainKey . assessmentsOfGroupAssessment . entityVal)
                assessments

#ifdef TEST

assessmentTests = do
  ioTest "Assessment end-to-end test" $ runSql $ do
    c  <- saveCourse course
    g  <- saveGroup c group
    ca <- saveCourseAssessment c ast
    ga <- saveGroupAssessment g ast

    cast' <- loadAssessment ca
    equals ast cast' "The saved and loaded course assessnment were different."
    cca <- courseOfAssessment ca
    equals (Just c) cca "The course assessment has no appropiate course"
    cga <- groupOfAssessment ca
    equals Nothing cga "The course assessment had a group"
    modifyAssessment ca ast2
    cast2 <- loadAssessment ca
    equals ast2 cast2 "The course assessment modification has failed"

    gast' <- loadAssessment ga
    equals ast gast' "The saved and loaded group assessnment were different."
    cga <- courseOfAssessment ga
    equals Nothing cga "The group assessment had course"
    gga <- groupOfAssessment ga
    equals (Just g) gga "The group assessment had no group"
    modifyAssessment ga ast2
    gast2 <- loadAssessment ga
    equals ast2 gast2 "The course assessment modification has failed"

  ioTest "List course assessments" $ runSql $ do
    c  <- saveCourse course
    as <- assessmentsOfCourse c
    equals [] as "The course had some assessment after the creation"
    a1 <- saveCourseAssessment c ast
    as <- assessmentsOfCourse c
    equals
      (Set.fromList [a1])
      (Set.fromList as) "The course had different assessment set"
    a2 <- saveCourseAssessment c ast
    as <- assessmentsOfCourse c
    equals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The course had different assessment set"

  ioTest "List group assessments" $ runSql $ do
    c  <- saveCourse course
    g  <- saveGroup c group
    as <- assessmentsOfGroup g
    equals [] as "The group had some assessment after the creation"
    a1 <- saveGroupAssessment g ast
    as <- assessmentsOfGroup g
    equals
      (Set.fromList [a1])
      (Set.fromList as) "The group had different assessment set"
    a2 <- saveGroupAssessment g ast
    as <- assessmentsOfGroup g
    equals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The group had different assessment set"

#endif
