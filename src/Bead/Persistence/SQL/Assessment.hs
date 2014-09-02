{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.Assessment where

import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sqlite

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

import           Bead.Persistence.SQL.TestData

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
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
    $ \desc cfg ->
        [ AssessmentDescription =. Text.pack desc
        , AssessmentEvalConfig  =. encodeEvalConfig cfg
        ]

courseOfAssessment :: Domain.AssessmentKey -> Persist (Maybe Domain.CourseKey)
courseOfAssessment key = do
  courses <- selectList [AssessmentsOfCourseAssessment ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . assessmentsOfCourseCourse  . entityVal)
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
  shrink "Assessment end-to-end story"
    (do ioTest "Assessment end-to-end test" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          g  <- dbStep $ saveGroup c group
          ca <- dbStep $ saveCourseAssessment c ast
          ga <- dbStep $ saveGroupAssessment g ast

          cast' <- dbStep $ loadAssessment ca
          assertEquals ast cast' "The saved and loaded course assessnment were different."
          cca <- dbStep $ courseOfAssessment ca
          assertEquals (Just c) cca "The course assessment has no appropiate course"
          cga <- dbStep $ groupOfAssessment ca
          assertEquals Nothing cga "The course assessment had a group"
          dbStep $ modifyAssessment ca ast2
          cast2 <- dbStep $ loadAssessment ca
          assertEquals ast2 cast2 "The course assessment modification has failed"

          gast' <- dbStep $ loadAssessment ga
          assertEquals ast gast' "The saved and loaded group assessnment were different."
          cga <- dbStep $ courseOfAssessment ga
          assertEquals Nothing cga "The group assessment had course"
          gga <- dbStep $ groupOfAssessment ga
          assertEquals (Just g) gga "The group assessment had no group"
          dbStep $ modifyAssessment ga ast2
          gast2 <- dbStep $ loadAssessment ga
          assertEquals ast2 gast2 "The course assessment modification has failed"
    ) (return ())

  ioTest "List course assessments" $ runSql $ do
    dbStep initDB
    c  <- dbStep $ saveCourse course
    as <- dbStep $ assessmentsOfCourse c
    assertEquals [] as "The course had some assessment after the creation"
    a1 <- dbStep $ saveCourseAssessment c ast
    as <- dbStep $ assessmentsOfCourse c
    assertEquals
      (Set.fromList [a1])
      (Set.fromList as) "The course had different assessment set"
    a2 <- dbStep $ saveCourseAssessment c ast
    as <- dbStep $ assessmentsOfCourse c
    assertEquals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The course had different assessment set"

  ioTest "List group assessments" $ runSql $ do
    dbStep initDB
    c  <- dbStep $ saveCourse course
    g  <- dbStep $ saveGroup c group
    as <- dbStep $ assessmentsOfGroup g
    assertEquals [] as "The group had some assessment after the creation"
    a1 <- dbStep $ saveGroupAssessment g ast
    as <- dbStep $ assessmentsOfGroup g
    assertEquals
      (Set.fromList [a1])
      (Set.fromList as) "The group had different assessment set"
    a2 <- dbStep $ saveGroupAssessment g ast
    as <- dbStep $ assessmentsOfGroup g
    assertEquals
      (Set.fromList [a1,a2])
      (Set.fromList as) "The group had different assessment set"

#endif
