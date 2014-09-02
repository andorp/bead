{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.Evaluation where

import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON

#ifdef TEST
import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.Submission
import           Bead.Persistence.SQL.User

import           Bead.Persistence.SQL.TestData

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
#endif

-- * Evaluation

-- Save the evaluation for the given submission
saveSubmissionEvaluation :: Domain.SubmissionKey -> Domain.Evaluation -> Persist Domain.EvaluationKey
saveSubmissionEvaluation submissionKey ev = do
  key <- insert (fromDomainValue ev)
  insert (SubmissionOfEvaluation (toEntityKey submissionKey) key)
  return $! toDomainKey key

-- Save the evaluation for the given submission
saveScoreEvaluation :: Domain.ScoreKey -> Domain.Evaluation -> Persist Domain.EvaluationKey
saveScoreEvaluation scoreKey ev = do
  key <- insert (fromDomainValue ev)
  insert (ScoreOfEvaluation (toEntityKey scoreKey) key)
  return $! toDomainKey key

-- Load the evaluatuon from the database
loadEvaluation :: Domain.EvaluationKey -> Persist Domain.Evaluation
loadEvaluation key = do
  val <- get (toEntityKey key)
  return $!
    maybe (persistError "loadEvaluation" $ "no evaluation is found " ++ show key)
          toDomainValue
          val

-- Modify the evalution for the given key in the database
modifyEvaluation :: Domain.EvaluationKey -> Domain.Evaluation -> Persist ()
modifyEvaluation key ev =
  update (fromDomainKey key) $ Domain.withEvaluation ev $ \result written ->
    [ EvaluationResult  =. encodeEvaluationResult result
    , EvaluationWritten =. Text.pack written
    ]

-- Returns the submission of the given evaluation
submissionOfEvaluation :: Domain.EvaluationKey -> Persist (Maybe Domain.SubmissionKey)
submissionOfEvaluation key = do
  es <- selectList [ SubmissionOfEvaluationEvaluation ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . submissionOfEvaluationSubmission . entityVal)
         (listToMaybe es)

-- Returns the score entry of the given evaluation
scoreOfEvaluation :: Domain.EvaluationKey -> Persist (Maybe Domain.ScoreKey)
scoreOfEvaluation key = do
  scores <- selectList [ScoreOfEvaluationEvaluation ==. toEntityKey key] []
  return $!
    fmap (toDomainKey . scoreOfEvaluationScore . entityVal)
         (listToMaybe scores)

#ifdef TEST

evaluationTests = do
  shrink "Evaluation end-to-end story"
    (do ioTest "Evaluation end-to-end story" $ runSql $ do
          dbStep $ initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          dbStep $ saveUser user1
          s  <- dbStep $ saveSubmission ca user1name sbm
          se1 <- dbStep $ evaluationOfSubmission s
          assertEquals Nothing se1 "Found some evaluation for a non evaluated submission."
          e  <- dbStep $ saveSubmissionEvaluation s ev
          ev' <- dbStep $ loadEvaluation e
          assertEquals ev ev' "Saved and load evaluation are differents."
          s2 <- dbStep $ submissionOfEvaluation e
          assertEquals (Just s) s2 "Submission of the evaluation is not calculated correctly."
          se2 <- dbStep $ evaluationOfSubmission s
          assertEquals (Just e) se2 "Found some evaluation for a non evaluated submission."
          dbStep $ modifyEvaluation e ev2
          ev2' <- dbStep $ loadEvaluation e
          assertEquals ev2 ev2' "The modified evaluation was not read out correctly.")
    (return ()) -- TODO: Shrinked tests
  return ()

#endif
