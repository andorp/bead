{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
#endif

-- * Evaluation

-- Save the evaluation for the given submission
saveEvaluation :: Domain.SubmissionKey -> Domain.Evaluation -> Persist Domain.EvaluationKey
saveEvaluation submissionKey ev = do
  key <- insert (fromDomainValue ev)
  insert (SubmissionOfEvaluation (toEntityKey submissionKey) key)
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
submissionOfEvaluation :: Domain.EvaluationKey -> Persist Domain.SubmissionKey
submissionOfEvaluation key = do
  es <- selectList [ SubmissionOfEvaluationEvaluation ==. toEntityKey key] []
  return $!
    maybe (persistError "submissionOfEvaluation" $ "no evaluation is found " ++ show key)
          (toDomainKey . submissionOfEvaluationSubmission . entityVal)
          (listToMaybe es)

#ifdef TEST

evaluationTests = do
  let course  = Domain.Course "name" "desc" (Domain.BinEval ()) Domain.TestScriptSimple
      time    = read "2014-06-09 12:55:27.959203 UTC"
      sbm     = Domain.Submission "submission" time
      sbm2    = Domain.Submission "submission2" time
      asg     = Domain.Assignment "name" "desc" Domain.Urn time time
      user1name = Domain.Username "user1"
      user1 = Domain.User Domain.Student user1name (Domain.Email "email") "name" (Domain.TimeZoneName "UTC") (Domain.Language "hu")
      ev    = Domain.Evaluation (Domain.BinEval (Domain.Binary Domain.Passed)) "written"
      ev2   = Domain.Evaluation (Domain.PctEval (Domain.Percentage (Domain.Scores [0.01]))) "escrito"
  shrink "Evaluation end-to-end story"
    (do ioTest "Evaluation end-to-end story" $ runSql $ do
          dbStep $ initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          dbStep $ saveUser user1
          s  <- dbStep $ saveSubmission ca user1name sbm
          se1 <- dbStep $ evaluationOfSubmission s
          assertEquals Nothing se1 "Found some evaluation for a non evaluated submission."
          e  <- dbStep $ saveEvaluation s ev
          ev' <- dbStep $ loadEvaluation e
          assertEquals ev ev' "Saved and load evaluation are differents."
          s2 <- dbStep $ submissionOfEvaluation e
          assertEquals s s2 "Submission of the evaluation is not calculated correctly."
          se2 <- dbStep $ evaluationOfSubmission s
          assertEquals (Just e) se2 "Found some evaluation for a non evaluated submission."
          dbStep $ modifyEvaluation e ev2
          ev2' <- dbStep $ loadEvaluation e
          assertEquals ev2 ev2' "The modified evaluation was not read out correctly.")
    (return ()) -- TODO: Shrinked tests
  return ()

#endif
