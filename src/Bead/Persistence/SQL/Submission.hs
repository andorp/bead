{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Submission where

import           Control.Applicative
import           Data.Function (on)
import           Data.Maybe
import           Data.List (maximumBy)
import qualified Data.Text as Text

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import qualified Data.Set as Set

import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.User

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
#endif

-- * Submission

-- Saves the submission for a given assignment, submitted by the given user
saveSubmission :: Domain.AssignmentKey -> Domain.Username -> Domain.Submission -> Persist Domain.SubmissionKey
saveSubmission assignmentKey username submission =
  withUser
    username
    (persistError "saveSubmission" $ "No user is found" ++ show username)
    (\userEnt -> do
       key <- insert (fromDomainValue submission)
       let assignmentKey' = toEntityKey assignmentKey
           userKey        = entityKey userEnt
       insertUnique (SubmissionsOfAssignment assignmentKey' key)
       insertUnique (UserOfSubmission key userKey)
       insert (UserSubmissionOfAssignment key assignmentKey' userKey)
       insert (OpenedSubmission key assignmentKey' userKey)
       return $! toDomainKey key)

-- Loads the given submission from the database
loadSubmission :: Domain.SubmissionKey -> Persist Domain.Submission
loadSubmission key = do
  mSub <- get (toEntityKey key)
  return $!
    maybe
      (persistError "loadSubmission" $ "submission is not found." ++ show key)
      toDomainValue
      mSub

-- Returns the assignment for the submission
assignmentOfSubmission :: Domain.SubmissionKey -> Persist Domain.AssignmentKey
assignmentOfSubmission key = do
  assignments <- selectList [SubmissionsOfAssignmentSubmission ==. toEntityKey key] []
  return $!
    maybe
      (persistError "assignmentOfSubmission" $ "no submission was found " ++ show key)
      (toDomainKey . submissionsOfAssignmentAssignment . entityVal)
      (listToMaybe assignments)

-- Returns the username for the submission
usernameOfSubmission :: Domain.SubmissionKey -> Persist Domain.Username
usernameOfSubmission key = do
  usersOfSub <- selectList [UserOfSubmissionSubmission ==. toEntityKey key] []
  maybe
    (persistError "usernameOfSubmission" $ "No submission was found " ++ show key)
    (\userOfSub -> do
        let userId = userOfSubmissionUser $ entityVal userOfSub
        mUser <- get userId
        maybe
          (persistError "usernameOfSubmission" $ "No user us found " ++ show userId)
          (\user -> return $! Domain.Username . Text.unpack $ userUsername user)
          mUser)
    (listToMaybe usersOfSub)

-- Lists all the submissions stored in the database
submissionKeys :: Persist [Domain.SubmissionKey]
submissionKeys = map toDomainKey <$> selectSubmissionKeys
  where
    selectSubmissionKeys :: Persist [Key Submission]
    selectSubmissionKeys = selectKeysList [] []

-- Returns the evaluation for the submission if the evalution exist, otherwise Nothing
evaluationOfSubmission :: Domain.SubmissionKey -> Persist (Maybe Domain.EvaluationKey)
evaluationOfSubmission key = do
  es <- selectList [ SubmissionOfEvaluationSubmission ==. toEntityKey key] []
  return $
    fmap (toDomainKey . submissionOfEvaluationEvaluation . entityVal)
         (listToMaybe es)

-- Returns all the comments for the given submission
commentsOfSubmission :: Domain.SubmissionKey -> Persist [Domain.CommentKey]
commentsOfSubmission key =
  map (toDomainKey . commentsOfSubmissionComment . entityVal) <$>
    selectList [CommentsOfSubmissionSubmission ==. toEntityKey key] []

-- Returns all the feedbacks for the given submission
feedbacksOfSubmission :: Domain.SubmissionKey -> Persist [Domain.FeedbackKey]
feedbacksOfSubmission key =
  map (toDomainKey . feedbacksOfSubmissionFeedback . entityVal) <$>
    selectList [FeedbacksOfSubmissionSubmission ==. toEntityKey key] []

-- Returns the last submission of an assignment submitted by the given user if the
-- user is submitted something otherwise Nothing
lastSubmission :: Domain.AssignmentKey -> Domain.Username -> Persist (Maybe Domain.SubmissionKey)
lastSubmission assignmentKey username =
  withUser
    username
    (persistError "lastSubmission" $ "The user was not found " ++ show username)
    (\userEnt -> do
       submissionKeys <-
         map (userSubmissionOfAssignmentSubmission . entityVal) <$> selectList
           [ UserSubmissionOfAssignmentAssignment ==. toEntityKey assignmentKey
           , UserSubmissionOfAssignmentUser       ==. entityKey userEnt
           ] []
       case submissionKeys of
         [] -> return Nothing
         ks -> do subEnts <- catMaybes <$> mapM getWithKey ks
                  return $! Just . toDomainKey . entityKey $
                    maximumBy (on compare (submissionPostDate . entityVal)) subEnts)
  where
    getWithKey key = (fmap (Entity key)) <$> (get key)

-- Remove the submission from the opened (which need to be evaluated) queue
removeFromOpened :: Domain.AssignmentKey -> Domain.Username -> Domain.SubmissionKey -> Persist ()
removeFromOpened ak username sk = withUser username (return ()) $ \userEnt -> void $ do
  deleteWhere
    [ OpenedSubmissionSubmission ==. toEntityKey sk
    , OpenedSubmissionAssignment ==. toEntityKey ak
    , OpenedSubmissionUser       ==. entityKey userEnt
    ]

-- Returns all the opened submissions
openedSubmissions :: Persist [Domain.SubmissionKey]
openedSubmissions = do
  openeds <- selectOpenedSubmissions
  return $! map (toDomainKey . openedSubmissionSubmission . entityVal) openeds
  where
    selectOpenedSubmissions :: Persist [Entity OpenedSubmission]
    selectOpenedSubmissions = selectList [] []

-- Calculates all the opened submisison for a given user and a given assignment
usersOpenedSubmissions :: Domain.AssignmentKey -> Domain.Username -> Persist [Domain.SubmissionKey]
usersOpenedSubmissions key username =
  withUser
    username
    (persistError "usersOpenedSubmissions" $ "No user is found" ++ show username)
    (\userEnt -> do
       map (toDomainKey . openedSubmissionSubmission . entityVal) <$> selectList
         [ OpenedSubmissionAssignment ==. toEntityKey key
         , OpenedSubmissionUser       ==. entityKey userEnt
         ] [])

#ifdef TEST

submissionTests = do
  let course  = Domain.Course "name" "desc" Domain.TestScriptSimple
      time    = read "2014-06-09 12:55:27.959203 UTC"
      sbm     = Domain.Submission "submission" time
      sbm2    = Domain.Submission "submission2" time
      ballot  = Domain.aspectsFromList [Domain.BallotBox]
      asg     = Domain.Assignment "name" "desc" ballot time time Domain.binaryConfig
      user1name = Domain.Username "user1"
      user1  = Domain.User Domain.Student user1name (Domain.Email "email") "name" (Domain.TimeZoneName "UTC") (Domain.Language "hu")
      user2name = Domain.Username "user2"
      user2  = Domain.User Domain.Student user2name (Domain.Email "email") "name" (Domain.TimeZoneName "UTC") (Domain.Language "hu")

  shrink "Submission end-to-end story."
    (do ioTest "Submission end-to-end test case" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          dbStep $ saveUser user1
          ls0 <- dbStep $ lastSubmission ca user1name
          assertEquals Nothing ls0 "Some submission is returned"
          s  <- dbStep $ saveSubmission ca user1name sbm
          os <- dbStep $ openedSubmissions
          assertEquals [s] os "The opened submission returned wrong set"
          sbm' <- dbStep $ loadSubmission s
          assertEquals sbm sbm' "Saved and loaded submission were different."
          ca' <- dbStep $ assignmentOfSubmission s
          assertEquals ca ca' "Wrong assignment key was returned for the submission"
          uname' <- dbStep $ usernameOfSubmission s
          assertEquals user1name uname' "Wrong username was returned for the submission"
          ls <- dbStep $ lastSubmission ca user1name
          assertEquals (Just s) ls "Wrong last submission was returned"
          s2 <- dbStep $ saveSubmission ca user1name sbm2
          ls <- dbStep $ lastSubmission ca user1name
          assertEquals (Just s2) ls "Wrong last submission after a resubmission"
          os <- dbStep $ openedSubmissions
          assertEquals
            (Set.fromList [s,s2])
            (Set.fromList os)
            "The opened submission returned wrong set after the second submission"
          dbStep $ saveUser user2
          s3 <- dbStep $ saveSubmission ca user2name sbm
          os2 <- dbStep $ openedSubmissions
          assertEquals
            (Set.fromList [s,s2,s3])
            (Set.fromList os2)
            "The opened submission returned wrong set after the second user's submission"
          os4 <- dbStep $ usersOpenedSubmissions ca user1name
          assertEquals
            (Set.fromList [s,s2])
            (Set.fromList os4)
            "The users submission set was not calculated correctly #1"
          os5 <- dbStep $ usersOpenedSubmissions ca user2name
          assertEquals
            (Set.fromList [s3])
            (Set.fromList os5)
            "The users submission set was not calculated correctly #2"
          us1 <- dbStep $ userSubmissions user1name ca
          assertEquals
            (Set.fromList [s,s2])
            (Set.fromList us1)
            "Submissions for user1 and course assignment were wrong"
          us2 <- dbStep $ userSubmissions user2name ca
          assertEquals
            (Set.fromList [s3])
            (Set.fromList us2)
            "Submissions for user2 and course assignment were wrong"
          ca2 <- dbStep $ saveCourseAssignment c asg
          us3 <- dbStep $ userSubmissions user1name ca2
          assertEquals [] us3 "Submissions found for user assignment pair that does not have any"
          dbStep $ removeFromOpened ca user1name s
          os6 <- dbStep $ openedSubmissions
          assertEquals
            (Set.fromList [s2,s3])
            (Set.fromList os6)
            "The opened submission returned wrong set after removing one opened submissions for user1"
          return ())
    (do ioTest "Save and load submission" $ runSql $ do
          dbStep initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          dbStep $ saveUser user1
          s  <- dbStep $ saveSubmission ca user1name sbm
          sbm' <- dbStep $ loadSubmission s
          assertEquals sbm sbm' "Saved and loaded submission were different.")
  return ()

#endif
