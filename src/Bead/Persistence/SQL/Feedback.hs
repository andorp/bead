{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Feedback where

import           Data.Maybe

import           Database.Persist.Sql

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import qualified Data.Set as Set

import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.Submission
import           Bead.Persistence.SQL.User

import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, shrink, equals)
#endif

-- * Comment

-- Saves the feedback for the given submission
saveFeedback :: Domain.SubmissionKey -> Domain.Feedback -> Persist Domain.FeedbackKey
saveFeedback submissionKey f = do
  key <- insert (fromDomainValue f)
  insert (FeedbacksOfSubmission (toEntityKey submissionKey) key)
  return $! toDomainKey key

-- Loads the feedback from the database
loadFeedback :: Domain.FeedbackKey -> Persist Domain.Feedback
loadFeedback key = do
  f <- get (toEntityKey key)
  return $!
    maybe (persistError "loadFeedback" $ "The feedback was not found: " ++ show key)
          toDomainValue
          f

-- Returns the submission of the comment
submissionOfFeedback :: Domain.FeedbackKey -> Persist Domain.SubmissionKey
submissionOfFeedback key = do
  sbs <- selectList [ FeedbacksOfSubmissionFeedback ==. toEntityKey key ] []
  return $!
    maybe (persistError "submissionsOfFeedback" $ "The feedback is not found: " ++ show key)
          (toDomainKey . feedbacksOfSubmissionSubmission . entityVal)
          (listToMaybe sbs)

#ifdef TEST
feedbackTests = do
  shrink "Feedback end-to-end story."
    (do ioTest "Feedback end-to-end test" $ runSql $ do
          initDB
          c  <- saveCourse course
          ca <- saveCourseAssignment c asg
          saveUser user1
          s  <- saveSubmission ca user1name sbm
          fs <- feedbacksOfSubmission s
          equals
            (Set.fromList [])
            (Set.fromList fs)
            "Feedbacks were for an empty submission."

          let saveFb s f = do fm <- saveFeedback s f
                              f' <- loadFeedback fm
                              equals f f' "The feedback was not saved and loaded correctly"
                              return fm

          fm <- saveFb s fbTestResult
          fs <- feedbacksOfSubmission s
          equals
            (Set.fromList [fm])
            (Set.fromList fs)
            "Saved feedback was not found for the submission."
          fc <- submissionOfFeedback fm
          equals s fc "The submission of the feedback was wrong"

          fm2 <- saveFb s fbMsgStudent
          fs <- feedbacksOfSubmission s
          equals
            (Set.fromList [fm,fm2])
            (Set.fromList fs)
            "Feedbacks of the submission were wrong."

          saveFb s fbMsgForAdmin
          saveFb s fbEvaluated

        return ())
    (do return ())
  return ()
#endif

