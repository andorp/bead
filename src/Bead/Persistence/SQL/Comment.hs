{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Comment where

import           Data.Maybe

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import qualified Data.Set as Set
import           Data.String (fromString)

import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.Submission
import           Bead.Persistence.SQL.User

import           Bead.Persistence.SQL.TestData

import           Test.Themis.Test (ioTest, shrink)
import           Test.Themis.Keyword.Encaps
#endif

-- * Comment

-- Saves the comment for the given submission
saveComment :: Domain.SubmissionKey -> Domain.Comment -> Persist Domain.CommentKey
saveComment submissionKey c = do
  key <- insert (fromDomainValue c)
  insert (CommentsOfSubmission (toEntityKey submissionKey) key)
  return $! toDomainKey key

-- Loads the comment from the database
loadComment :: Domain.CommentKey -> Persist Domain.Comment
loadComment key = do
  c <- get (toEntityKey key)
  return $!
    maybe (persistError "loadComment" $ "The comment was not found: " ++ show key)
          toDomainValue
          c

-- Returns the submission of the comment
submissionOfComment :: Domain.CommentKey -> Persist Domain.SubmissionKey
submissionOfComment key = do
  sbs <- selectList [ CommentsOfSubmissionComment ==. toEntityKey key ] []
  return $!
    maybe (persistError "submissionOfComment" $ "The comment is not found: " ++ show key)
          (toDomainKey . commentsOfSubmissionSubmission . entityVal)
          (listToMaybe sbs)

#ifdef TEST
commentTests = do
  shrink "Comment end-to-end story."
    (do ioTest "Comment end-to-end test" $ runSql $ do
          dbStep $ initDB
          c  <- dbStep $ saveCourse course
          ca <- dbStep $ saveCourseAssignment c asg
          dbStep $ saveUser user1
          s  <- dbStep $ saveSubmission ca user1name sbm
          cs <- dbStep $ commentsOfSubmission s
          assertEquals
            (Set.fromList [])
            (Set.fromList cs)
            "Comments were for an empty submission."
          cm <- dbStep $ saveComment s cmt
          cs <- dbStep $ commentsOfSubmission s
          assertEquals
            (Set.fromList [cm])
            (Set.fromList cs)
            "Saved comment was not found for the submission."
          cmt' <- dbStep $ loadComment cm
          assertEquals cmt cmt' "The comment was not saved and loaded correctly"
          sc <- dbStep $ submissionOfComment cm
          assertEquals s sc "The submission of the comment was wrong"
          cm2 <- dbStep $ saveComment s cmt
          cs <- dbStep $ commentsOfSubmission s
          assertEquals
            (Set.fromList [cm,cm2])
            (Set.fromList cs)
            "Comments of the submission were wrong."
        return ())
    (do return ())
  return ()
#endif

