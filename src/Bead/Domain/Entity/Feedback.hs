{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Feedback where

import           Data.Data
import           Data.Time (UTCTime(..))

import           Bead.Domain.Shared.Evaluation



-- | The feedback info describes information and the stage of the
-- evaluation for a given submission.
data FeedbackInfo
  = TestResult { testResult :: Bool }
    -- ^ Indicates that the submission has passed the automated test scripts
    -- or not, True means that the submission is passed.
  | CommentForStudent { studentComment :: String }
    -- ^ Represents a message that can be viewed by the student
  | CommentForAdmin { adminComment :: String }
    -- ^ Represents a message that can be view by the admin of the course or group
  | Evaluated { evalResult :: EvResult, evalComment :: String }
    -- ^ Stores the evaluation result at a time, to be able to show later on, if the
    -- evaluation changes over time.
  deriving (Data, Eq, Read, Show, Typeable)

feedbackInfo
  result
  student
  admin
  evaluated
  s
  = case s of
      TestResult testResult -> result testResult
      CommentForStudent studentComment -> student studentComment
      CommentForAdmin adminComment -> admin adminComment
      Evaluated evalResult evalComment -> evaluated evalResult evalComment

-- | Feedback consist of a piece of information and a date when the information
-- is posted intot the system.
data Feedback = Feedback { info :: FeedbackInfo, postDate :: UTCTime }
  deriving (Data, Eq, Read, Show, Typeable)

stage f (Feedback i p) = f i p

