{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Feedback (
    Feedback(..)
  , feedback
  , mkFeedback
  , FeedbackInfo(..)
  , feedbackInfo
  , isTestedFeedback
  , feedbackTestResult
#ifdef TEST
  , feedbackTests
#endif
  ) where

import           Control.Applicative
import           Data.Data
import           Data.Time (UTCTime(..))

import           Bead.Domain.Func
import           Bead.Domain.Evaluation

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.TestSet hiding (shrink)
#endif


-- | The feedback info describes information and the stage of the
-- evaluation for a given submission.
data FeedbackInfo
  = TestResult { testResult :: Bool }
    -- ^ Indicates that the submission has passed the automated test scripts
    -- or not, True means that the submission is passed.
  | MessageForStudent { studentComment :: String }
    -- ^ Represents a message that can be viewed by the student
  | MessageForAdmin { adminComment :: String }
    -- ^ Represents a message that can be view by the admin of the course or group
  | Evaluated { evalResult :: EvResult, evalComment :: String, evalAuthor :: String }
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
      MessageForStudent studentComment -> student studentComment
      MessageForAdmin adminComment -> admin adminComment
      Evaluated evalResult evalComment evalAuthor -> evaluated evalResult evalComment evalAuthor

#ifdef TEST
instance Arbitrary FeedbackInfo where
  arbitrary = oneof [
      TestResult <$> arbitrary
    , MessageForStudent <$> arbitrary
    , MessageForAdmin <$> arbitrary
    , Evaluated <$> arbitrary <*> arbitrary <*> arbitrary
    ]
  shrink = feedbackInfo
    (fmap TestResult . shrink)
    (fmap MessageForStudent . shrink)
    (fmap MessageForAdmin . shrink)
    (\evalResult evalComment evalAuthor -> do
      result <- shrink evalResult
      comment <- shrink evalComment
      author <- shrink evalAuthor
      return $ Evaluated result comment author)
#endif

-- | Feedback consist of a piece of information and a date when the information
-- is posted intot the system.
data Feedback = Feedback { info :: FeedbackInfo, postDate :: UTCTime }
  deriving (Data, Eq, Read, Show, Typeable)

feedback g f (Feedback i p) = f (g i) p

-- Creates a feedback in applicative style.
mkFeedback info date = Feedback <$> info <*> date

-- Returns True if the feedback is a test result one, otherwise False
isTestedFeedback :: Feedback -> Bool
isTestedFeedback = feedback
  (feedbackInfo (const True) (const False) (const False) (const3 False))
  forgetTheDate

#ifdef TEST
isTestedFeedbackTests = group "isTestedFeedback" $ do
  let date = read "2014-08-10 17:15:19.707507 UTC"
  eqPartitions isTestedFeedback
    [ Partition "Result: True"      (Feedback (TestResult True) date) True "Does not recognized."
    , Partition "Result: False"     (Feedback (TestResult False) date) True "Does not recognized."
    , Partition "MessageForStudent" (Feedback (MessageForStudent "s") date) False "Does recognized."
    , Partition "MessageForAdmin"   (Feedback (MessageForAdmin "a") date) False "Does recognized."
    , Partition "Evaluated"         (Feedback (Evaluated undefined "a" "b") date) False "Does recognized."
    ]
#endif

-- Returns the result of a test fedback otherwise Nothing
feedbackTestResult :: Feedback -> Maybe Bool
feedbackTestResult = feedback
  (feedbackInfo Just (const Nothing) (const Nothing) (const3 Nothing))
  forgetTheDate

#ifdef TEST
feedbackTestResultTests = group "feedbackTestResult" $ do
  let date = read "2014-08-10 17:15:19.707507 UTC"
  eqPartitions feedbackTestResult
    [ Partition "Result: True"      (Feedback (TestResult True) date) (Just True) "Does not recognized."
    , Partition "Result: False"     (Feedback (TestResult False) date) (Just False) "Does not recognized."
    , Partition "MessageForStudent" (Feedback (MessageForStudent "s") date) Nothing "Does recognized."
    , Partition "MessageForAdmin"   (Feedback (MessageForAdmin "a") date) Nothing "Does recognized."
    , Partition "Evaluated"         (Feedback (Evaluated undefined "a" "b") date) Nothing "Does recognized."
    ]

#endif

-- * Helper

forgetTheDate = p_1_2

#ifdef TEST
feedbackTests = do
  isTestedFeedbackTests
  feedbackTestResultTests
#endif
