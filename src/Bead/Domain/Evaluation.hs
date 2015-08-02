{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bead.Domain.Evaluation (
    Evaluate(..)
  , calculateEvaluation
  , score
  , percentage
  , point
  , module Bead.Domain.Shared.Evaluation
  ) where

import Data.Monoid
import Bead.Domain.Shared.Evaluation

#ifdef TEST
import Control.Applicative
import Test.Tasty.Arbitrary
import Test.Tasty.TestSet hiding (shrink)
#endif

class Monoid m => Evaluate m e where
  evaluate :: m -> e -> Result

calculateEvaluation :: Evaluate m e => [m] -> e -> Result
calculateEvaluation [] _ = Failed
calculateEvaluation ms e = evaluate (mconcat ms) e

instance Monoid Binary where
  mempty = Binary Passed
  (Binary Passed) `mappend` (Binary Passed) = Binary Passed
  _               `mappend` _               = Binary Failed

instance Evaluate Binary () where
  evaluate (Binary Passed) _ = Passed
  evaluate (Binary Failed) _ = Failed

score :: a -> Scores a
score x = Scores [x]

instance Monoid (Scores a) where
  mempty = Scores mempty
  mappend (Scores p) (Scores q) = Scores (mappend p q)

percentage :: Double -> Percentage
percentage = Percentage . score

point :: Percentage -> Maybe Double
point (Percentage (Scores [p])) = Just p
point (Percentage _) = Nothing

instance Monoid Percentage where
  mempty = Percentage mempty
  mappend (Percentage p) (Percentage q) = Percentage (mappend p q)

instance Evaluate Percentage PctConfig where
  evaluate (Percentage s) c =
    case unScores s of
      [] -> Failed
      cs -> if ((sum cs) / (fromIntegral . length $ cs) >= (pLimit c))
              then Passed
              else Failed

#ifdef TEST

-- The arbitrary instances are defined here, because FAY does support
-- only the "#ifdef FAY"

instance Arbitrary Result where
  arbitrary = elements   [Passed, Failed]
  shrink    = resultCata [Failed] []

instance Arbitrary Binary where
  arbitrary = Binary <$> arbitrary
  shrink = fmap Binary . binaryCata shrink

instance Arbitrary Percentage where
  arbitrary = Percentage . mkScores <$> arbitrary
  shrink = percentageCata (fmap (Percentage . Scores) . shrink . unScores)

instance Arbitrary EvResult where
  arbitrary = EvResult <$> oneof [
      BinEval <$> arbitrary
    , PctEval <$> arbitrary
    ]
  shrink = fmap EvResult . evResultCata
    (fmap BinEval . shrink)
    (fmap PctEval . shrink)

instance Arbitrary EvConfig where
  arbitrary = EvConfig <$> oneof [
      BinEval <$> arbitrary
    , PctEval <$> arbitrary
    ]
  shrink = fmap EvConfig . evConfigCata
    []
    (fmap PctEval . shrink)
#endif
