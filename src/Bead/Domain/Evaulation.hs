{-# LANGUAGE MultiParamTypeClasses #-}
module Bead.Domain.Evaulation (
    Evaulate(..)
  , calculateEvaulation
  , score
  , percentage
  , point
  , module Bead.Domain.Shared.Evaulation
  ) where

import Control.Monad.Reader
import Data.Monoid

import Bead.Domain.Shared.Evaulation

class Monoid m => Evaulate m e where
  evaulate :: m -> e -> Result

calculateEvaulation :: Evaulate m e => [m] -> e -> Result
calculateEvaulation = evaulate . mconcat

instance Monoid Binary where
  mempty = Binary Failed
  (Binary Passed) `mappend` (Binary Passed) = Binary Passed
  _               `mappend` _               = Binary Failed

instance Evaulate Binary () where
  evaulate (Binary Passed) _ = Passed
  evaulate (Binary Failed) _ = Failed

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

instance Evaulate Percentage PctConfig where
  evaulate (Percentage s) c =
    case unScores s of
      [] -> Failed
      cs -> if ((sum cs) / (fromIntegral . length $ cs) >= (pLimit c))
              then Passed
              else Failed
