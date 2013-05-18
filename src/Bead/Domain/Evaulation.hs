{-# LANGUAGE MultiParamTypeClasses #-}
module Bead.Domain.Evaulation where

import Control.Monad.Reader
import Data.Monoid


class Monoid m => Evaulate m e where
  evaulate :: m -> e -> Result

calculateEvaulation :: Evaulate m e => [m] -> e -> Result
calculateEvaulation = evaulate . mconcat

data Result = Passed | Failed
  deriving (Eq, Show, Read)

newtype Binary = Binary Result
  deriving (Eq, Show, Read)

binary = Binary

instance Monoid Binary where
  mempty = Binary Failed
  (Binary Passed) `mappend` (Binary Passed) = Binary Passed
  _               `mappend` _               = Binary Failed

instance Evaulate Binary () where
  evaulate (Binary Passed) _ = Passed
  evaulate (Binary Failed) _ = Failed

newtype Scores a = Scores { unScores :: [a] }
  deriving (Eq, Show, Read)

score :: a -> Scores a
score x = Scores [x]

instance Monoid (Scores a) where
  mempty = Scores mempty
  mappend (Scores p) (Scores q) = Scores (mappend p q)

data Percentage = Percentage (Scores Float)
  deriving (Eq, Show, Read)

percentage :: Float -> Percentage
percentage = Percentage . score

point :: Percentage -> Maybe Float
point (Percentage (Scores [p])) = Just p
point (Percentage _) = Nothing

instance Monoid Percentage where
  mempty = Percentage mempty
  mappend (Percentage p) (Percentage q) = Percentage (mappend p q)

data PctConfig = PctConfig { pLimit :: Float }
  deriving (Eq, Show, Read)

instance Evaulate Percentage PctConfig where
  evaulate (Percentage s) c =
    case unScores s of
      [] -> Failed
      cs -> if ((sum cs) / (fromIntegral . length $ cs) >= (pLimit c))
              then Passed
              else Failed
