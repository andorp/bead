module Bead.Domain.Entity.Assessment where

import Data.Time (UTCTime)

import Bead.Domain.Shared.Evaluation

-- | Assesment for a student, without any submission
-- just an evaluation for it.
data Assessment = Assessment {
    title         :: String
  , description   :: String
  , created       :: UTCTime
  , evaluationCfg :: EvConfig
  } deriving (Eq, Show)

assessment f (Assessment title desc creation cfg) = f title desc creation cfg

withAssessment a f = assessment f a

