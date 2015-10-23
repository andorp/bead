module Bead.Domain.Entity.Assessment where

import Bead.Domain.Shared.Evaluation

-- | Assesment for a student, without any submission
-- just an evaluation for it.
data Assessment = Assessment {
    title         :: String
  , description   :: String
  , evaluationCfg :: EvConfig
  } deriving (Eq, Show)

assessment f (Assessment title desc cfg) = f title desc cfg

withAssessment a f = assessment f a

