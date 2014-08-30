module Bead.Domain.Entity.Assessment where

import Bead.Domain.Shared.Evaluation

-- | Assesment for a student, without any submission
-- just an evaluation for it.
data Assessment = Assessment {
    description   :: String
  , evaluationCfg :: EvConfig
  } deriving (Eq, Show)

assessment f (Assessment desc cfg) = f desc cfg

withAssessment a f = assessment f a

