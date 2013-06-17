{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Shared.Evaulation where

import Prelude
import Data.Data

{- Shared data structures between Client and Server -}

data Result = Passed | Failed
  deriving (Eq, Show, Read, Data, Typeable)

data EvaulationData b p
  = BinEval b
  | PctEval p
  deriving (Eq, Show, Read, Data, Typeable)

evaulationTypes :: [EvaulationData () ()]
evaulationTypes = [BinEval (), PctEval ()]

binaryEval :: EvaulationData b p -> Maybe b
binaryEval (BinEval b) = Just b
binaryEval _           = Nothing

percentEval :: EvaulationData b p -> Maybe p
percentEval (PctEval p) = Just p
percentEval _           = Nothing

encodeEvalType :: EvaulationData a b -> String
encodeEvalType (BinEval _) = "BinEval"
encodeEvalType (PctEval _) = "PctEval"

decodeEvalType :: String -> EvaulationData () ()
decodeEvalType "BinEval" = BinEval ()
decodeEvalType "PctEval" = PctEval ()

data PctConfig = PctConfig { pLimit :: Double }
  deriving (Eq, Show, Read, Data, Typeable)

data Scores a = Scores { unScores :: [a] }
  deriving (Eq, Show, Read, Data, Typeable)

data Binary = Binary Result
  deriving (Eq, Show, Read, Data, Typeable)

data Percentage = Percentage (Scores Double)
  deriving (Eq, Show, Read, Data, Typeable)

data EvResult = EvResult {
    evResult :: EvaulationData Binary Percentage
  } deriving (Eq, Show, Read, Data, Typeable)

percentageResult :: Double -> EvResult
percentageResult d = EvResult (PctEval (Percentage (Scores { unScores = [ d ]})))
