{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Shared.Evaluation where

import Prelude
import Data.Data

{- Shared data structures between Client and Server -}

data Result = Passed | Failed
  deriving (Eq, Show, Read, Ord, Data, Typeable)

resultCata passed failed r = case r of
  Passed -> passed
  Failed -> failed

-- Represents the evaluation type for an assignment
data EvaluationData b p
  = BinEval b
  | PctEval p
  deriving (Eq, Show, Read, Ord, Data, Typeable)

evaluationDataMap :: (b -> a) -> (p -> a) -> EvaluationData b p -> a
evaluationDataMap f _ (BinEval x) = f x
evaluationDataMap _ f (PctEval x) = f x

withEvaluationData d f g = evaluationDataMap f g d

evaluationTypes :: [EvaluationData () ()]
evaluationTypes = [BinEval (), PctEval ()]

binaryEval :: EvaluationData b p -> Maybe b
binaryEval (BinEval b) = Just b
binaryEval _           = Nothing

percentEval :: EvaluationData b p -> Maybe p
percentEval (PctEval p) = Just p
percentEval _           = Nothing

encodeEvalType :: EvaluationData a b -> String
encodeEvalType (BinEval _) = "BinEval"
encodeEvalType (PctEval _) = "PctEval"

decodeEvalType :: String -> EvaluationData () ()
decodeEvalType "\"BinEval\"" = BinEval ()
decodeEvalType "\"PctEval\"" = PctEval ()
decodeEvalType s = error $ "decodeEvalType: '" ++ s ++ "'"

data PctConfig = PctConfig { pLimit :: Double }
  deriving (Eq, Show, Read, Data, Typeable)

data Scores a = Scores { unScores :: [a] }
  deriving (Eq, Show, Read, Ord, Data, Typeable)

data Binary = Binary Result
  deriving (Eq, Show, Read, Ord, Data, Typeable)

binaryCata f (Binary x) = f x

data Percentage = Percentage (Scores Double)
  deriving (Eq, Show, Read, Ord, Data, Typeable)

percentageCata f (Percentage x) = f x

data EvResult = EvResult {
    evResult :: EvaluationData Binary Percentage
  } deriving (Eq, Show, Read, Ord, Data, Typeable)

evResultCata
  binary
  percentage
  e = case e of
    (EvResult (BinEval b)) -> binary b
    (EvResult (PctEval p)) -> percentage p

withEvResult result binary percentage
  = evResultCata binary percentage result

percentageResult :: Double -> EvResult
percentageResult d = EvResult (PctEval (Percentage (Scores { unScores = [ d ]})))

percentValue :: EvResult -> Maybe Double
percentValue (EvResult (PctEval (Percentage (Scores [p])))) = Just p
percentValue _ = Nothing

binaryResult :: Result -> EvResult
binaryResult r = EvResult (BinEval (Binary r))

data EvConfig = EvConfig {
    evConfig :: EvaluationData () Double
  } deriving (Eq, Show, Read, Data, Typeable)

evConfigCata
  binary
  percentage
  e = case e of
    (EvConfig (BinEval ())) -> binary
    (EvConfig (PctEval p))  -> percentage p

withEvConfig e b p = evConfigCata b p e

percentageConfig :: Double -> EvConfig
percentageConfig = EvConfig . PctEval

binaryConfig :: EvConfig
binaryConfig = EvConfig (BinEval ())

-- Command that can send from the evaluation page to the
-- server. It consists of a comment value, come from the
-- text field, or the value of the evaluation
data EvalOrComment
  = EvCmtComment
  | EvCmtResult EvResult
  deriving (Eq, Show, Read, Data, Typeable)

evalOrCommentCata
  comment
  result
  e = case e of
    EvCmtComment  -> comment
    EvCmtResult r -> result r

withEvalOrComment e comment result = evalOrCommentCata comment result e
