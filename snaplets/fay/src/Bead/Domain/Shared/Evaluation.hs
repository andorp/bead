{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Shared.Evaluation where

import Prelude
import Data.Data

{- Shared data structures between Client and Server -}

data Result = Passed | Failed
  deriving (Eq, Show, Read, Data, Typeable)

resultCata
  passed
  failed
  r = case r of
    Passed -> passed
    Failed -> failed

-- Represents the evaluation type for an assignment
data EvaluationData b p f
  = BinEval b
  | PctEval p
  | FreeEval f
  deriving (Eq, Show, Read, Data, Typeable)

evaluationDataMap
  binEval
  pctEval
  freeEval
  e = case e of
    BinEval b -> binEval b
    PctEval p -> pctEval p
    FreeEval f -> freeEval f

withEvaluationData d binEval pctEval freeEval
  = evaluationDataMap binEval pctEval freeEval d

evaluationTypes :: [EvaluationData () () ()]
evaluationTypes = [BinEval (), PctEval (), FreeEval ()]

binaryEval :: EvaluationData b p f -> Maybe b
binaryEval (BinEval b) = Just b
binaryEval _           = Nothing

percentEval :: EvaluationData b p f -> Maybe p
percentEval (PctEval p) = Just p
percentEval _           = Nothing

encodeEvalType :: EvaluationData b p f -> String
encodeEvalType = evaluationDataMap
  (const "BinEval")
  (const "PctEval")
  (const "FreeEval")

decodeEvalType :: String -> EvaluationData () () ()
decodeEvalType "\"BinEval\"" = BinEval ()
decodeEvalType "\"PctEval\"" = PctEval ()
decodeEvalType "\"FreeEval\"" = FreeEval ()
decodeEvalType s = error $ "decodeEvalType: '" ++ s ++ "'"

data PctConfig = PctConfig { pLimit :: Double }
  deriving (Eq, Show, Read, Data, Typeable)

data Scores a = Scores { unScores :: [a] }
  deriving (Eq, Show, Read, Data, Typeable)

mkScores :: a -> Scores a
mkScores = Scores . (:[])

scoresCata
  score
  scores
  s = case s of
    Scores [x] -> score x
    Scores xs  -> scores xs

data Binary = Binary Result
  deriving (Eq, Show, Read, Data, Typeable)

binaryCata f (Binary x) = f x

data Percentage = Percentage (Scores Double)
  deriving (Eq, Show, Read, Data, Typeable)

percentageCata f (Percentage x) = f x

data FreeForm = FreeForm String
  deriving (Eq, Show, Read, Data, Typeable)

freeForm
  free
  x = case x of
    FreeForm f -> free f

data EvResult = EvResult {
    evResult :: EvaluationData Binary Percentage FreeForm
  } deriving (Eq, Show, Read, Data, Typeable)

evResultCata
  binary
  percentage
  freeForm
  e = case e of
    (EvResult (BinEval b)) -> binary b
    (EvResult (PctEval p)) -> percentage p
    (EvResult (FreeEval f)) -> freeForm f

withEvResult result binary percentage freeForm
  = evResultCata binary percentage freeForm result

percentageResult :: Double -> EvResult
percentageResult d = EvResult (PctEval (Percentage (Scores { unScores = [ d ]})))

percentValue :: EvResult -> Maybe Double
percentValue (EvResult (PctEval (Percentage (Scores [p])))) = Just p
percentValue _ = Nothing

binaryResult :: Result -> EvResult
binaryResult r = EvResult (BinEval (Binary r))

freeFormResult :: String -> EvResult
freeFormResult = EvResult . FreeEval . FreeForm

data EvConfig = EvConfig {
    evConfig :: EvaluationData () Double ()
  } deriving (Eq, Show, Read, Data, Typeable)

evConfigCata
  binary
  percentage
  freeForm
  e = case e of
    (EvConfig (BinEval ())) -> binary
    (EvConfig (PctEval p))  -> percentage p
    (EvConfig (FreeEval ())) -> freeForm

withEvConfig e binary percentage freeForm
  = evConfigCata binary percentage freeForm e

percentageConfig :: Double -> EvConfig
percentageConfig = EvConfig . PctEval

binaryConfig :: EvConfig
binaryConfig = EvConfig (BinEval ())

freeFormConfig :: EvConfig
freeFormConfig = EvConfig (FreeEval ())

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
