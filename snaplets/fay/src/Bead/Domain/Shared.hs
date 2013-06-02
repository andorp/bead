module Bead.Domain.Shared where

{- Shared data structures between Client and Server -}

data EvaulationData b p
  = BinEval b
  | PctEval p
  deriving (Eq, Show, Read)

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

