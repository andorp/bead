module Bead.Domain.Relationships.SubmissionInfo (
    displayableFreeFormResultLength
  , submissionInfo
  ) where

import Bead.Domain.Shared.Evaluation
import Bead.Domain.Relationships (submissionInfoCata)


displayableFreeFormResultLength :: Int
displayableFreeFormResultLength = 6

formatFreeFormText :: String -> String -> String
formatFreeFormText placeholder msg =
  if length msg >= displayableFreeFormResultLength
    then placeholder
    else msg

-- Uses the given placeholder string if the length
-- of the free format value is longer than the maximum.
formatFreeForm :: String -> FreeForm -> String
formatFreeForm placeholder = freeForm $ formatFreeFormText placeholder

evaluationResult
  accepted
  rejected
  percentage percentagesError
  freeFormat placeholder
  = evResultCata bin pct free
    where
      bin  = binaryCata (resultCata accepted rejected)
      pct  = percentageCata (scoresCata percentage percentagesError)
      free f = freeFormat (freeForm id f) (formatFreeForm placeholder f)

submissionInfo
  notFound
  unevaluated
  tested
  accepted
  rejected
  percentage percentagesError
  freeFormat placeholder
  = submissionInfoCata
      notFound
      unevaluated
      tested
      (const (evaluationResult
                accepted
                rejected
                percentage percentagesError
                freeFormat placeholder))
