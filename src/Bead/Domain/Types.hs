module Bead.Domain.Types where

import Data.Char (isSpace)

type Erroneous a = Either String a

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
  case reads s of
    [(x,cs)] -> emptyEnding (x,cs)
    _        -> Nothing
  where
    emptyEnding (x,cs)
      | all isSpace cs = Just x
      | otherwise      = Nothing

newtype FileName = FileName String
  deriving (Show, Eq)

fileName :: FileName -> String
fileName (FileName f) = f
