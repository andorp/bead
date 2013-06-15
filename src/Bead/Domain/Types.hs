module Bead.Domain.Types where

import Data.Char (isSpace)
import Data.Time.Clock (UTCTime)

import Control.Monad (join)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Error

type Erroneous a = Either String a

class Str s where
  str :: s -> String

readMsg :: (Read a) => String -> String -> a
readMsg msg s =
  case readMaybe s of
    Nothing -> error $ join ["No read: ", msg, "'", s, "'"]
    Just  x -> x

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

newtype EncryptKey = EncryptKey String
  deriving (Eq)

newtype Encrypted = Encrypted String
  deriving (Eq)

data Encryption e = Encryption {
    encrpyt :: EncryptKey -> e -> Encrypted
  , decrypt :: Encrypted -> EncryptKey -> Erroneous e
  }
