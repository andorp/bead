module Bead.Domain.Types where

import Data.Time.Clock (UTCTime)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Error

type Erroneous a = Either String a

data Date = Date {
    utcTime :: UTCTime
  } deriving (Eq, Ord)

instance Show Date where
  show _ = "TODO: SHOW DATE"

class Str s where
  str :: s -> String

newtype EncryptKey = EncryptKey String
  deriving (Eq)

newtype Encrypted = Encrypted String
  deriving (Eq)

data Encryption e = Encryption {
    encrpyt :: EncryptKey -> e -> Encrypted
  , decrypt :: Encrypted -> EncryptKey -> Erroneous e
  }

