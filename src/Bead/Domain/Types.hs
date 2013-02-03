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


liftE :: (Monad m, Error e) => m (Erroneous a) -> ErrorT e m a
liftE em
  = do x <- lift em
       case x of
         Left msg -> throwError $ strMsg msg
         Right x' -> return x'

firstError :: [Erroneous a] -> Erroneous a
firstError ((Left msg):_) = Left msg
firstError [Right x]    = Right x
firstError (_:es) = firstError es

unsafeFirstError :: [Erroneous a] -> Erroneous b
unsafeFirstError ((Left msg):_) = Left msg
unsafeFirstError [Right _]    = Right (error "unsafeFirstError: Right was found")
unsafeFirstError (_:es) = unsafeFirstError es

forgetVal :: Erroneous a -> Erroneous ()
forgetVal (Left msg) = Left msg
forgetVal (Right _)  = Right ()

