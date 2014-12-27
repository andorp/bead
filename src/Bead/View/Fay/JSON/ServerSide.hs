module Bead.View.Fay.JSON.ServerSide (
    decodeFromFay
  , encodeToFay
  , module Data.Data
  ) where

import Data.Data
import Data.Aeson (encode, decode)
import Fay.Convert
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Monad ((<=<))

-- | Produce 'Just a' if the given string represents an 'a' JSON object
decodeFromFay :: (Data a) => String -> Maybe a
decodeFromFay = readFromFay <=< (decode . pack)

-- | Produces 'Just String' representation if the given value is a JSON represatble
encodeToFay :: (Show a, Data a) => a -> Maybe String
encodeToFay = fmap (unpack . encode) . showToFay
