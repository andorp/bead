module Data.AndOr where

data AndOr a b
  = And b [AndOr a b]
  | Or  b [AndOr a b]
  | Leaf a
  deriving (Show, Eq)

map :: (a -> a') -> (b -> b') -> AndOr a b -> AndOr a' b'
map = undefined

