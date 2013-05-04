module Data.AndOr where

import Prelude hiding (map)
import qualified Data.List as L
import Control.Monad (mapM)

data AndOr a
  = And  a [AndOr a]
  | Or   a [AndOr a]
  | Leaf a
  deriving (Show, Eq)

map :: (a -> b) -> AndOr a -> AndOr b
map f (Leaf a)   = (Leaf . f $ a)
map f (And b ts) = And (f b) (L.map (map f) ts)
map f (Or  b ts) = Or  (f b) (L.map (map f) ts)

fold :: (a -> c -> b)
     -> (a -> c -> b)
     -> (a -> b)
     -> ([b] -> c)
     -> ([b] -> c)
     -> AndOr a -> b
fold _  _  fl _  _  (Leaf x)   = fl x
fold fa fo fl ga go (And x ts) = fa x (ga (L.map (fold fa fo fl ga go) ts))
fold fa fo fl ga go (Or  x ts) = fo x (go (L.map (fold fa fo fl ga go) ts))

foldM
  :: (Monad m)
  => (a -> c -> m b)
  -> (a -> c -> m b)
  -> (a -> m b)
  -> ([b] -> m c)
  -> ([b] -> m c)
  -> AndOr a -> m b
foldM _  _  fl _  _  (Leaf x)   = fl x
foldM fa fo fl ga go (And x ts) = (fa x) =<< ga =<< (mapM (foldM fa fo fl ga go) ts)
foldM fa fo fl ga go (Or  x ts) = (fo x) =<< go =<< (mapM (foldM fa fo fl ga go) ts)
