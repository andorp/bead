module Bead.Domain.Func where

{-
Helper functions for constant functions and
selecting parameters from multiparameter functions
-}

const2 c = \_ _ -> c
const3 c = \_ _ _ -> c
const4 c = \_ _ _ _ -> c
const5 c = \_ _ _ _ _ -> c
const6 c = \_ _ _ _ _ _ -> c
const7 c = \_ _ _ _ _ _ _ -> c
const8 c = \_ _ _ _ _ _ _ _ -> c

p_1_2 x _ = x
p_2_2 _ x = x

bool true false x =
  if x then true else false
