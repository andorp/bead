{-# LANGUAGE CPP #-}
module Bead.View.DataDir (referenceDataDir) where

#ifndef GHCI
import Paths_Bead (getDataDir)
#endif

referenceDataDir = dir

#ifndef GHCI
dir = getDataDir
#else
dir = error "Snaplet initialized within GHCI"
#endif

