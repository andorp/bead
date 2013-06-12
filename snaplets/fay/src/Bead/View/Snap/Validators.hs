module Bead.View.Snap.Validators where

username :: String -> Bool
username = not . null

password :: String -> Bool
password = (>4) . length

