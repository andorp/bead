{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateGroup (
    createGroup
  ) where

import Bead.View.Snap.Content

createGroup :: Content
createGroup = postContentHandler submitGroup

submitGroup :: POSTContentHandler
submitGroup = do
  courseKey <- getValue
  group     <- getValue
  return $ UA.CreateGroup courseKey group

