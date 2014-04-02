{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateCourse (
    createCourse
  ) where

import Bead.View.Snap.Content
import qualified Bead.View.UserActions as UA

createCourse :: ModifyHandler
createCourse = ModifyHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourse <$> getValue
