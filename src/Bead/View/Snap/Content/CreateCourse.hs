{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateCourse (
    createCourse
  ) where

import Bead.View.Snap.Content
import qualified Bead.View.UserActions as UA

createCourse :: Content
createCourse = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = UA.CreateCourse <$> getValue
