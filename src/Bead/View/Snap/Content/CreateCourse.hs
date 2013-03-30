{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CreateCourse (
    createCourse
  ) where

import Bead.View.Snap.Content
import qualified Bead.View.UserActions as UA

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

createCourse :: Content
createCourse = postContentHandler submitCourse

submitCourse :: POSTContentHandler
submitCourse = do
  course <- getValue
  return . UA.CreateCourse $ course
