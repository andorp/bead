{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Style where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

centerTable = A.style "margin-left: auto; margin-right: auto"

textAlignLeft  = A.style "text-align: left"
textAlignRight = A.style "text-align: right"

