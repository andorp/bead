{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  ) where

import Data.String

import Bead.Domain.Entities (Comment(..))

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

commentsDiv :: [Comment] -> Html
commentsDiv cs = H.div ! A.id "comments" $ do
  H.p "Comments"
  mapM_ commentPar cs

commentPar :: Comment -> Html
commentPar c = H.div $ do
  H.p . fromString . show . commentDate $ c
  H.p . fromString . comment $ c
