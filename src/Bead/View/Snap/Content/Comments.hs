{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  ) where

import Data.String

import Bead.Domain.Entities (Comment(..))
import Bead.View.Snap.Content (I18NHtml, mkI18NHtml, joinHtml)

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

commentsDiv :: [Comment] -> I18NHtml
commentsDiv cs = mkI18NHtml $ \i -> do
  H.div ! A.id "comments" $ do
    H.p (joinHtml i "Comments")
    mapM_ commentPar cs

commentPar :: Comment -> Html
commentPar c = H.div $ do
  H.p . fromString . show . commentDate $ c
  H.p . fromString . comment $ c
