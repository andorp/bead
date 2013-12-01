{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  ) where

import Data.String

import Bead.Domain.Entities (Comment(..))
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

commentsDiv :: [Comment] -> I18NHtml
commentsDiv cs = mkI18NHtml $ \i -> do
  H.div ! A.id "comments" $ do
    H.h2 (translate i "Comments")
    mapM_ commentPar cs

commentPar :: Comment -> Html
commentPar c = H.div # commentTextDiv $ do
  H.p # textAlign "left" $ fromString . showDate . commentDate $ c
  H.pre # commentTextPre $ fromString . comment $ c
