{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  ) where

import Data.String
import Data.Time (UTCTime, LocalTime)

import Bead.Domain.Entities (Comment(..))
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

commentsDiv :: UserTimeConverter -> [Comment] -> I18NHtml
commentsDiv t cs = mkI18NHtml $ \i -> do
  H.div ! A.id "comments" $ do
    H.h2 (translate i "Comments")
    mapM_ (commentPar t) cs

commentPar :: UserTimeConverter -> Comment -> Html
commentPar t c = H.div # commentTextDiv $ do
  H.p # textAlign "left" $ fromString . showDate . t . commentDate $ c
  H.pre # commentTextPre $ fromString . comment $ c
