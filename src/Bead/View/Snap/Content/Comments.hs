{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  , commentPostForm
  ) where

import Data.String
import Data.Time (UTCTime, LocalTime)

import Bead.Domain.Entities (Comment(..))
import Bead.Controller.Pages (Page(..))
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

commentsDiv :: UserTimeConverter -> [Comment] -> I18NHtml
commentsDiv t cs = mkI18NHtml $ \i -> do
  H.div ! A.id "comments" $ do
    H.h2 (translate i "Hozzászólások")
    mapM_ (commentPar t) cs

commentPar :: UserTimeConverter -> Comment -> Html
commentPar t c = H.div # commentTextDiv $ do
  H.p # textAlign "left" $ fromString . showDate . t . commentDate $ c
  H.pre # commentTextPre $ fromString . comment $ c

-- Creates a post form for the given route assignment key and submission key, where
-- a comment can be placed and the result is submitted to the given page, which is
-- need to understand the given parameters
commentPostForm :: Page -> AssignmentKey -> I18NHtml
commentPostForm p ak = mkI18NHtml $ \i -> postForm (routeOf p) $ do
  H.div ! formDiv $ do
    textAreaInput (fieldName commentValueField) Nothing ! fillDiv
    hiddenInput (fieldName assignmentKeyField) (paramValue ak)
  submitButton (fieldName commentBtn) (i "Beküld")

-- * CSS section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"
