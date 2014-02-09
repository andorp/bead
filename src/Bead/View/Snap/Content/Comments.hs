{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  , commentPostForm
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.String
import Data.Time (UTCTime, LocalTime)

import Bead.Domain.Entities (Comment(..))
import Bead.Controller.Pages (Page(..))
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Bead.View.Snap.I18N (IHtml)

commentsDiv :: UserTimeConverter -> [Comment] -> IHtml
commentsDiv t cs = do
  msg <- getI18N
  return $ H.div ! A.id "comments" $ do
    mapM_ (commentPar t) $ sortBy ((flip compare) `on` commentDate) cs

commentPar :: UserTimeConverter -> Comment -> Html
commentPar t c = H.div # commentTextDiv $ do
  H.p # textAlign "left" $ fromString $
    (showDate . t . commentDate $ c) ++ ", " ++ (commentAuthor $ c)
  H.pre # commentTextPre $ fromString $ comment $ c

-- Creates a post form for the given route assignment key and submission key, where
-- a comment can be placed and the result is submitted to the given page, which is
-- need to understand the given parameters
commentPostForm :: Page -> AssignmentKey -> IHtml
commentPostForm p ak = do
  msg <- getI18N
  return $ postForm (routeOf p) $ do
    H.div ! formDiv $ do
      textAreaInput (fieldName commentValueField) Nothing ! fillDiv ! A.required ""
      hiddenInput (fieldName assignmentKeyField) (paramValue ak)
    H.br
    submitButton (fieldName commentBtn) (msg $ Msg_Comments_SubmitButton "Beküld")

-- * CSS section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"
