{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Comments (
    commentsDiv
  , commentPostForm
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.String

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Controller.Pages as Pages
import           Bead.View.Snap.Content
import           Bead.View.Snap.Content.SeeMore

commentsDiv :: UserTimeConverter -> [Comment] -> IHtml
commentsDiv t cs = do
  msg <- getI18N
  return $ H.div ! A.id "comments" $ do
    mapM_ (commentPar msg t) $ sortBy ((flip compare) `on` commentDate) cs

commentPar :: I18N -> UserTimeConverter -> Comment -> Html
commentPar i18n t c = H.div # (commentDiv c) $ do
  H.p # textAlign "left" $
    fromString $ (showDate . t . commentDate $ c) ++ ", " ++ (commentAuthor $ c)
  seeMorePre i18n maxLength maxLines (comment c)
  where
    commentAuthor = commentCata $ \_comment author _date ->
      commentTypeCata
        author -- student
        author -- groupAdmin
        author -- courseAdmin
        author -- admin
        author -- evaluation
        (i18n $ Msg_Comments_AuthorTestScript_Private "Test Script (seen by only admins)") -- test agent
        (i18n $ Msg_Comments_AuthorTestScript_Public "Test Script") -- message

    commentDiv = commentCata $ \_comment _author _date ->
      commentTypeCata
        commentTextDiv -- student
        commentTextDiv -- groupAdmin
        commentTextDiv -- courseAdmin
        commentTextDiv -- admin
        commentTextDiv -- evaluation
        commentTextDiv -- testAgent
        messageCommentTextDiv -- message

    maxLength = 100
    maxLines = 5

-- Creates a post form for the given route assignment key and submission key, where
-- a comment can be placed and the result is submitted to the given page, which is
-- need to understand the given parameters
commentPostForm :: Page a -> AssignmentKey -> IHtml
commentPostForm p ak = do
  msg <- getI18N
  return $ postForm (routeOf p) $ do
    H.div ! formDiv $ do
      textAreaInput (fieldName commentValueField) Nothing ! fillDiv ! A.required ""
      hiddenInput (fieldName assignmentKeyField) (paramValue ak)
    H.br
    submitButton (fieldName commentBtn) (msg $ Msg_Comments_SubmitButton "Submit")

-- * CSS section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"
