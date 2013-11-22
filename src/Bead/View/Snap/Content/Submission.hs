{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  ) where

import Data.List (intersperse)
import Data.Time
import Data.String (fromString)
import Control.Monad (liftM)
import Control.Applicative ((<*>))

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (userAssignmentKeys, loadAssignment)
import Bead.Controller.Pages as P (Page(Submission))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Markdown (markdownToHtml)
import Bead.View.Snap.Content.Utils
import qualified Bead.Domain.Entities as E

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

submission :: Content
submission = getPostContentHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  }

submissionPage :: GETContentHandler
submissionPage = withUserState $ \s -> do
  let render p = renderPagelet $ withUserFrame s p
  ak <- getParameter assignmentKeyPrm
  userAssignmentForSubmission
    ak
    (\desc asg -> render $ submissionContent (PageData { asKey = ak, asValue = asg, asDesc = desc }))
    (render invalidAssignment)

submissionPostHandler :: POSTContentHandler
submissionPostHandler =
  NewSubmission
    <$> getParameter assignmentKeyPrm
    <*> (E.Submission
           <$> getParameter (stringParameter (fieldName submissionTextField) "Submission text")
           <*> liftIO getCurrentTime)

submissionContent :: PageData -> Pagelet
submissionContent p = onlyHtml $ mkI18NHtml $ \i -> do
  postForm (routeOf P.Submission) $ H.div ! formDiv $ do
    H.div ! title $ H.p $ H.b $ (translate i "Solution")
    H.div ! leftInput $ do
      textAreaInput (fieldName submissionTextField) Nothing ! fillDiv
    H.div ! rightInfo $ do
      H.p $ do
        H.table $ do
          H.tr $ do
            H.td $ H.b $ (translate i "Course: ")
            H.td $ (fromString . aGroup $ asDesc p)
          H.tr $ do
            H.td $ H.b $ (translate i "Teacher: ")
            H.td $ (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
          H.tr $ do
            H.td $ H.b $ (translate i "Assignment: ")
            H.td $ (fromString . assignmentName . asValue $ p)
        H.br
        submitButton (fieldName submitSolutionBtn) (i "Submit")
    hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
  H.h2 (translate i "Description")
  H.div # assignmentTextDiv $ H.pre # assignmentTextPre $
    markdownToHtml . assignmentDesc . asValue $ p

invalidAssignment :: Pagelet
invalidAssignment = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "You have tried to open an assignment that not belongs to you")

-- CSS Section

formDiv = A.style "width: 100%; height: 300px"
title   = A.style "float: left; width: 100%"
leftInput = A.style "float: left; width: 58%; height: 80%"
rightInfo = A.style "float: right; width: 39% height: 80%"
fillDiv   = A.style "width: 98%; height: 98%"
