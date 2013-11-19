{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionDetails (
    submissionDetails
  ) where

import Data.Time (UTCTime, getCurrentTime)
import Data.List (intersperse)
import Data.String (fromString)
import Control.Monad (liftM)
import Control.Applicative ((<*>))

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(SubmissionDetails))
import Bead.Controller.UserStories (submissionDetailsDesc)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Content.Utils
import Bead.View.Snap.Content.Comments
import Bead.Domain.Entities (Comment(..))
import Bead.Domain.Relationships


import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (class_, style)

submissionDetails :: Content
submissionDetails = getPostContentHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = withUserState $ \s -> do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  usersSubmission ak sk $ \submission -> do
    case submission of
      Nothing -> renderPagelet . withUserFrame s $ invalidSubmission
      Just sm -> do
        sd <- runStoryE $ submissionDetailsDesc sk
        renderPagelet . withUserFrame s $
          submissionDetailsContent PageData {
              smKey = sk
            , aKey  = ak
            , smDetails = sd
            }

submissionDetailsPostHandler :: POSTContentHandler
submissionDetailsPostHandler = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  c  <- getParameter (stringParameter (fieldName commentValueField) "Comment")
  now <- liftIO $ getCurrentTime
  usersSubmission ak sk $ \s -> do
    return $ case s of
      Nothing -> LogMessage "Submission does not belong to the user"
      Just _  -> SubmissionComment sk Comment {
                     comment = c
                   , commentDate = now
                   }

submissionDetailsContent :: PageData -> Pagelet
submissionDetailsContent p = onlyHtml $ mkI18NHtml $ \i -> do
  let sm = smDetails p
  H.p $ do
    H.h4 (translate i "Group / Course: ")
    (fromString . sdGroup $ sm)
  H.p $ do
    H.h4 (translate i "Teacher: ")
    (fromString . join . intersperse ", " . sdTeacher $ sm)
  H.h2 $ (translate i "Assignment: ")
  H.div # assignmentTextDiv $ H.pre # assignmentTextPre $ fromString . sdAssignment $ sm
  H.p $ do
    H.h4 (translate i "Status: ")
    (fromString . sdStatus $ sm)
  H.p $ do
    H.h4 (translate i "Submission text: ")
    H.div # submissionTextDiv $ H.pre # submissionTextPre $ fromString . sdSubmission $ sm
  H.p $ do
    H.h4 (translate i "New comment")
    postForm (routeOf P.SubmissionDetails) $ do
      H.div ! formDiv $ do
        textAreaInput (fieldName commentValueField) Nothing ! fillDiv
        hiddenInput (fieldName assignmentKeyField) (paramValue . aKey  $ p)
        hiddenInput (fieldName submissionKeyField) (paramValue . smKey $ p)
      submitButton (fieldName commentBtn) (i "Comment")
  translate i . commentsDiv . sdComments $ sm

invalidSubmission :: Pagelet
invalidSubmission = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "You have tried to open a submission that not belongs to you")

-- CSS Section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"

