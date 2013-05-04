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
import Bead.Domain.Entities (Comment(..))
import Bead.Domain.Relationships

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

submissionDetails :: Content
submissionDetails = getPostContentHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = withUserStateE $ \s -> do
  ak <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key was not found"
  usersSubmission ak sk $ \submission -> do
    case submission of
      Nothing -> blaze . withUserFrame s $ invalidSubmission
      Just sm -> do
        sd <- runStoryE $ submissionDetailsDesc sk
        blaze . withUserFrame s $
          submissionDetailsContent PageData {
              smKey = sk
            , aKey  = ak
            , smDetails = sd
            }

submissionDetailsPostHandler :: POSTContentHandler
submissionDetailsPostHandler = do
  ak <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  sk <- getParamE (fieldName submissionKeyField) SubmissionKey "Submission key was not found"
  c  <- getParamE (fieldName commentValueField) id "Comment was not found"
  now <- liftIO $ getCurrentTime
  usersSubmission ak sk $ \s -> do
    return $ case s of
      Nothing -> LogMessage "Submission does not belong to the user"
      Just _  -> SubmissionComment sk Comment {
                     comment = c
                   , commentDate = now
                   }

submissionDetailsContent :: PageData -> Html
submissionDetailsContent p = do
  let sm = smDetails p
  H.p $ do
    "Group / Course"
    (fromString . sdGroup $ sm)
  H.p $ do
    "Teacher"
    (fromString . join . intersperse ", " . sdTeacher $ sm)
  H.p $ do
    "Assignment"
    (fromString . sdAssignment $ sm)
  H.p $ do
    "Status"
    (fromString . sdStatus $ sm)
  H.p $ do
    "Submission text"
    (fromString . sdSubmission $ sm)
  H.p $ do
    "New comment"
    postForm (routeOf P.SubmissionDetails) $ do
      textAreaInput (fieldName commentValueField) 50 10 Nothing
      hiddenInput (fieldName assignmentKeyField) (paramValue . aKey  $ p)
      hiddenInput (fieldName submissionKeyField) (paramValue . smKey $ p)
      submitButton (fieldName commentBtn) "Comment"
  H.p $ do
    "Comments"
    (mapM_ fromString (sdComments sm))

invalidSubmission :: Html
invalidSubmission = "You have tried to open a submission that not belongs to you"

