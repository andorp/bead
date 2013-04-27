{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  ) where

import Data.Time
import Data.String (fromString)
import Control.Monad (liftM)
import Control.Applicative ((<*>))

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (userAssignmentKeys, loadAssignment)
import Bead.Controller.Pages as P (Page(Submission))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Content.Utils
import qualified Bead.Domain.Entities as E

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

submission :: Content
submission = getPostContentHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  }

submissionPage :: GETContentHandler
submissionPage = withUserStateE $ \s -> do
  ak <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  usersAssignment ak $ \assignment ->
    blaze . withUserFrame s $
      case assignment of
        Nothing -> invalidAssignment
        Just a  -> submissionContent (PageData { asKey = ak, asValue = a })

submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  submissionText <- getParamE (fieldName submissionTextField) id "Submission text was not found"
  assignmentKey  <- getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"
  now <- liftIO getCurrentTime
  return $ NewSubmission assignmentKey (E.Submission submissionText now)

submissionContent :: PageData -> Html
submissionContent p = postForm (routeOf P.Submission) $ do
  H.p $ do
    "Solution text box / Solution files"
    textAreaInput (fieldName submissionTextField) 300 200 Nothing
  H.p $ do
    "Description of the Assignment"
    (fromString (assignmentDesc (asValue p)))
  H.p $ "Course / Group / Teacher / Assignment Information"
  hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
  submitButton (fieldName submitSolutionBtn) "Submit"

invalidAssignment :: Html
invalidAssignment = "You have tried to open an assignment that not belongs to you"

