{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionDetails (
    submissionDetails
  ) where

import Data.Time (UTCTime, LocalTime, getCurrentTime)
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
  , uTime :: UTCTime -> LocalTime
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
        tc <- usersTimeZoneConverter
        renderPagelet . withUserFrame s $
          submissionDetailsContent PageData {
              smKey = sk
            , aKey  = ak
            , smDetails = sd
            , uTime = tc
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
      tc = uTime p
  H.table $ do
    H.tr $ do
      H.td # textAlignRight $ H.b $ fromString (i "Group / Course:")
      H.td $ fromString (sdGroup $ sm)
    H.tr $ do
      H.td # textAlignRight $ H.b $ fromString (i "Teacher:")
      H.td $ fromString (join . intersperse ", " . sdTeacher $ sm)
    H.tr $ do
      H.td # textAlignRight $ H.b $ fromString (i "Assignment:")
      H.td $ fromString (assignmentName . sdAssignment $ sm)
  H.h2 $ (translate i "Assignment Text")
  H.div # assignmentTextDiv $ fromString . assignmentDesc $ sdAssignment $ sm
  H.h2 (translate i "Submission Text")
  H.div # submissionTextDiv $ H.pre # submissionTextPre $ fromString . sdSubmission $ sm
  H.h2 $ (translate i "Evaluation")
  (fromString . sdStatus $ sm)
  when (not . null $ sdComments sm) $ do
    translate i . commentsDiv tc $ sdComments sm
  H.hr
  H.h2 (translate i "New comment")
  postForm (routeOf P.SubmissionDetails) $ do
    H.div ! formDiv $ do
      textAreaInput (fieldName commentValueField) Nothing ! fillDiv
      hiddenInput (fieldName assignmentKeyField) (paramValue . aKey  $ p)
      hiddenInput (fieldName submissionKeyField) (paramValue . smKey $ p)
    submitButton (fieldName commentBtn) (i "Submit")

invalidSubmission :: Pagelet
invalidSubmission = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "You have tried to open a submission that not belongs to you")

-- CSS Section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"

