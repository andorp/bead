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
import Bead.View.Snap.Markdown
import Bead.View.Snap.Content.Utils
import Bead.View.Snap.Content.Comments
import Bead.Domain.Entities (Comment(..))
import Bead.Domain.Relationships


import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as A (class_, style)
import qualified Text.Blaze.Html5 as H

submissionDetails :: Content
submissionDetails = getPostContentHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  , uTime :: UserTimeConverter
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = withUserState $ \s -> do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  usersSubmission ak sk $ \submission -> do
    case submission of
      Nothing -> renderPagelet . withUserFrame s $ invalidSubmission
      Just sm -> do
        sd <- userStory $ submissionDetailsDesc sk
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
  c  <- getParameter (stringParameter (fieldName commentValueField) "Hozzászólás")
  now <- liftIO $ getCurrentTime
  usersSubmission ak sk $ \s -> do
    return $ case s of
      Nothing -> LogMessage "Submission does not belong to the user"
      Just _  -> SubmissionComment sk Comment {
                     comment = c
                   , commentDate = now
                   , commentType = CT_Student
                   }

submissionDetailsContent :: PageData -> IHtml
submissionDetailsContent p = do
  let sm = smDetails p
      tc = uTime p
  msg <- getI18N
  return $ do
    H.table $ do
      H.tr $ do
        H.td # textAlignRight $ H.b $ (fromString . msg $ Msg_SubmissionDetails_Course "Tárgy, csoport:")
        H.td $ fromString (sdGroup $ sm)
      H.tr $ do
        H.td # textAlignRight $ H.b $ (fromString . msg $ Msg_SubmissionDetails_Admins "Oktató:")
        H.td $ fromString (join . intersperse ", " . sdTeacher $ sm)
      H.tr $ do
        H.td # textAlignRight $ H.b $ (fromString . msg $ Msg_SubmissionDetails_Assignment "Feladat:")
        H.td $ fromString (assignmentName . sdAssignment $ sm)
      H.tr $ do
        H.td # textAlignRight $ H.b $ (fromString . msg $ Msg_SubmissionDetails_Deadline "Határidő:")
        H.td $ fromString (showDate . tc . assignmentEnd $ sdAssignment sm)
    H.h2 . fromString . msg $ Msg_SubmissionDetails_Description "Részletes leírás"
    H.div # assignmentTextDiv $
      markdownToHtml . assignmentDesc . sdAssignment $ sm
    H.h2 . fromString . msg $ Msg_SubmissionDetails_Solution "Megoldás szövege"
    H.div # submissionTextDiv $ H.pre # submissionTextPre $ fromString . sdSubmission $ sm
    H.h2 . fromString . msg $ Msg_SubmissionDetails_Evaluation "Értékelés"
    (fromString $ sdStatus sm)
    let studentComments = filter isStudentComment $ sdComments sm
    when (not $ null studentComments) $ do
      i18n msg $ commentsDiv tc studentComments
    H.hr
    H.h2 (fromString . msg $ Msg_SubmissionDetails_NewComment "Új hozzászólás")
    postForm (routeOf $ P.SubmissionDetails (aKey p) (smKey p)) $ do
      H.div ! formDiv $ do
        textAreaInput (fieldName commentValueField) Nothing ! fillDiv
      submitButton (fieldName commentBtn) (msg $ Msg_SubmissionDetails_SubmitComment "Beküld")

invalidSubmission :: IHtml
invalidSubmission = do
  msg <- getI18N
  return . fromString . msg $
    Msg_SubmissionDetails_InvalidSubmission "Olyan megoldást próbáltál megnyitni, amelyik nem hozzád tartozik!"

-- CSS Section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"

