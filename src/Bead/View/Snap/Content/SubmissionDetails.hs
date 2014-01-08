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
import qualified Text.Blaze.Html5.Attributes as A (class_, style)
import qualified Bead.View.Snap.I18NHtml as H

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

submissionDetailsContent :: PageData -> Pagelet
submissionDetailsContent p = onlyHtml $ do
  let sm = smDetails p
      tc = uTime p
  H.table $ do
    H.tr $ do
      H.td # textAlignRight $ H.b $ "Tárgy, csoport:"
      H.td $ fromString (sdGroup $ sm)
    H.tr $ do
      H.td # textAlignRight $ H.b $ "Oktató:"
      H.td $ fromString (join . intersperse ", " . sdTeacher $ sm)
    H.tr $ do
      H.td # textAlignRight $ H.b $ "Feladat:"
      H.td $ fromString (assignmentName . sdAssignment $ sm)
    H.tr $ do
      H.td # textAlignRight $ H.b $ "Határidő:"
      H.td $ fromString (showDate . tc . assignmentEnd $ sdAssignment sm)
  H.h2 $ "Részletes leírás"
  H.div # assignmentTextDiv $ fromString . assignmentDesc $ sdAssignment $ sm
  H.h2 "Megoldás szövege"
  H.div # submissionTextDiv $ H.pre # submissionTextPre $ fromString . sdSubmission $ sm
  H.h2 "Értékelés"
  (fromString $ sdStatus sm)
  let studentComments = filter isStudentComment $ sdComments sm
  when (not $ null studentComments) $ do
    commentsDiv tc studentComments
  H.hr
  H.h2 "Új hozzászólás"
  postForm (routeOf $ P.SubmissionDetails (aKey p) (smKey p)) $ do
    H.div ! formDiv $ do
      textAreaInput (fieldName commentValueField) Nothing ! fillDiv
    submitButton (fieldName commentBtn) "Beküld"

invalidSubmission :: Pagelet
invalidSubmission = onlyHtml $ "Olyan megoldást próbáltál megnyitni, amelyik nem hozzád tartozik!"

-- CSS Section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"

