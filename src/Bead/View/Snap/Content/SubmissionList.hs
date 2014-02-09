{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionList (
    submissionList
  ) where

import Data.Time
import Data.String (fromString)
import Control.Monad (join, liftM)
import Control.Applicative ((<*>))

import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionListDesc)
import Bead.Controller.Pages as P (Page(SubmissionList, SubmissionDetails))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.Content.Submission (resolveStatus)
import Bead.View.Snap.Markdown
import Bead.View.Snap.Content.Utils
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A (class_)
import qualified Text.Blaze.Html5 as H

submissionList :: Content
submissionList = getContentHandler submissionListPage

data PageData = PageData {
    asKey :: AssignmentKey
  , smList :: SubmissionListDesc
  , uTime :: UserTimeConverter
  }

submissionListPage :: GETContentHandler
submissionListPage = withUserState $ \s -> do
  ak <- getParameter assignmentKeyPrm
  usersAssignment ak $ \assignment -> do
    case assignment of
      Nothing -> renderPagelet . withUserFrame s $ invalidAssignment
      Just asg -> do
        now <- liftIO getCurrentTime
        case (assignmentStart asg > now) of
          True  -> renderPagelet . withUserFrame s $ assignmentNotStartedYet
          False -> do
            sl <- userStory (submissionListDesc ak)
            tc <- usersTimeZoneConverter
            renderPagelet . withUserFrame s .
              submissionListContent $
                PageData { asKey = ak
                         , smList = sortSbmListDescendingByTime sl
                         , uTime = tc
                         }

submissionListContent :: PageData -> IHtml
submissionListContent p = do
  msg <- getI18N
  return $ do
    H.div ! A.class_ (className submissionListDiv) $ do
    H.table $ do
      H.tr $ do
        firstCol  (msg $ Msg_SubmissionList_CourseOrGroup "Tárgy, csoport:")
        secondCol (slGroup . smList $ p)
      H.tr $ do
        firstCol  (msg $ Msg_SubmissionList_Admin "Oktató:")
        secondCol (join . slTeacher . smList $ p)
      H.tr $ do
        firstCol  (msg $ Msg_SubmissionList_Assignment "Feladat:")
        secondCol (assignmentName . slAssignment . smList $ p)
      H.tr $ do
        firstCol  (msg $ Msg_SubmissionList_Deadline "Határidő:")
        secondCol (showDate . (uTime p) . assignmentEnd . slAssignment $ smList p)
    H.h2 . fromString . msg $ Msg_SubmissionList_Description "Részletes leírás"
    H.div # assignmentTextDiv $
      (markdownToHtml . assignmentDesc . slAssignment . smList $ p)
    let submissions = slSubmissions . smList $ p
    H.h2 . fromString . msg $ Msg_SubmissionList_SubmittedSolutions "Beadott megoldások"
    either (userSubmissionTimes msg) (userSubmissionInfo msg) submissions
  where
    firstCol  t = H.td # textAlignRight $ H.b $ fromString t
    secondCol t = H.td # textAlignLeft $ fromString t

    submissionLine msg (sk, time, status, t) = H.tr $ do
      H.td # informationalCell $ linkWithText
        (routeOf $ P.SubmissionDetails (asKey p) sk)
        (fromString . showDate $ (uTime p) time)
      H.td # informationalCell $ (resolveStatus msg status)

    submissionTimeLine time =
      H.tr $ (H.td # informationalCell) $ fromString $ showDate $ (uTime p) time

    userSubmissionInfo  msg = userSubmission msg (submissionLine msg)
    userSubmissionTimes msg = userSubmission msg submissionTimeLine

    userSubmission msg line submissions =
      if (not $ null submissions)
        then do
          H.p $ fromString . msg $ Msg_SubmissionList_Info
            "A beadott megoldásokhoz a beadás után még hozzászólások írhatóak."
          table (fieldName submissionTableName) (className submissionListTable) # informationalTable $
            mapM_ line submissions
        else do
          (fromString . msg $ Msg_SubmissionList_NoSubmittedSolutions "Nincsenek még beadott megoldások.")

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return . fromString . msg $ Msg_SubmissionList_NonAssociatedAssignment "Olyan feladatot próbáltál megnyitni, amelyik nem hozzád tartozik!"

assignmentNotStartedYet :: IHtml
assignmentNotStartedYet = do
  msg <- getI18N
  return . fromString . msg $ Msg_SubmissionList_NonReachableAssignment "Olyan feladatot próbáltál megnyitni, amelyik nem érhető el!"
