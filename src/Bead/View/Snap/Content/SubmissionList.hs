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
import Bead.View.Snap.Markdown
import Bead.View.Snap.Content.Utils
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (class_)

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

submissionListContent :: PageData -> Pagelet
submissionListContent p = onlyHtml $ mkI18NHtml $ \i -> H.div ! A.class_ (className submissionListDiv) $ do
  H.table $ do
    H.tr $ do
      firstCol  (i "Tárgy, csoport:")
      secondCol (slGroup . smList $ p)
    H.tr $ do
      firstCol (i "Oktató:")
      secondCol (join . slTeacher . smList $ p)
    H.tr $ do
      firstCol (i "Feladat:")
      secondCol (assignmentName . slAssignment . smList $ p)
    H.tr $ do
      firstCol (i "Határidő:")
      secondCol (showDate . (uTime p) . assignmentEnd . slAssignment $ smList p)
  H.h2 $ (translate i "Részletes leírás")
  H.div # assignmentTextDiv $
    (markdownToHtml . assignmentDesc . slAssignment . smList $ p)
  let submissions = slSubmissions . smList $ p
  H.h2 (translate i "Beadott megoldások")
  either (userSubmissionTimes i) (userSubmissionInfo i) submissions
  where
    firstCol  t = H.td # textAlignRight $ H.b $ fromString t
    secondCol t = H.td # textAlignLeft $ fromString t

    submissionLine (sk, time, status, t) = H.tr $ do
      H.td # informationalCell $ link
        (routeOf $ P.SubmissionDetails (asKey p) sk)
        (fromString . showDate $ (uTime p) time)
      H.td # informationalCell $ (fromString status)

    submissionTimeLine time =
      H.tr $ (H.td # informationalCell) $ (fromString . showDate $ (uTime p) time)

    userSubmissionInfo  i = userSubmission i submissionLine
    userSubmissionTimes i = userSubmission i submissionTimeLine

    userSubmission i line submissions =
      if (not $ null submissions)
        then do
          table (fieldName submissionTableName) (className submissionListTable) # informationalTable $
            mapM_ line submissions
        else do
          translate i "Nincsenek még beadott megoldások."



invalidAssignment :: Pagelet
invalidAssignment = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "Olyan feladatot próbáltál megnyitni, amelyik nem hozzád tartozik!")

assignmentNotStartedYet :: Pagelet
assignmentNotStartedYet = onlyHtml $ mkI18NHtml $ \i ->
  (translate i "Olyan feladatot próbáltál megnyitni, amelyik nem érhető el!")
