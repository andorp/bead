{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  , resolveStatus
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

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

submission :: Content
submission = getPostContentHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  , asTimeConv :: UserTimeConverter
  }

submissionPage :: GETContentHandler
submissionPage = withUserState $ \s -> do
  let render p = renderPagelet $ withUserFrame s p
  ak <- getParameter assignmentKeyPrm
  ut <- usersTimeZoneConverter
  userAssignmentForSubmission
    ak
    (\desc asg -> render $ submissionContent
       (PageData { asKey = ak, asValue = asg, asDesc = desc, asTimeConv = ut }))
    (render invalidAssignment)

submissionPostHandler :: POSTContentHandler
submissionPostHandler =
  NewSubmission
    <$> getParameter assignmentKeyPrm
    <*> (E.Submission
           <$> getParameter (stringParameter (fieldName submissionTextField) "Megoldás szövege")
           <*> liftIO getCurrentTime)

submissionContent :: PageData -> IHtml
submissionContent p = do
  msg <- getI18N
  return $ postForm (routeOf P.Submission) $ H.div ! formDiv $ do
    H.table $ do
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Course "Tárgy: ")
        H.td $ (fromString . aGroup $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Admin "Oktató: ")
        H.td $ (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Assignment "Feladat: ")
        H.td $ (fromString . assignmentName . asValue $ p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Deadline "Határidő: ")
        H.td $ (fromString . showDate . (asTimeConv p) . assignmentEnd $ asValue p)
    H.h2 $ (fromString . msg $ Msg_Submission_Description "Leírás")
    H.div # assignmentTextDiv $
      markdownToHtml . assignmentDesc . asValue $ p
    H.h2 $ (fromString . msg $ Msg_Submission_Solution "Megoldás")
    H.div $ do
      textAreaInput (fieldName submissionTextField) Nothing ! A.rows "25" ! A.cols "80"
    submitButton (fieldName submitSolutionBtn) (msg $ Msg_Submission_Submit "Beküld")
    hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return . fromString . msg $ Msg_Submission_Invalid_Assignment "Olyan feladatot próbáltál megnyitni, amely nem hozzád tartozik!"

resolveStatus :: I18N -> Maybe String -> H.Html
resolveStatus msg Nothing    = fromString . msg $ Msg_SubmissionList_NotEvaluatedYet "Még nem értékelt"
resolveStatus msg (Just str) = fromString str

-- CSS Section

formDiv = A.style "width: 100%; height: 300px"
title   = A.style "float: left; width: 100%"
leftInput = A.style "float: left; width: 58%; height: 80%"
rightInfo = A.style "float: right; width: 39% height: 80%"
fillDiv   = A.style "width: 98%; height: 98%"
