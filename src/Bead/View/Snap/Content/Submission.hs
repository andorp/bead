{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission (
    submission
  , resolveStatus
  ) where

import           Data.List (intersperse)
import           Data.String (fromString)
import           Data.Time

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Snap.Content
import           Bead.View.Snap.Content.Utils
import           Bead.View.Snap.Markdown (markdownToHtml)

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

submission = ViewModifyHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  , asTimeConv :: UserTimeConverter
  , asNow :: UTCTime
  }

submissionPage :: GETContentHandler
submissionPage = withUserState $ \s -> do
  let render p = renderDynamicPagelet $ withUserFrame s p
  ak <- getParameter assignmentKeyPrm
  ut <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  -- TODO: Refactor use guards
  userAssignmentForSubmission
    ak
    (\desc asg -> render $ submissionContent
       (PageData { asKey = ak, asValue = asg, asDesc = desc, asTimeConv = ut, asNow = now }))
    (render invalidAssignment)

submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  ak <- getParameter assignmentKeyPrm
  userAssignmentForSubmission
    ak
    -- Assignment is for the user
    (\_desc asg -> do
       let aspects = Assignment.aspects asg
       if Assignment.isPasswordProtected aspects
         -- Password-protected assignment
         then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Feltöltési jelszó")
                 if Assignment.getPassword aspects == pwd
                   -- Passwords do match
                   then NewSubmission ak
                          <$> (E.Submission
                                <$> (SimpleSubmission <$> getParameter (stringParameter (fieldName submissionTextField) "Megoldás szövege"))
                                <*> liftIO getCurrentTime)
                   -- Passwords do not match
                   else return . ErrorMessage $ Msg_Submission_InvalidPassword "Invalid password, the solution could not be submitted!"
         -- Non password protected assignment
         else NewSubmission ak
                <$> (E.Submission
                       <$> (SimpleSubmission <$> getParameter (stringParameter (fieldName submissionTextField) "Megoldás szövege"))
                       <*> liftIO getCurrentTime))
    -- Assignment is not for the user
    (return . ErrorMessage $ Msg_Submission_NonUsersAssignment "The assignment is not for the actual user!")

submissionContent :: PageData -> IHtml
submissionContent p = do
  msg <- getI18N
  return $ postForm (routeOf submission) `withId` (rFormId submissionForm) $ H.div ! formDiv $ do
    H.table $ do
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Course "Course: ")
        H.td $ (fromString . aGroup $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Admin "Teacher: ")
        H.td $ (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Assignment "Assignment: ")
        H.td $ (fromString . Assignment.name . asValue $ p)
      H.tr $ do
        H.td $ H.b $ (fromString . msg $ Msg_Submission_Deadline "Deadline: ")
        H.td $ (fromString . showDate . (asTimeConv p) . Assignment.end $ asValue p)
      H.tr $ do
        H.td $ H.b $ fromString . msg $ Msg_Submission_TimeLeft "Time left: "
        H.td $ startEndCountdownDiv
          "ctd"
          (msg $ Msg_Submission_Days "day(s)")
          (msg $ Msg_Submission_DeadlineReached "Deadline is reached")
          (asNow p)
          (Assignment.end $ asValue p)
    H.h2 $ (fromString . msg $ Msg_Submission_Description "Description")
    H.div # assignmentTextDiv $
      markdownToHtml . Assignment.desc . asValue $ p
    H.h2 $ (fromString . msg $ Msg_Submission_Solution "Submission")
    (assignmentPasswordDiv msg)
    H.div $ do
      textAreaInput (fieldName submissionTextField) Nothing ! A.rows "25" ! A.cols "80"
    submitButton (fieldName submitSolutionBtn) (msg $ Msg_Submission_Submit "Submit")
    hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
  where
    submission = Pages.submission ()

    assignmentPasswordDiv msg =
      when (Assignment.isPasswordProtected . Assignment.aspects $ asValue p) $ do
        H.div $ do
          H.p $ fromString . msg $ Msg_Submission_Info_Password
            "This assignment can only accept submissions by providing the password."
          H.table $ do
            H.tr $ do
              H.td $ H.b $ fromString . msg $ Msg_Submission_Password "Password for the assignment:"
              H.td $ passwordInput (fieldName submissionPwdField) 20 Nothing ! A.required ""
            H.tr $ do
              H.td $ H.b $ fromString . msg $ Msg_Submission_PasswordAgain "Password again:"
              H.td $ passwordInput (fieldName submissionPwdAgainField) 20 Nothing ! A.required ""

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return . fromString . msg $ Msg_Submission_Invalid_Assignment "It is not allowed to access this assignment with this user."

resolveStatus :: I18N -> Maybe String -> H.Html
resolveStatus msg Nothing     = fromString . msg $ Msg_SubmissionList_NotEvaluatedYet "Not evaluated yet"
resolveStatus _msg (Just str) = fromString str

-- CSS Section

formDiv = A.style "width: 100%; height: 300px"
title   = A.style "float: left; width: 100%"
leftInput = A.style "float: left; width: 58%; height: 80%"
rightInfo = A.style "float: right; width: 39% height: 80%"
fillDiv   = A.style "width: 98%; height: 98%"
