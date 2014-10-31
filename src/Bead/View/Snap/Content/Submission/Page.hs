{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission.Page (
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
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import           Bead.View.Snap.Content.Utils
import           Bead.View.Snap.Markdown (markdownToHtml)

import           Text.Blaze.Html5 as H

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
  let render p = renderBootstrapPage $ bootstrapUserFrame s p
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
         then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Submission password")
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
  return $ do
    -- Informational table on the page
    Bootstrap.rowColMd12 $ Bootstrap.table $
      H.tbody $ do
        (msg $ Msg_Submission_Course "Course: ")         .|. (fromString . aGroup $ asDesc p)
        (msg $ Msg_Submission_Admin "Teacher: ")         .|. (fromString . concat . intersperse ", " . aTeachers $ asDesc p)
        (msg $ Msg_Submission_Assignment "Assignment: ") .|. (fromString . Assignment.name $ asValue p)
        (msg $ Msg_Submission_Deadline "Deadline: ")     .|.
          (fromString . showDate . (asTimeConv p) . Assignment.end $ asValue p)
        (msg $ Msg_Submission_TimeLeft "Time left: ")    .|. (startEndCountdownDiv
                "ctd"
                (msg $ Msg_Submission_Days "day(s)")
                (msg $ Msg_Submission_DeadlineReached "Deadline is reached")
                (asNow p)
                (Assignment.end $ asValue p))
    Bootstrap.rowColMd12 $ do
      H.h2 $ fromString $ msg $ Msg_Submission_Description "Description"
      H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc $ asValue p
    postForm (routeOf submission) $ do
      hiddenInput (fieldName assignmentKeyField) (paramValue (asKey p))
      assignmentPassword msg
      Bootstrap.rowColMd12 $ h2 $
        fromString $ msg $ Msg_Submission_Solution "Submission"
      Bootstrap.textArea (fieldName submissionTextField) "" ""
      Bootstrap.submitButton (fieldName submitSolutionBtn) (fromString $ msg $ Msg_Submission_Submit "Submit")

  where
    submission = Pages.submission ()

    assignmentPassword msg =
      when (Assignment.isPasswordProtected . Assignment.aspects $ asValue p) $ do
        H.p $ fromString . msg $ Msg_Submission_Info_Password
          "This assignment can only accept submissions by providing the password."
        Bootstrap.passwordInput (fieldName submissionPwdField) (msg $ Msg_Submission_Password "Password for the assignment:")
        Bootstrap.passwordInput (fieldName submissionPwdAgainField) (msg $ Msg_Submission_PasswordAgain "Password again:")

-- Creates a table line first element is a bold text and the second is a HTML snippet
infixl 7 .|.
name .|. value = H.tr $ do
  H.td $ b $ fromString $ name
  H.td value

invalidAssignment :: IHtml
invalidAssignment = do
  msg <- getI18N
  return . fromString . msg $ Msg_Submission_Invalid_Assignment "It is not allowed to access this assignment with this user."

resolveStatus :: I18N -> Maybe String -> H.Html
resolveStatus msg Nothing     = fromString . msg $ Msg_SubmissionList_NotEvaluatedYet "Not evaluated yet"
resolveStatus _msg (Just str) = fromString str
