{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Evaluation.Page (
    evaluation
  , modifyEvaluation
  ) where

import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Arrow ((&&&))
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Text.Printf
import           Data.String (fromString)
import           Data.Time (getCurrentTime)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (submissionDescription)
import           Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Evaluation
import           Bead.View.Content as C
import           Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Comments
import           Bead.View.Content.SeeMore
import           Bead.View.Content.SubmissionTable (formatSubmissionInfo)
import           Bead.View.Content.VisualConstants

import           Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

evaluation :: ViewModifyHandler
evaluation = ViewModifyHandler evaluationPage evaluationPostHandler

modifyEvaluation :: ViewModifyHandler
modifyEvaluation = ViewModifyHandler modifyEvaluationPage modifyEvaluationPost

-- Page Data consitits of a description for a submission key, which contains
-- the assignment key as well, the submission key, an evautation key, and
-- the time converter for the actual user.
-- If the evaluation key is Nothing means a new evaulation, otherwise
-- the modification of a given evaulation is done
data PageData = PageData {
    sbmDesc :: SubmissionDesc
  , sbmSubmissionKey :: SubmissionKey
  , sbmEvaluationKey :: Maybe EvaluationKey
  , userTime :: UserTimeConverter
  }

evaluationPage :: GETContentHandler
evaluationPage = do
  sk <- getParameter submissionKeyPrm
  sd <- userStory (submissionDescription sk)
  tc <- userTimeZoneToLocalTimeConverter
  let pageData = PageData {
      sbmDesc = sd
    , sbmSubmissionKey = sk
    , sbmEvaluationKey = Nothing
    , userTime = tc
    }
  return $ evaluationContent pageData

modifyEvaluationPage :: GETContentHandler
modifyEvaluationPage = do
  sk <- getParameter submissionKeyPrm
  ek <- getParameter evaluationKeyPrm
  sd <- userStory (submissionDescription sk)
  tc <- userTimeZoneToLocalTimeConverter
  let pageData = PageData {
    sbmDesc = sd
  , sbmSubmissionKey = sk
  , sbmEvaluationKey = Just ek
  , userTime = tc
  }
  return $ evaluationContent pageData

evalConfigParam = evalConfigParameter (fieldName evaluationConfigField)
freeFormEvaluationParam = stringParameter (fieldName evaluationFreeFormField) "Free format evaluation"

-- Reads the evaluation result, from the parameters and determine if the content
-- of the text area would be a comment of the textual evaluation of the given submission.
-- The result of the computation is a UserActon which is a CreateComment or
-- something that depends on the key end the evaluation itself.
abstractEvaluationPostHandler
  :: ContentHandler key
  -> (key -> C.Evaluation -> UserAction)
  -> POSTContentHandler
abstractEvaluationPostHandler getEvKeyParameter evCommand = do
  sk <- getParameter submissionKeyPrm
  commentText <- getParameter evaluationValuePrm
  config <- getParameter evalConfigParam
  commentOrResult <-
    evConfigCata
      (getJSONParam (fieldName evaluationResultField) "No evaluation can be found.")
      (\_ -> do
        percentage  <- getParameterWithDefault 0 evaluationPercentagePrm
        commentOnly <- getParameter evaluationCommentOnlyPrm
        return $
          if commentOnly
            then EvCmtComment
            else EvCmtResult $ percentageResult (fromIntegral percentage / 100))
      (do freeForm <- getParameter freeFormEvaluationParam
          return $ if (null freeForm)
            then EvCmtComment
            else EvCmtResult $ freeFormResult freeForm)
      config
  withEvalOrComment commentOrResult
    (case null commentText of
      True -> return $
        ErrorMessage (msg_Evaluation_EmptyCommentAndFreeFormResult "Neither comment nor evaluation was given!")
      False -> do
        (mrole,mname) <- (getRole &&& getName) <$> userState
        let uname = fromMaybe "???" mname
        case mrole of
          Nothing -> return $ LogMessage "The user is not logged in" -- Impossible
          Just role -> do
            now <- liftIO $ getCurrentTime
            return $ SubmissionComment sk Comment {
               comment = commentText
             , commentAuthor = uname
             , commentDate = now
             , commentType = roleToCommentType role
             })
    (\result -> do
      key <- getEvKeyParameter
      let e = C.Evaluation {
          evaluationResult = result
        , writtenEvaluation = commentText
        }
      return $ evCommand key e)
  where
    roleToCommentType = roleCata
      CT_Student
      CT_GroupAdmin
      CT_CourseAdmin
      CT_Admin

    getRole = userStateCata
      Nothing
      Nothing
      Nothing
      (\_username _uid _page _name role _token _timezone _status -> Just role)

    getName = userStateCata
      Nothing
      Nothing
      Nothing
      (\_username _uid _page name _role _token _timezone _status -> Just name)

evaluationPostHandler :: POSTContentHandler
evaluationPostHandler = abstractEvaluationPostHandler (getParameter submissionKeyPrm) NewEvaluation

modifyEvaluationPost :: POSTContentHandler
modifyEvaluationPost = abstractEvaluationPostHandler (getParameter evaluationKeyPrm) ModifyEvaluation

evaluationFrame :: EvConfig -> I18N -> Html -> Html
evaluationFrame evConfig msg content = do
  hiddenInput (fieldName evalConfigParam) (encodeToFay' "inputEvalType" evConfig)
  withEvConfig evConfig
    (do content
        Bootstrap.formGroup $ evaluationDiv $
          Bootstrap.radioButtonGroup (fieldName evaluationResultField) $
            [ (True,  encodeToFay' "inputEvalResult" EvCmtComment   , msg $ msg_Evaluation_New_Comment "New Comment")
            , (False, encodeToFay' "inputEvalResult" $ binary Passed, msg $ msg_Evaluation_Accepted "Accepted")
            , (False, encodeToFay' "inputEvalResult" $ binary Failed, msg $ msg_Evaluation_Rejected "Rejected")
            ])
    -- When the page is dynamic the percentage spinner is hooked on the field
    (\_ ->
      do content
         Bootstrap.formGroup . evaluationDiv $ do
           Bootstrap.colMd4 $
             Bootstrap.radioButtonGroup (fieldName evaluationCommentOnlyPrm) $
               [ (True,  show True,  msg $ msg_Evaluation_New_Comment "New Comment")
               , (False, show False, msg $ msg_Evaluation_Percentage "Percentage: ")
               ]
           Bootstrap.colMd4 $
             H.input ! A.name (fieldName evaluationPercentagePrm) ! A.type_ "number"
                     ! A.min (fromString $ show 0) ! A.max (fromString $ show 100)
                     ! A.value (fromString $ show 0))
    (do Bootstrap.optionalTextInput (fieldName freeFormEvaluationParam) (msg $ msg_Evaluation_FreeFormEvaluation "Evaluation") ""
        H.p . fromString $ printf (msg $ msg_Evaluation_FreeForm_Information $ unwords
          [ "Note that this text will be used everywhere as the evaluation itself.  Hence it is recommended to keep"
          , "the length of the text under size %d, otherwise it may not be directly shown." ]) displayableFreeFormResultLength
        content)
  where
    binary = EvCmtResult . binaryResult
    evaluationDiv = withEvConfig
      evConfig
      (H.div)
      (const $ H.div ! A.id (fieldName evaluationPercentageDiv))
      (H.div)

-- * View

evaluationContent :: PageData -> IHtml
evaluationContent pd = do
  let sd = sbmDesc pd
      tc = userTime pd
  msg <- getI18N
  let freeFormCommentTitle = evConfigCata
        (return ())
        (const $ return ())
        (Bootstrap.labelFor (fieldName evaluationValueField) (msg $ msg_Evaluation_FreeFormComment "Comment"))
  return $ do
    Bootstrap.row $ Bootstrap.colMd12 $
      H.p $ fromString . msg $ msg_Evaluation_Info $ concat
        [ "It is not mandatory to evaluate the submission, it is allowed to comment on it only.  "
        , "The student may answer the comments by further comments.  The submission may be "
        , "evaluated many times."
        ]

    Bootstrap.row $ Bootstrap.colMd12 $ Bootstrap.table $
      H.tbody $ do
        let aName = assignmentCata (\name _ _ _ _ _ -> name)
        (msg $ msg_Evaluation_Course "Course: ") .|. (fromString . eCourse $ sd)
        (msg $ msg_Evaluation_Assignment "Assignment: ") .|. (fromString . aName . eAssignment $ sd)
        maybe
          mempty
          (\group -> (msg $ msg_Evaluation_Group "Group: ") .|. (fromString group))
          (eGroup sd)
        (msg $ msg_Evaluation_Student "Student: ") .|. (fromString . eStudent $ sd)
        (msg $ msg_Evaluation_Username "Username: ") .|. (fromString . uid Prelude.id $ eUid sd)
        (msg $ msg_Evaluation_SubmissionDate "Date of submission: ") .|. (fromString . showDate . tc $ eSubmissionDate sd)
        (msg $ msg_Evaluation_SubmissionInfo "State: ") .|. submissionIcon msg (eSubmissionInfo sd)

    Bootstrap.row $ Bootstrap.colMd12 $ do
      let downloadSubmissionButton =
            Bootstrap.buttonLink
              (routeOf $ Pages.getSubmission submissionKey ())
              (msg $ msg_Evaluation_Submitted_Solution_Zip_Link "Download")

      h2 $ fromString $ msg $ msg_Evaluation_Submitted_Solution "Submission"
      if (Assignment.isZippedSubmissions . Assignment.aspects . eAssignment $ sd)
        then do
          H.p $ fromString . msg $ msg_Evaluation_Submitted_Solution_Zip_Info $ mconcat
            [ "The submission was uploaded as a compressed file so it could not be displayed verbatim.  "
            , "But it may be downloaded as a file by clicking on the link."
            ]
          downloadSubmissionButton
        else do
          H.p $ fromString . msg $ msg_Evaluation_Submitted_Solution_Text_Info $
            "The submission may be downloaded as a plain text file by clicking on the link."
          downloadSubmissionButton
          H.br
          H.div # submissionTextDiv $ seeMoreSubmission "submission-text-" msg maxLength maxLines (eSolution sd)

    Bootstrap.row $ Bootstrap.colMd12 $
      postForm (routeOf . evPage $ maybeEvalKey) $ do
        let evType = Assignment.evType $ eAssignment sd
        evaluationFrame evType msg $ do
          freeFormCommentTitle evType
          Bootstrap.optionalTextArea (fieldName evaluationValueField) "" $ mempty
          hiddenInput (fieldName assignmentKeyField) (paramValue $ eAssignmentKey sd)
          hiddenInput (fieldName evCommentOnlyText) (msg $ msg_Evaluation_New_Comment "New Comment")
        Bootstrap.submitButton
          (fieldName saveEvalBtn) (fromString . msg $ msg_Evaluation_SaveButton "Submit")

    let comments = submissionDescToCFs sd
    when (not $ null comments) $ do
      Bootstrap.row $ Bootstrap.colMd12 $ hr
      Bootstrap.row $ Bootstrap.colMd12 $
        H.h2 (fromString . msg $ msg_Comments_Title "Comments")
      -- Renders the comment area where the user can place a comment
      i18n msg $ commentsDiv "evaluation-comments-" tc comments
  where
    submissionKey = sbmSubmissionKey pd
    maybeEvalKey  = sbmEvaluationKey pd

    evPage (Just ek) = Pages.modifyEvaluation submissionKey ek ()
    evPage Nothing   = Pages.evaluation submissionKey ()

    maxLength = 2048
    maxLines  = 100

    submissionIcon :: I18N -> SubmissionInfo -> H.Html
    submissionIcon msg =
      formatSubmissionInfo
        id
        mempty -- not found
        (H.i ! A.class_ "glyphicon glyphicon-stop"  ! A.style "color:#AAAAAA; font-size: large"
             ! tooltip (msg_Home_SubmissionCell_NonEvaluated "Non evaluated") $ mempty) -- non-evaluated
        (bool (H.i ! A.class_ "glyphicon glyphicon-ok-circle" ! A.style "color:#AAAAAA; font-size: large"
                   ! tooltip (msg_Home_SubmissionCell_Tests_Passed "Tests are passed") $ mempty)  -- tested accepted
              (H.i ! A.class_ "glyphicon glyphicon-remove-circle" ! A.style "color:#AAAAAA; font-size: large"
                   ! tooltip (msg_Home_SubmissionCell_Tests_Failed "Tests are failed") $ mempty)) -- tested rejected
        (H.i ! A.class_ "glyphicon glyphicon-thumbs-up" ! A.style "color:#00FF00; font-size: large"
             ! tooltip (msg_Home_SubmissionCell_Accepted "Accepted") $ mempty) -- accepted
        (H.i ! A.class_ "glyphicon glyphicon-thumbs-down" ! A.style "color:#FF0000; font-size: large"
             ! tooltip (msg_Home_SubmissionCell_Rejected "Rejected") $ mempty) -- rejected
      where
        tooltip m = A.title (fromString $ msg m)
