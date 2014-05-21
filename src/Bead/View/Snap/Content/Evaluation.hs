{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaluation (
    evaluation
  , modifyEvaluation
  ) where

import           Control.Arrow ((&&&))
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Time (getCurrentTime)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (submissionDescription)
import           Bead.Domain.Evaluation
import           Bead.View.Snap.Content as C
import           Bead.View.Snap.Content.Comments
import           Bead.View.Snap.Content.SeeMore

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

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
evaluationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  sd <- userStory (submissionDescription sk)
  tc <- usersTimeZoneConverter
  let pageData = PageData {
      sbmDesc = sd
    , sbmSubmissionKey = sk
    , sbmEvaluationKey = Nothing
    , userTime = tc
    }
  renderDynamicPagelet $ withUserFrame s (evaluationContent pageData)

modifyEvaluationPage :: GETContentHandler
modifyEvaluationPage = withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  ek <- getParameter evaluationKeyPrm
  sd <- userStory (submissionDescription sk)
  tc <- usersTimeZoneConverter
  let pageData = PageData {
    sbmDesc = sd
  , sbmSubmissionKey = sk
  , sbmEvaluationKey = Just ek
  , userTime = tc
  }
  renderDynamicPagelet $ withUserFrame s (evaluationContent pageData)

-- Reads the evaluation result, from the parameters and determine if the content
-- of the text area would be a comment of the textual evaluation of the given submission.
-- The result of the computation is a UserActon which is a CreateComment or
-- something that depends on the key end the evaluation itself.
abstractEvaluationPostHandler
  :: HandlerError App App key
  -> (key -> C.Evaluation -> UserAction)
  -> POSTContentHandler
abstractEvaluationPostHandler getEvKeyParameter evCommand = do
  sk <- getParameter submissionKeyPrm
  ak <- getParameter assignmentKeyPrm
  commentText <- getParameter evaluationValuePrm
  commentOrResult <- getJSONParam (fieldName evaluationResultField) "Nem található értékelés!"
  withEvalOrComment commentOrResult
    (do (mrole,mname) <- (getRole &&& getName) <$> userState
        let uname = fromMaybe "???" mname
        case mrole of
          Nothing -> return $ LogMessage "A felhasználó nincs bejelentkezve" -- Impossible
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
            evaluationResult = evResult result
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
      (\_username _page _name role _token _timezone _status -> Just role)

    getName = userStateCata
      Nothing
      Nothing
      Nothing
      (\_username _page name _role _token _timezone _status -> Just name)

evaluationPostHandler :: POSTContentHandler
evaluationPostHandler = abstractEvaluationPostHandler (getParameter submissionKeyPrm) NewEvaluation

modifyEvaluationPost :: POSTContentHandler
modifyEvaluationPost = abstractEvaluationPostHandler (getParameter evaluationKeyPrm) ModifyEvaluation

evaluationContent :: PageData -> IHtml
evaluationContent pd = do
  let sd = sbmDesc pd
      tc = userTime pd
  msg <- getI18N
  return $ do
    H.div . H.p $ fromString . msg $ Msg_Evaluation_Info $ concat
      [ "It is not mandatory to evaluate the submission, it is allowed to comment on it only.  "
      , "The student may answer the comments by further comments.  The submission may be "
      , "evaluated many times."
      ]
    H.div $ postForm (routeOf . evPage $ maybeEvalKey) $ do
      H.div $ do
        H.table $ do
          H.tr $ do
            infoCell (msg $ Msg_Evaluation_Course "Course: ") (eCourse sd)
            maybe empty (infoCell (msg $ Msg_Evaluation_Group "Group: ")) $ eGroup sd
          H.tr $ do
            infoCell (msg $ Msg_Evaluation_Student "Student: ") (eStudent sd)
            infoCell (msg $ Msg_Evaluation_Username "Username: ") (usernameCata id $ eUsername sd)
          H.tr $ do
            infoCell
              (msg $ Msg_Evaluation_SubmissionDate "Date of submission: ")
              (showDate . tc $ eSubmissionDate sd)
      H.div $ H.h2 $ (fromString . msg $ Msg_Evaluation_Submited_Solution "Submission")
      H.div # submissionTextDiv $ do
        seeMorePre msg maxLength maxLines (eSolution sd)
      H.div $ do
        textAreaInput (fieldName evaluationValueField) Nothing ! fillDiv ! A.rows "10"
        hiddenInput (fieldName assignmentKeyField) (paramValue $ eAssignmentKey sd)
        H.div ! alignToRight $ do
          hiddenInput (fieldName evCommentOnlyText) (msg $ Msg_Evaluation_New_Comment "New Comment")
          evaluationDiv . i18n msg . inputEvalResult $ eConfig sd
          submitButton
            (fieldName saveEvalBtn)
            (fromString . msg $ Msg_Evaluation_SaveButton "Submit")
    when (not . null $ eComments sd) $ H.div $ do
      H.h2 (fromString . msg $ Msg_Comments_Title "Comments")
      -- Renders the comment area where the user can place a comment
      do
        H.hr
        i18n msg $ commentsDiv tc . eComments $ sd

  where
    infoCell title value = do
      H.td . H.b $ fromString title
      H.td ! A.style "padding: 0px 10px 0px 0px" $ fromString value

    evaluationDiv = withEvaluationData
      (eConfig $ sbmDesc pd)
      (const H.div)
      (const $ H.div ! A.id (fieldName evaluationPercentageDiv))

    submissionKey = sbmSubmissionKey pd
    maybeEvalKey  = sbmEvaluationKey pd

    evPage (Just ek) = Pages.modifyEvaluation submissionKey ek ()
    evPage Nothing   = Pages.evaluation submissionKey ()

    maxLength = 100
    maxLines  = 5

    empty = return ()

inputEvalResult :: EvaluationConfig -> IHtml
inputEvalResult (BinEval cfg) = do
  msg <- getI18N
  return $ horizontalRadioButtonsDef (fieldName evaluationResultField) 0 $
    [ (EvCmtComment,  msg $ Msg_Evaluation_New_Comment "New Comment")
    , (binary Passed, msg $ Msg_Evaluation_Accepted "Accepted")
    , (binary Failed, msg $ Msg_Evaluation_Rejected "Rejected")
    ]
  where
    binary = EvCmtResult . EvResult . mkEvalResult . Binary

-- When the page is dynamic the percentage spinner is hooked on the field
inputEvalResult (PctEval cfg) =
  return $ hiddenInput
    (fieldName evaluationResultField)
    (fromString . errorOnNothing . encodeToFay . EvResult . mkEvalResult . Percentage $ Scores [0.0])

errorOnNothing = maybe (error "Hiba a bemenet kódolásában!") id

-- CSS Section

fillDiv = A.style "width: 100%"
alignToRight = A.style "text-align: right"
