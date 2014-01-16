{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Evaluation (
    evaluation
  , modifyEvaluation
  , commentFromEvaluation
  , commentFromModifyEvaluation
  ) where

import Data.String (fromString)
import Data.Time (UTCTime, LocalTime, getCurrentTime)
import Control.Monad (liftM)

import Bead.Domain.Types (readMsg)
import Bead.Domain.Relationships (SubmissionDesc(..))
import Bead.Controller.Pages as P(Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.UserStories (submissionDescription)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content as C
import Bead.View.Snap.Content.Comments

import Bead.Domain.Evaluation

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A (id, class_, style)
import qualified Text.Blaze.Html5 as H
import Bead.View.Snap.I18N (IHtml)

evaluation :: Content
evaluation = getPostContentHandler evaluationPage evaluationPostHandler

modifyEvaluation :: Content
modifyEvaluation = getPostContentHandler modifyEvaluationPage modifyEvaluationPost

-- Comment on the given evaluation page, the admin does not want to evaluate
-- the submission, only places a comment
commentFromEvaluation :: Content
commentFromEvaluation = postContentHandler commentOnSubmissionHandler

-- Comment on the given evaluation page, the admin does not want to
-- modify the evaluation only places a comment
commentFromModifyEvaluation :: Content
commentFromModifyEvaluation = postContentHandler commentOnSubmissionHandler

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

render (BinEval _) = renderPagelet
render (PctEval _) = renderDynamicPagelet

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
  render (eConfig sd) $ withUserFrame s (evaluationContent pageData)

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
  render (eConfig sd) $ withUserFrame s (evaluationContent pageData)

evaluationPostHandler :: POSTContentHandler
evaluationPostHandler = do
  sk <- getParameter submissionKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaluationResultField) "Nem található értékelés!"
  let e = C.Evaluation {
    evaluationResult = evResult er
  , writtenEvaluation = ev
  }
  return $ NewEvaluation sk e

modifyEvaluationPost :: POSTContentHandler
modifyEvaluationPost = do
  ek <- getParameter evaluationKeyPrm
  ev <- getParameter evaluationValuePrm
  er <- getJSONParam (fieldName evaluationResultField) "Nem található értékelés!"
  let e = C.Evaluation {
    evaluationResult = evResult er
  , writtenEvaluation = ev
  }
  return $ C.ModifyEvaluation ek e

commentOnSubmissionHandler :: POSTContentHandler
commentOnSubmissionHandler = do
  mrole <- getRole <$> userState
  case mrole of
    Nothing -> return $ LogMessage "A felhasználó nincs bejelentkezve" -- Impossible
    Just role -> do
      sk <- getParameter submissionKeyPrm
      ak <- getParameter assignmentKeyPrm
      c  <- getParameter (stringParameter (fieldName commentValueField) "Hozzászólás")
      now <- liftIO $ getCurrentTime
      return $ SubmissionComment sk Comment {
         comment = c
       , commentDate = now
       , commentType = roleToCommentType role
       }
  where
    roleToCommentType = roleCata
      CT_Student
      CT_GroupAdmin
      CT_CourseAdmin
      CT_Admin

    getRole = userStateCata
      Nothing
      Nothing
      (\_username _page _name role _token _timezone _status -> Just role)

evaluationContent :: PageData -> IHtml
evaluationContent pd = do
  let sd = sbmDesc pd
      tc = userTime pd
  msg <- getI18N
  return $ do
    postForm (routeOf . evPage $ maybeEvalKey) $ H.div ! formDiv $ do
      H.div ! title $ H.h2 (fromString . msg $ Msg_Evaluation_Title "Értékelés")
      H.div ! leftInfo $ do
        H.table $ do
          H.tr $ do
            H.td $ H.b $ (fromString . msg $ Msg_Evaluation_Course "Tárgy, csoport: ")
            H.td $ (fromString . eGroup $ sd)
          H.tr $ do
            H.td $ H.b $ (fromString . msg $ Msg_Evaluation_Student "Hallgató: ")
            H.td $ (fromString . eStudent $ sd)
        H.div ! A.id (fieldName evaluationPercentageDiv) $
          i18n msg $ inputEvalResult . eConfig $ sd
        submitButton (fieldName saveEvalBtn) (fromString . msg $ Msg_Evaluation_SaveButton "Mentés")
      H.div ! rightText $ do
        textAreaInput (fieldName evaluationValueField) Nothing ! fillDiv
    H.div $ H.h2 $ (fromString . msg $ Msg_Evaluation_Submited_Solution "Beadott megoldás")
    H.div # submissionTextDiv $ H.pre # submissionTextPre $ do
      (fromString . eSolution $ sd)
    when (not . null $ eComments sd) $ do
      i18n msg $ commentsDiv tc . eComments $ sd
    -- Renders the comment area where the user can place a comment
    i18n msg $ commentPostForm (commentPage maybeEvalKey) (eAssignmentKey sd)
  where
    submissionKey = sbmSubmissionKey pd
    maybeEvalKey  = sbmEvaluationKey pd

    defaultEvalCfg :: EvaluationResult
    defaultEvalCfg = BinEval (Binary Passed)

    evPage (Just ek) = P.ModifyEvaluation submissionKey ek
    evPage Nothing   = P.Evaluation submissionKey

    commentPage (Just ek) = P.CommentFromModifyEvaluation submissionKey ek
    commentPage Nothing   = P.CommentFromEvaluation submissionKey

inputEvalResult :: EvaluationConfig -> IHtml
inputEvalResult (BinEval cfg) = do
  msg <- getI18N
  return $ valueSelection valueAndText (fieldName evaluationResultField) $
             [ (Passed, msg $ Msg_Evaluation_Accepted "Elfogadott")
             , (Failed, msg $ Msg_Evaluation_Rejected "Elutasított")
             ]
  where
    valueAndText :: (Result, String) -> (String, String)
    valueAndText (v,n) = (errorOnNothing . encodeToFay . EvResult . mkEvalResult $ Binary v, n)

-- When the page is dynamic the percentage spinner is hooked on the field
inputEvalResult (PctEval cfg) =
  return $ hiddenInput
    (fieldName evaluationResultField)
    (fromString . errorOnNothing . encodeToFay . EvResult . mkEvalResult . Percentage $ Scores [0.0])

errorOnNothing = maybe (error "Hiba a bemenet kódolásában!") id

-- CSS Section

formDiv = A.style "width: 100%; height: 200px"
title   = A.style "width: 100%"
leftInfo = A.style "float: left; width: 28%; height: 100%"
rightText = A.style "float: right; width: 68%; height: 100%"
fillDiv = A.style "width: 98%; height: 98%"

