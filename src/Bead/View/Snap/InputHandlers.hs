module Bead.View.Snap.InputHandlers where

import Control.Applicative ((<$>),(<*>))
import Data.Maybe (maybe, isJust, fromJust)
import Data.Time (UTCTime(..))

import Bead.Domain.Types (Str(..), readMaybe, readMsg)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Evaluation
import qualified Bead.View.Snap.DataBridge as B

import Bead.Controller.ServiceContext (UserState(..))
import qualified Bead.Controller.UserStories as Story (userState)

import Bead.View.Snap.Application (App(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.DataBridge

import Bead.View.Snap.I18N (IHtml, getI18N, i18n)
import Bead.View.Snap.Translation

import Text.Blaze.Html5 (Html)

-- * Input pagelet and handler

class GetValueHandler i where
  getValue :: HandlerError App App i

class InputPagelet i where
  inputPagelet :: Maybe i -> IHtml

-- * Instances

instance GetValueHandler GroupKey where
  getValue = getParameter groupKeyPrm

emptyGroup :: Maybe Group
emptyGroup = Nothing

instance InputPagelet Group where
  inputPagelet g = do
    msg <- getI18N
    let hook = createGroupHook
    evalConfig <- evaluationConfig (evSelectionId hook) (fmap groupEvalConfig g)
    return $ do
      table "create-group" "create-group-table" $ do
        tableLine (msg $ Msg_Input_Group_Name "Title") $ required $ textInput (fieldName groupNameField) 10 (fmap groupName g)
        tableLine (msg $ Msg_Input_Group_Description "Description") $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)
        tableLine (msg $ Msg_Input_Group_Evaluation "Evaluation") $ evalConfig
      hiddenInputWithId (evHiddenValueId hook) ""
      evalSelectionDiv hook

instance GetValueHandler Group where
  getValue = Group
      <$> getParameter (stringParameter (fieldName groupNameField) "Csoport neve")
      <*> getParameter (stringParameter (fieldName groupDescField) "Csoport leírása")
      <*> getParameter (evalConfigPrm createGroupHook)

instance GetValueHandler CourseKey where
  getValue = getParameter courseKeyPrm

instance GetValueHandler Course where
  getValue = Course
    <$> getParameter (stringParameter (fieldName courseNameField) "Tárgy neve")
    <*> getParameter (stringParameter (fieldName courseDescField) "Tárgy leírása")
    <*> getParameter (evalConfigPrm createCourseHook)

emptyCourse :: Maybe Course
emptyCourse = Nothing

instance InputPagelet Course where
  inputPagelet c = do
    msg <- getI18N
    let hook = createCourseHook
    evalConfig <- evaluationConfig (evSelectionId hook) (fmap courseEvalConfig c)
    return $ do
      table "create-course" "create-course-table" $ do
        tableLine (msg $ Msg_Input_Course_Name "Title") $ required $ textInput (fieldName courseNameField) 10 (fmap courseName c)
        tableLine (msg $ Msg_Input_Course_Description "Description") $ textInput (fieldName courseDescField) 10 (fmap courseDesc c)
        tableLine (msg $ Msg_Input_Course_Evaluation "Evaluation") $ evalConfig
      hiddenInputWithId (evHiddenValueId hook) ""
      evalSelectionDiv hook

emptyRole :: Maybe Role
emptyRole = Nothing

instance InputPagelet Role where
  inputPagelet q = do
    msg <- getI18N
    return $ selection (fieldName userRoleField) $ mapM_ (roleOptions msg q) roles
    where
      roleOptions msg Nothing   r = option (show r) (msg $ roleLabel r) False
      roleOptions msg (Just q') r = option (show r) (msg $ roleLabel r) (q' == r)

      roleLabel = roleCata
        (Msg_InputHandlers_Role_Student "Student")
        (Msg_InputHandlers_Role_GroupAdmin "Teacher")
        (Msg_InputHandlers_Role_CourseAdmin "Course Admin")
        (Msg_InputHandlers_Role_Admin "Administrator")

instance GetValueHandler Role where
  getValue = getParameter rolePrm

emptyUsername :: Maybe Username
emptyUsername = Nothing

instance InputPagelet Username where
  inputPagelet u = return $ textInput (fieldName usernameField) 20 (fmap str u)

instance GetValueHandler Username where
  getValue = getParameter usernamePrm

emptyAssignment :: Maybe Assignment
emptyAssignment = Nothing

instance GetValueHandler AssignmentKey where
  getValue = getParameter assignmentKeyPrm

-- The start date of the assignment should be placed before
-- than the end date
instance GetValueHandler Assignment where
  getValue = do
    timeZone <- timezone <$> userStory Story.userState
    startDate <- getParameter (assignmentStartPrm timeZone)
    endDate   <- getParameter (assignmentEndPrm timeZone)
    when (endDate < startDate) . throwError $ strMsg "A feladat kezdetének dátuma később van mint a feladat vége"
    assignmentAna
      (getParameter (stringParameter (fieldName assignmentNameField) "Név"))
      (getParameter (stringParameter (fieldName assignmentDescField) "Leírás"))
      (getParameter assignmentTypePrm)
      (return startDate)
      (return timeZone)
      (return endDate)
      (return timeZone)

-- * Combined input fields

emptyEvaluationConfig :: Maybe (EvaluationData Binary Percentage)
emptyEvaluationConfig = Nothing

-- TODO
evaluationConfig :: String -> Maybe EvaluationConfig -> IHtml
evaluationConfig n v = do
  msg <- getI18N
  return $ valueSelection (valueAndName msg) n evaluationTypes
  where
    valueAndName msg e = (encodeEvalType e, msg $ name e)

    name (BinEval ()) = Msg_InputHandlers_BinEval "Binary"
    name (PctEval ()) = Msg_InputHandlers_PctEval "Percentage"

-- TODO
dateInput :: String -> Maybe UTCTime -> Html
dateInput n v = required . setHookClass datePickerClass $ textInput n 10 (show <$> v)

hourInput :: String -> Maybe Int -> Html
hourInput n v = required . setHookClass hourSpinnerClass $ textInput n 2 (show <$> v)

minInput :: String -> Maybe Int -> Html
minInput n v = required . setHookClass minuteSpinnerClass $ textInput n 2 (show <$> v)
