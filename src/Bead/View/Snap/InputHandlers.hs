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

import Text.Blaze.Html5 (Html(..))

-- * Input pagelet and handler

class GetValueHandler i where
  getValue :: HandlerError App App i

class InputPagelet i where
  inputPagelet :: Maybe i -> Html

-- * Instances

instance GetValueHandler GroupKey where
  getValue = getParameter groupKeyPrm

emptyGroup :: Maybe Group
emptyGroup = Nothing

instance InputPagelet Group where
  inputPagelet g = do
    let hook = createGroupHook
    table "create-group" "create-group-table" $ do
      tableLine "Név" $ required $ textInput (fieldName groupNameField) 10 (fmap groupName g)
      tableLine "Leírás" $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)
      tableLine "Értékelés" $ evaluationConfig (evSelectionId hook) (fmap groupEvalConfig g)
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
    let hook = createCourseHook
    table "create-course" "create-course-table" $ do
      tableLine "Név" $ required $ textInput (fieldName courseNameField) 10 (fmap courseName c)
      tableLine "Leírás" $ textInput (fieldName courseDescField) 10 (fmap courseDesc c)
      tableLine "Értékelés" $ evaluationConfig (evSelectionId hook) (fmap courseEvalConfig c)
    hiddenInputWithId (evHiddenValueId hook) ""
    evalSelectionDiv hook

emptyRole :: Maybe Role
emptyRole = Nothing

instance InputPagelet Role where
  inputPagelet q = selection (fieldName userRoleField) $ mapM_ (roleOptions q) roles
    where
      roleOptions Nothing   r = option (show r) (roleLabel r) False
      roleOptions (Just q') r = option (show r) (roleLabel r) (q' == r)

      roleLabel = roleCata
        "Hallgató"
        "Oktató"
        "Tárgyfelelős"
        "Rendszergazda"

instance GetValueHandler Role where
  getValue = getParameter rolePrm

emptyUsername :: Maybe Username
emptyUsername = Nothing

instance InputPagelet Username where
  inputPagelet u = textInput (fieldName usernameField) 20 (fmap str u)

instance GetValueHandler Username where
  getValue = getParameter usernamePrm

emptyUser :: Maybe User
emptyUser = Nothing

instance GetValueHandler User where
  getValue = User
    <$> getValue -- role
    <*> getValue -- username
    <*> getParameter userEmailPrm
    <*> getParameter (stringParameter (fieldName userFamilyNameField) "Teljes név")
    <*> getParameter userTimeZonePrm

instance InputPagelet User where
  inputPagelet u = table "user-detail-table" "user-detail-table" $ do
    tableLine "Szerepkör"  $ required $ inputPagelet (fmap u_role u)
    tableLine "Email cím"  $ required $ textInput (fieldName userEmailField)      20 (fmap (str . u_email) u)
    tableLine "Teljes név" $ required $ textInput (fieldName userFamilyNameField) 20 (fmap u_name u)
    tableLine "Időzóna"    $ required $ defEnumSelection (B.name userTimeZonePrm) (maybe UTC u_timezone u)
    when (isJust u) . hiddenTableLine . hiddenInput (fieldName usernameField) . str . u_username . fromJust $ u

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
evaluationConfig :: String -> Maybe EvaluationConfig -> Html
evaluationConfig n v = do
  valueSelection valueAndName n evaluationTypes
  where
    valueAndName e = (encodeEvalType e, name e)

    name (BinEval ()) = "Kétállapotú"
    name (PctEval ()) = "Százalékos"

-- TODO
dateInput :: String -> Maybe UTCTime -> Html
dateInput n v = required . setHookClass datePickerClass $ textInput n 10 (show <$> v)

hourInput :: String -> Maybe Int -> Html
hourInput n v = required . setHookClass hourSpinnerClass $ textInput n 2 (show <$> v)

minInput :: String -> Maybe Int -> Html
minInput n v = required . setHookClass minuteSpinnerClass $ textInput n 2 (show <$> v)
