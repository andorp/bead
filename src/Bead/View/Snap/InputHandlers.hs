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
      tableLine "Group Name" $ required $ textInput (fieldName groupNameField) 10 (fmap groupName g)
      tableLine "Group Desc" $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)
      tableLine "Group Eval" $ evaluationConfig (evSelectionId hook) (fmap groupEvalConfig g)
    hiddenInputWithId (evHiddenValueId hook) ""
    evalSelectionDiv hook

instance GetValueHandler Group where
  getValue = Group
      <$> getParameter (stringParameter (fieldName groupNameField) "Group name")
      <*> getParameter (stringParameter (fieldName groupDescField) "Group description")
      <*> getParameter (evalConfigPrm createGroupHook)

instance GetValueHandler CourseKey where
  getValue = getParameter courseKeyPrm

instance GetValueHandler Course where
  getValue = Course
    <$> getParameter (stringParameter (fieldName courseNameField) "Course name")
    <*> getParameter (stringParameter (fieldName courseDescField) "Course description")
    <*> getParameter (evalConfigPrm createCourseHook)

emptyCourse :: Maybe Course
emptyCourse = Nothing

instance InputPagelet Course where
  inputPagelet c = do
    let hook = createCourseHook
    table "create-course" "create-course-table" $ do
      tableLine "Course Name" $ required $ textInput (fieldName courseNameField) 10 (fmap courseName c)
      tableLine "Course Desc" $ textInput (fieldName courseDescField) 10 (fmap courseDesc c)
      tableLine "Course Eval" $ evaluationConfig (evSelectionId hook) (fmap courseEvalConfig c)
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
        "Student'"
        "Group admin'"
        "Course admin'"
        "Admin'"

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
    <*> getParameter (stringParameter (fieldName userFamilyNameField) "Full name")
    <*> getParameter userTimeZonePrm

instance InputPagelet User where
  inputPagelet u = table "user-detail-table" "user-detail-table" $ do
    tableLine "User's role'"    $ required $ inputPagelet (fmap u_role u)
    tableLine "User's email"    $ required $ textInput (fieldName userEmailField)      20 (fmap (str . u_email) u)
    tableLine "User's fullname" $ required $ textInput (fieldName userFamilyNameField) 20 (fmap u_name u)
    tableLine "User's timezone" $ required $ defEnumSelection (B.name userTimeZonePrm) (maybe UTC u_timezone u)
    when (isJust u) . hiddenTableLine . hiddenInput (fieldName usernameField) . str . u_username . fromJust $ u

emptyAssignment :: Maybe Assignment
emptyAssignment = Nothing

instance GetValueHandler AssignmentKey where
  getValue = getParameter assignmentKeyPrm

instance GetValueHandler Assignment where
  getValue = do
    timeZone <- timezone <$> runStoryE Story.userState
    assignmentAna
      (getParameter (stringParameter (fieldName assignmentNameField) "Assignment Name"))
      (getParameter (stringParameter (fieldName assignmentDescField) "Assignment Description"))
      (getParameter assignmentTypePrm)
      (getParameter (assignmentStartPrm timeZone))
      (return timeZone)
      (getParameter (assignmentEndPrm   timeZone))
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

    name (BinEval ()) = "Binary"
    name (PctEval ()) = "Percentage"

-- TODO
dateInput :: String -> Maybe UTCTime -> Html
dateInput n v = required . setHookClass datePickerClass $ textInput n 10 (show <$> v)

hourInput :: String -> Maybe Int -> Html
hourInput n v = required . setHookClass hourSpinnerClass $ textInput n 2 (show <$> v)

minInput :: String -> Maybe Int -> Html
minInput n v = required . setHookClass minuteSpinnerClass $ textInput n 2 (show <$> v)
