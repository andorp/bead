module Bead.View.Snap.InputHandlers where

import Control.Applicative ((<$>))
import Data.Maybe (maybe, isJust, fromJust)
import Data.Time (UTCTime(..))

import Bead.Domain.Types (Str(..), readMaybe)
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Bead.View.Snap.Application (App(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.TemplateAndComponentNames

import Text.Blaze.Html5 (Html(..))

-- * Input pagelet and handler

class GetValueHandler i where
  getValue :: HandlerError App App i

class InputPagelet i where
  inputPagelet :: Maybe i -> Html

-- * Instances

instance GetValueHandler GroupKey where
  getValue = getParamE (fieldName groupKeyName) GroupKey "Group key is not found"

emptyGroup :: Maybe Group
emptyGroup = Nothing

instance InputPagelet Group where
  inputPagelet g = table "create-group" "create-group-table" $ do
    tableLine "Group Name" $ textInput (fieldName groupNameField) 10 (fmap groupName g)
    tableLine "Group Desc" $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)

instance GetValueHandler Group where
  getValue = do
    nameParam <- getParamE (fieldName groupNameField) id "Group name is not found"
    descParam <- getParamE (fieldName groupDescField) id "Group description is not found"
    return $ Group {
        groupName = nameParam
      , groupDesc = descParam
      }

instance GetValueHandler CourseKey where
  getValue = getParamE (fieldName courseKeyInfo) CourseKey "Course key is not found"

instance GetValueHandler Course where
  getValue = do
    nameParam <- getParamE (fieldName courseNameField) id "Course name is not found"
    descParam <- getParamE (fieldName courseDescField) id "Course description is not found"
    return Course {
        courseName = nameParam
      , courseDesc = descParam
      }

emptyCourse :: Maybe Course
emptyCourse = Nothing

instance InputPagelet Course where
  inputPagelet c = table "create-course" "create-course-table" $ do
    tableLine "Course Name" $ textInput (fieldName courseNameField) 10 (fmap courseName c)
    tableLine "Course Desc" $ textInput (fieldName courseDescField) 10 (fmap courseDesc c)

emptyRole :: Maybe Role
emptyRole = Nothing

instance InputPagelet Role where
  inputPagelet q = selection (fieldName userRoleField) $ mapM_ (roleOptions q) roles
    where
      roleOptions Nothing   r = option (show r) (show r) False
      roleOptions (Just q') r = option (show r) (show r) (q' == r)

instance GetValueHandler Role where
  getValue = getParamE (fieldName userRoleField) id "Role is not defined" >>= createRole where

    createRole :: String -> HandlerError App App Role
    createRole s = do
      case parseRole s of
        Just r  -> return r
        Nothing -> throwError . strMsg $ "Role was not parseable: " ++ s

emptyUsername :: Maybe Username
emptyUsername = Nothing

instance InputPagelet Username where
  inputPagelet u = textInput (fieldName usernameField) 20 (fmap str u)

instance GetValueHandler Username where
  getValue = getParamE (fieldName usernameField) Username "Username is not found"

emptyUser :: Maybe User
emptyUser = Nothing

instance GetValueHandler User where
  getValue = do
    role     <- getValue
    username <- getValue
    email    <- getParamE (fieldName userEmailField) Email   "Email is not found"
    fname    <- getParamE (fieldName userFamilyNameField) id   "Full name is not found"
    return $ User {
        u_role = role
      , u_username = username
      , u_email = email
      , u_name = fname
      }

instance InputPagelet User where
  inputPagelet u = table "user-detail-table" "user-detail-table" $ do
    tableLine "User's role'"    $ inputPagelet (fmap u_role u)
    tableLine "User's email"    $ textInput (fieldName userEmailField)      20 (fmap (str . u_email) u)
    tableLine "User's fullname" $ textInput (fieldName userFamilyNameField) 20 (fmap u_name u)
    when (isJust u) . hiddenTableLine . hiddenInput (fieldName usernameField) . str . u_username . fromJust $ u

emptyAssignment :: Maybe Assignment
emptyAssignment = Nothing

instance GetValueHandler AssignmentKey where
  getValue = getParamE (fieldName assignmentKeyField) AssignmentKey "Assignment key was not found"

instance GetValueHandler Assignment where
  getValue = do
    name  <- getParamE (fieldName assignmentNameField) id "Assignment Name is not found"
    desc  <- getParamE (fieldName assignmentDescField) id "Assignment Description is not found"
    tcs   <- getParamE (fieldName assignmentTCsField) id "Assignment TCs is not found"
    tp    <- getParamE (fieldName assignmentTypeField) (readMsg "Assignment type") "Assignment Type is not found"
    start <- getParamE (fieldName assignmentStartField) (readMsg "Assignment start") "Assignment Start is not found"
    end   <- getParamE (fieldName assignmentEndField) (readMsg "Assignment end") "Assignment End is not found"
    ev    <- getParamE (fieldName assignmentEvField) (readMsg "Assignment evaulation") "Assignment Evaulation type is not found"
    return $ Assignment {
        assignmentDesc = desc
      , assignmentName = name
      , assignmentTCs  = tcs
      , assignmentType = tp
      , assignmentStart = start
      , assignmentEnd   = end
      , evaulationType  = ev
      }
  
-- * Combined input fields

utcTimeInput :: String -> Maybe UTCTime -> Html
utcTimeInput n v = textInput n 10 (show <$> v)

utcTimeParam :: String -> HandlerError App App (Maybe UTCTime)
utcTimeParam n = getParamE n readMaybe "UTC Time field was not found"
