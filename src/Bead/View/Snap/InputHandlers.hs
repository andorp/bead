module Bead.View.Snap.InputHandlers where

import Bead.Domain.Types (Str(..))
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
  inputPagelet g = table "create-group" $ do
    tableLine "Group Code" $ textInput (fieldName groupCodeField) 10 (fmap (str . groupCode) g)
    tableLine "Group Name" $ textInput (fieldName groupNameField) 10 (fmap groupName g)
    tableLine "Group Desc" $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)

instance GetValueHandler Group where
  getValue = do
    codeParam <- getParamE (fieldName groupCodeField) GroupCode "Group code is not found"
    nameParam <- getParamE (fieldName groupNameField) id "Group name is not found"
    descParam <- getParamE (fieldName groupDescField) id "Group description is not found"
    return $ Group {
        groupCode = codeParam
      , groupName = nameParam
      , groupDesc = descParam
      }

instance GetValueHandler CourseKey where
  getValue = getParamE (fieldName courseKeyInfo) CourseKey "Course key is not found"

instance GetValueHandler Course where
  getValue = do
    codeParam <- getParamE (fieldName courseCodeField) CourseCode "Course code is not found"
    nameParam <- getParamE (fieldName courseNameField) id "Course name is not found"
    descParam <- getParamE (fieldName courseDescField) id "Course description is not found"
    return Course {
        courseCode = codeParam
      , courseName = nameParam
      , courseDesc = descParam
      }

emptyCourse :: Maybe Course
emptyCourse = Nothing

instance InputPagelet Course where
  inputPagelet c = table "create-course" $ do
    tableLine "Course Code" $ textInput (fieldName courseCodeField) 10 (fmap (str . courseCode) c)
    tableLine "Course Name" $ textInput (fieldName courseNameField) 10 (fmap courseName c)
    tableLine "Course Desc" $ textInput (fieldName courseDescField) 10 (fmap courseDesc c)
