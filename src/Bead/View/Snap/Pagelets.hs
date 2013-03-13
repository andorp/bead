{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Data.String (IsString(..), fromString)
import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities (Username(..), Role(..))
import Bead.Domain.Relationships
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf
import Bead.View.Snap.TemplateAndComponentNames

import Bead.Invariants (Invariants(..))

-- Definitions --

class BlazeTemplate b where
  template :: b -> Html

withTitleAndHead :: Html -> Html
withTitleAndHead content = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "content" $ content

withUserFrame :: UserState -> Html -> Maybe Html -> Html
withUserFrame s content loggedInContent = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "header" $ pageHeader s
    H.div ! A.id "menu" $ navigationMenu s
    H.div ! A.id "content" $ do
      "Content"
      content
    case loggedInContent of
      Nothing -> return ()
      Just inner  -> H.div $ do
        H.div ! A.id "menu1" $ do
          inner

empty :: Html
empty = return ()

errorPage :: Html
errorPage = withUserFrame undefined "Error page is not defined" Nothing

linkText :: (IsString s) => P.Page -> s
linkText P.Login      = fromString "Login"
linkText P.Logout     = fromString "Logout"
linkText P.Home       = fromString "Home"
linkText P.Profile    = fromString "Profile"
linkText P.Course     = fromString "Course"
linkText P.Courses    = fromString "Courses"
linkText P.Group      = fromString "Group"
linkText P.Groups     = fromString "Groups"
linkText P.Exercise   = fromString "Exercise"
linkText P.ClosedExam = fromString "Closed Exam"
linkText P.Error      = fromString "Error"
linkText P.SubmitExam = fromString "Submit Exam"
linkText P.Evaulation = fromString "Evaulation"
linkText P.Training   = fromString "Training"
linkText P.Users      = fromString "Users"
linkText P.UserDetails = fromString "UserDetails"
linkText P.CreateExercise = fromString "Create Exercise"
linkText P.CreateCourse   = fromString "Create Course"
linkText P.CreateGroup    = fromString "Create Group"
linkText P.Admin      = fromString "Admin"

linkToPage :: P.Page -> Html
linkToPage g = H.p $ H.a ! A.href (routeOf g) $ linkText g

navigationMenu :: UserState -> Html
navigationMenu s = do
  H.p $ "Menu"
  mapM_ (linkToPage) $ P.menuPages (role s) (page s)
  H.p $ "Menu"

class ButtonText b where
  buttonText :: b -> String

class KeyString b where
  keyString :: b -> String

data KeyFormData = KeyFormData {
    divId   :: AttributeValue
  , tableId :: AttributeValue
  , key     :: AttributeValue
  , parent  :: Maybe (AttributeValue, AttributeValue)
  , formTitle :: Html
  }

hiddenGroupKeyInput :: GroupKey -> Html
hiddenGroupKeyInput k =
  H.input ! A.type_ "hidden" ! A.name (fieldName groupKeyName) ! A.value (fromString . keyString $ k)

hiddenCourseKeyInput :: CourseKey -> Html
hiddenCourseKeyInput k =
  H.input ! A.type_ "hidden" ! A.name (fieldName courseKeyInfo) ! A.value (fromString . keyString $ k)

keySelectionForm :: (ButtonText k, KeyString k) => KeyFormData -> AttributeValue -> [k] -> Html
keySelectionForm formData act ks = do
    (formTitle formData)
    mapM_ keyDivForm ks
  where
  keyDivForm k =
    let parentRef = parentReference (parent formData) in
    H.div ! A.id (divId formData) $ do
      parentReference (parent formData)
      H.form ! A.method "get" ! A.action act $ do
        H.table ! A.id (tableId formData) $ do
          H.tr $ do
            parentRef
            H.td $ H.input ! A.type_ "hidden" ! A.name (key formData) ! A.value (fromString . keyString $ k)
            H.td $ H.input ! A.type_ "submit" ! A.value (fromString . buttonText $ k)

  parentReference Nothing      = return ()
  parentReference (Just (k,v)) = H.td $ H.input ! A.type_ "hidden" ! A.name k ! A.value v

instance ButtonText ExerciseKey where
  buttonText (ExerciseKey e) = e

instance KeyString ExerciseKey where
  keyString (ExerciseKey e) = e

exerciseKeys :: AttributeValue -> [ExerciseKey] -> Html
exerciseKeys act = keySelectionForm exerciseFormData act where
  exerciseFormData = KeyFormData {
      divId   = "exercise"
    , tableId = "exercise-table"
    , key     = fieldName exerciseKey
    , parent  = Nothing
    , formTitle = "Exercises for the user"
    }

pageHeader :: UserState -> Html
pageHeader s = do
  H.p $ "Header"
  H.span $ do { "Welcome "; fromString . str . user $ s ; "!"  }
  linkToPage P.Logout
  H.p $ "Header"

instance ButtonText CourseKey where
  buttonText (CourseKey e) = e

instance KeyString CourseKey where
  keyString (CourseKey e) = e

courseKeys :: AttributeValue -> [CourseKey] -> Html
courseKeys act = keySelectionForm courseFormData act where
  courseFormData = KeyFormData {
      divId   = "course"
    , tableId = "course-table"
    , key     = fieldName courseKeyInfo
    , parent  = Nothing
    , formTitle = "Courses"
    }

instance ButtonText GroupKey where
  buttonText (GroupKey g) = g

instance KeyString GroupKey where
  keyString (GroupKey g) = g

groupKeys :: AttributeValue -> CourseKey -> [GroupKey] -> Html
groupKeys act ck = keySelectionForm groupFormData act where
  groupFormData = KeyFormData {
      divId   = "group"
    , tableId = "group-table"
    , key     = fieldName groupKeyName
    , parent  = Just (fieldName courseKeyInfo, fromString . keyString $ ck)
    , formTitle = "Groups"
    }

instance ButtonText Username where
  buttonText (Username n) = n

instance KeyString Username where
  keyString (Username n) = n

userKeys :: AttributeValue -> [Username] -> Html
userKeys act = keySelectionForm userFormData act where
  userFormData = KeyFormData {
      divId   = "users"
    , tableId = "users"
    , key     = fieldName usernameField
    , parent  = Nothing
    , formTitle = "Users"
    }
    
class Selection t where
  selectionValue :: (IsString s) => t -> s
  selectionText  :: (IsString s) => t -> s

selection :: (Eq s, Selection s) => AttributeValue -> s -> [s] -> Html
selection name s = (H.select ! A.name name ! A.multiple "false") . mapM_ opt
  where
    -- TODO
    opt s'
      | s == s'   = (H.option ! A.value (selectionValue s')) $ (selectionText s')
      | otherwise = (H.option ! A.value (selectionValue s')) $ (selectionText s')

instance Selection Role where
  selectionValue = fromString . show
  selectionText  = fromString . show

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = linkText
