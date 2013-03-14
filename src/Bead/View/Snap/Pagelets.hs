{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..), fromString)
import Control.Monad (mapM_)

import Text.Blaze (ToMarkup(..), textTag)
import Text.Blaze.Html5 (Html, AttributeValue(..), (!))
import qualified Text.Blaze.Html5 as H
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
withTitleAndHead content = H.docTypeHtml $ do
  H.head $ H.title "Snap web server"
  H.body $ do
    H.div ! A.id "content" $ content

withUserFrame :: UserState -> Html -> Maybe Html -> Html
withUserFrame s content loggedInContent = H.docTypeHtml $ do
  H.head $ H.title "Snap web server"
  H.body $ do
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

-- * Basic building blocks

defaultValue :: a -> Maybe a
defaultValue = Just

hasNoDefaultValue :: Maybe a
hasNoDefaultValue = Nothing

withDefaultValue Nothing  h = h
withDefaultValue (Just v) h = h ! A.value (fromString v)

infix |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

textInput :: String -> Int -> Maybe String -> Html
textInput name size value =
  (H.input ! A.type_ "text" ! A.name (fromString name)) |> (withDefaultValue value)

textAreaInput :: String -> Int -> Int -> Maybe String -> Html
textAreaInput name cols rows value =
  (H.input ! A.type_ "textarea"
           ! A.name (fromString name)
           ! A.cols (fromString . show $ cols)
           ! A.rows (fromString . show $ rows))
  |>
  (withDefaultValue value)

hiddenInput :: String -> String -> Html
hiddenInput name value =
  H.input ! A.type_ "hidden"
          ! A.name (fromString name)
          ! A.value (fromString value)

submitButton :: String -> Html
submitButton t = H.input ! A.type_ "submit" ! A.value (fromString t)

postForm :: String -> Html -> Html
postForm action = H.form ! A.method "post" ! A.action (fromString action)

getForm :: String -> Html -> Html
getForm action = H.form ! A.method "get" ! A.action (fromString action)

table :: String -> Html -> Html
table i = H.table ! A.id (fromString i)

tableLine :: String -> Html -> Html
tableLine title field = H.tr $ do
  H.td (fromString title)
  H.td field

-- * 

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
    divId   :: String
  , tableId :: String
  , key     :: String
  , parent  :: Maybe (String, String)
  , title   :: Html
  }

hiddenGroupKeyInput :: GroupKey -> Html
hiddenGroupKeyInput k = hiddenInput (fieldName groupKeyName) (keyString k)

hiddenCourseKeyInput :: CourseKey -> Html
hiddenCourseKeyInput k = hiddenInput (fieldName courseKeyInfo) (keyString k)

keySelectionForm :: (ButtonText k, KeyString k) => KeyFormData -> String -> [k] -> Html
keySelectionForm formData act ks = do
    (title formData)
    mapM_ keyDivForm ks
  where
  keyDivForm k =
    let parentRef = parentReference (parent formData) in
    H.div ! A.id (fromString . divId $ formData) $ do
      getForm act $ do
        table (tableId formData) $
          H.tr $ do
            parentRef
            H.td $ hiddenInput (key formData) (keyString k)
            H.td $ submitButton (buttonText k)

  parentReference Nothing      = return ()
  parentReference (Just (k,v)) = H.td $ hiddenInput k v

instance ButtonText ExerciseKey where
  buttonText (ExerciseKey e) = e

instance KeyString ExerciseKey where
  keyString (ExerciseKey e) = e

exerciseKeys :: String -> [ExerciseKey] -> Html
exerciseKeys act = keySelectionForm exerciseFormData act where
  exerciseFormData = KeyFormData {
      divId   = "exercise"
    , tableId = "exercise-table"
    , key     = fieldName exerciseKey
    , parent  = Nothing
    , title   = "Exercises for the user"
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

courseKeys :: String -> [CourseKey] -> Html
courseKeys act = keySelectionForm courseFormData act where
  courseFormData = KeyFormData {
      divId   = "course"
    , tableId = "course-table"
    , key     = fieldName courseKeyInfo
    , parent  = Nothing
    , title   = "Courses"
    }

instance ButtonText GroupKey where
  buttonText (GroupKey g) = g

instance KeyString GroupKey where
  keyString (GroupKey g) = g

groupKeys :: String -> CourseKey -> [GroupKey] -> Html
groupKeys act ck = keySelectionForm groupFormData act where
  groupFormData = KeyFormData {
      divId   = "group"
    , tableId = "group-table"
    , key     = fieldName groupKeyName
    , parent  = Just (fieldName courseKeyInfo, keyString ck)
    , title   = "Groups"
    }

instance ButtonText Username where
  buttonText (Username n) = n

instance KeyString Username where
  keyString (Username n) = n

userKeys :: String -> [Username] -> Html
userKeys act = keySelectionForm userFormData act where
  userFormData = KeyFormData {
      divId   = "users"
    , tableId = "users"
    , key     = fieldName usernameField
    , parent  = Nothing
    , title   = "Users"
    }

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection :: String -> Html -> Html
selection name = H.select ! A.name (fromString name) ! A.multiple "false"

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = linkText
