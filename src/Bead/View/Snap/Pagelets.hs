{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..), fromString)
import Control.Monad (join, mapM_)

import Text.Blaze (ToMarkup(..), textTag)
import Text.Blaze.Html5 (Html, AttributeValue(..), (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities
import Bead.Domain.Relationships
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf
import Bead.View.Snap.Dictionary (I18N)
import Bead.View.Snap.TemplateAndComponentNames

import Bead.Invariants (Invariants(..))

-- Definitions --

newtype I18NHtml = I18NHtml { un18n :: I18N -> Html }

internationalization :: I18NHtml -> I18N -> Html
internationalization (I18NHtml h) i = h i

i18nHtml :: (I18N -> Html) -> I18NHtml
i18nHtml h = I18NHtml h

class BlazeTemplate b where
  template :: b -> Html

withTitleAndHead :: Html -> Html
withTitleAndHead content = H.docTypeHtml $ do
  H.head $ H.title "Snap web server"
  H.body $ do
    H.div ! A.id "content" $ content

withUserFrame :: UserState -> Html -> Html
withUserFrame s content = H.docTypeHtml $ do
  H.head $ H.title "Snap web server"
  H.body $ do
    H.div ! A.id "header" $ pageHeader s
    H.div ! A.id "menu" $ navigationMenu s
    H.div ! A.id "content" $ do
      "Content"
      content

withI18NUserFrame :: UserState -> I18NHtml -> I18N -> Html
withI18NUserFrame s h i = withUserFrame s (internationalization h i)

-- * Basic building blocks

defaultValue :: a -> Maybe a
defaultValue = Just

hasNoDefaultValue :: Maybe a
hasNoDefaultValue = Nothing

withDefaultValue Nothing  h = h
withDefaultValue (Just v) h = h ! A.value (fromString v)

html :: I18N -> String -> Html
html i18n = fromString . i18n

infix |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- * Input fields

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

-- * Form

postForm :: String -> Html -> Html
postForm action = H.form ! A.method "post" ! A.action (fromString action)

getForm :: String -> Html -> Html
getForm action = H.form ! A.method "get" ! A.action (fromString action)

-- * Table

table :: String -> Html -> Html
table i = H.table ! A.id (fromString i)

tableLine :: String -> Html -> Html
tableLine title field = H.tr $ do
  H.td (fromString title)
  H.td field

hiddenTableLine :: Html -> Html
hiddenTableLine = H.tr . H.td

empty :: Html
empty = return ()

errorPage :: Html
errorPage = withUserFrame (error "errorPage: undefined") "Error page is not defined"

linkText :: (IsString s) => P.Page -> s
linkText P.Login      = fromString "Login"
linkText P.Logout     = fromString "Logout"
linkText P.Home       = fromString "Home"
linkText P.Profile    = fromString "Profile"
linkText P.Error      = fromString "Error"
linkText P.CourseAdmin = fromString "CourseAdmin"
linkText P.Submission  = fromString "Submission"
linkText P.SubmissionList = fromString "Submission List"
linkText P.UserSubmissions = fromString "User Submission"
linkText P.ModifyEvaulation = fromString "Evaulation"
linkText P.SubmissionDetails = fromString "Submission Details"
linkText P.Administration  = fromString "Administration"
linkText P.Evaulation      = fromString "Evaulation"
linkText P.EvaulationTable = fromString "Evaulation"
linkText P.GroupRegistration = fromString "Register For A Course / Group"
linkText P.CreateCourse       = fromString "Create A course"
linkText P.UserDetails = fromString "User's Detail"
linkText P.AssignCourseAdmin = fromString "Add admin to the course"
linkText P.CreateGroup = fromString "Create a Group"
linkText P.AssignProfessor = fromString "Add professor to the group"
linkText P.NewGroupAssignment  = fromString "Create a New Group Assignment"
linkText P.NewCourseAssignment = fromString "Create a New Course Assignment"
linkText P.ModifyAssignment = fromString "Modify Assignment"

linkToPage :: P.Page -> Html
linkToPage g = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ linkText g

linkToPageWithText :: P.Page -> String -> Html
linkToPageWithText g t = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString t

link :: String -> String -> Html
link r t = H.a ! A.href (fromString r) $ fromString t

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

instance ButtonText AssignmentKey where
  buttonText (AssignmentKey e) = e

instance KeyString AssignmentKey where
  keyString (AssignmentKey e) = e

exerciseKeys :: String -> [AssignmentKey] -> Html
exerciseKeys act = keySelectionForm exerciseFormData act where
  exerciseFormData = KeyFormData {
      divId   = "assignment"
    , tableId = "assignment-table"
    , key     = fieldName exerciseKey
    , parent  = Nothing
    , title   = "Assignments for the user"
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

-- * Picklist

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection :: String -> Html -> Html
selection name = H.select ! A.name (fromString name) ! A.multiple "false"

class SelectionValue v where
  selectionValue :: v -> String

class SelectionText t where
  selectionText :: t -> String

instance (SelectionValue v, SelectionText t) => SelectionValue (v,t) where
  selectionValue (v,_) = selectionValue v

instance (SelectionValue v, SelectionText t) => SelectionText (v,t) where
  selectionText (_,t) = selectionText t

valueTextSelection :: (SelectionValue s, SelectionText s) => String -> [s] -> Html
valueTextSelection name = selection name . mapM_ option'
  where
    option' s = option (selectionValue s) (selectionText s) False

enumSelection :: (Enum e, SelectionValue e, SelectionText e) => String -> e -> Html
enumSelection name start = valueTextSelection name [start .. ]

-- SelectionValue and SelectionText instances

instance SelectionValue CourseKey where
  selectionValue (CourseKey k) = k

instance SelectionText Course where
  selectionText = courseName

instance SelectionValue User where
  selectionValue = str . u_username

instance SelectionText User where
  selectionText u = join [str . u_username $ u, " - ", u_name u]

instance SelectionText Group where
  selectionText = groupName

instance SelectionValue GroupKey where
  selectionValue (GroupKey k) = k

instance SelectionText AssignmentType where
  selectionText = show

instance SelectionValue AssignmentType where
  selectionValue = show

instance SelectionText EvaulationType where
  selectionText = show

instance SelectionValue EvaulationType where
  selectionValue = show

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = linkText
