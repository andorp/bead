{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..), fromString)
import Control.Monad (join, mapM_)

import Text.Blaze (ToMarkup(..), textTag)
import Text.Blaze.Html5 (Html, AttributeValue(..), (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Bead.View.Snap.I18N (I18NHtml, translate)
import qualified Bead.View.Snap.I18N as I18N

import Text.CSS

import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities
import Bead.Domain.Relationships
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf
import Bead.View.Snap.Dictionary (I18N)
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds

import Bead.Invariants (Invariants(..))

-- * Definitions

data Pagelet = Pagelet {
    struct :: I18NHtml
  , style  :: Css
  }

emptyPagelet  = Pagelet { struct = return () , style = return () }
onlyHtml h = Pagelet { struct = h, style = return () }

structMap :: (I18NHtml -> I18NHtml) -> Pagelet -> Pagelet
structMap f p = p { struct = f . struct $ p }

styleMap :: (Css -> Css) -> Pagelet -> Pagelet
styleMap f p = p { style = f . style $ p }

css :: String -> Html
css c = H.link ! A.type_ "text/css" ! A.href (fromString c) ! A.rel "stylesheet"

js :: String -> Html
js j = H.script ! A.src (fromString j) $ empty

document :: Html -> Html -> Html
document headers body = H.docTypeHtml $ do
  H.head $ do
    H.title "Bead"
    headers
    css "header.css"
  H.body $ body

dynamicDocument :: Html -> Html -> Html
dynamicDocument header = document
    (do css "jquery-ui.css"
        js "/jquery.js"
        js "/jquery-ui.js"
        js "/fay/DynamicContents.js"
        header)

runPagelet :: Pagelet -> I18N -> Html
runPagelet p i = document (css "inside.css") (translate i . struct $ p)

runDynamicPagelet :: Pagelet -> I18N -> Html
runDynamicPagelet p i =
  dynamicDocument
    (do css "inside.css"
        H.style ! A.type_ "text/css" $ fromString $ renderCSS (style p))
    (translate i . struct $ p)

class BlazeTemplate b where
  template :: b -> Html

titleAndHead :: (Html -> Html -> Html) -> String -> Html -> Html
titleAndHead doc title content = doc
  (css "screen.css")
  (do H.div ! A.id "header" $ do
        H.div ! A.id "logo" $ "Bead"
        H.div ! A.id "title" $ (fromString title)
      H.div ! A.id "content" $ content)

dynamicTitleAndHead :: String -> Html -> Html
dynamicTitleAndHead = titleAndHead dynamicDocument

withTitleAndHead :: String -> Html -> Html
withTitleAndHead = titleAndHead document

withUserFrame :: UserState -> Pagelet -> Pagelet
withUserFrame s = structMap withUserFrame'
  where
    withUserFrame' content = I18N.liftH2 $ \i -> do
      H.div ! A.id "header" $ pageHeader s
      H.div ! A.id "menu" $ navigationMenu s
      H.div ! A.id "content" $ translate i content

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

conditional :: Bool -> Html -> Html -> Html
conditional True _ visible = visible
conditional False text _   = text

nonEmpty :: [o] -> Html -> Html -> Html
nonEmpty os = conditional (not . null $ os)

-- * Input fields

charInput :: String -> String -> Int -> Maybe String -> Html
charInput t name size value =
  (H.input ! A.type_ (fromString t)
           ! A.id (fromString name)
           ! A.name (fromString name)
           ! A.size (fromString . show $ size))
  |> (withDefaultValue value)

textInput :: String -> Int -> Maybe String -> Html
textInput = charInput "text"

passwordInput :: String -> Int -> Maybe String -> Html
passwordInput = charInput "password"

textAreaInput :: String -> Maybe String -> Html
textAreaInput name value =
  (H.textarea ! A.name (fromString name)
              ! A.id   (fromString name)) value'
  where
    value' = fromString . maybe "" id $ value

hiddenInput :: String -> String -> Html
hiddenInput name value =
  H.input ! A.type_ "hidden"
          ! A.id (fromString name)
          ! A.name (fromString name)
          ! A.value (fromString value)

hiddenInputWithId :: String -> String -> Html
hiddenInputWithId n v = hiddenInput n v ! A.id (fromString n)

submitButton :: String -> String -> Html
submitButton i t = H.input ! A.id (fromString i) ! A.type_ "submit" ! A.value (fromString t)

withId :: (Html -> Html) -> String -> (Html -> Html)
withId f i = (f ! A.id (fromString i))

setHookClass c h = h ! A.class_ (className c)

required h = h ! A.required ""

-- * Form

postForm :: String -> Html -> Html
postForm action = H.form ! A.method "post" ! A.action (fromString action)

getForm :: String -> Html -> Html
getForm action = H.form ! A.method "get" ! A.action (fromString action)

-- * Table
table :: String -> String -> Html -> Html
table i c = H.table ! A.id (fromString i) ! A.class_ (fromString c)

table' :: String -> Html -> Html
table' i = H.table ! A.id (fromString i)

tableLine :: String -> Html -> Html
tableLine title field = H.tr $ do
  H.td (fromString title)
  H.td field

hiddenTableLine :: Html -> Html
hiddenTableLine = H.tr . H.td

empty :: Html
empty = return ()

linkText :: (IsString s) => P.Page -> s
linkText P.Login      = fromString "Login"
linkText P.Logout     = fromString "Logout"
linkText P.Home       = fromString "Home"
linkText P.Profile    = fromString "Profile"
linkText P.Error      = fromString "Error"
linkText P.CourseAdmin = fromString "Course Administration"
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
linkToPage g = H.a ! A.href (routeOf g) ! A.id (fieldName g) $ linkText g

linkToPageWithText :: P.Page -> String -> Html
linkToPageWithText g t = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString t

link :: String -> String -> Html
link r t = H.a ! A.href (fromString r) $ fromString t

navigationMenu :: UserState -> Html
navigationMenu s = do
  H.ul $ mapM_ (H.li . linkToPage) $ P.menuPages (role s) (page s)

pageHeader :: UserState -> Html
pageHeader s = do
  H.div ! A.id "logo" $ "Bead"
  H.div ! A.id "user" $ do
    fromString . str . user $ s
    H.br
    linkToPage P.Logout
  H.div ! A.id "title" $ title s
  where
    title u@(UserState {}) = linkText . page $ u
    title _ = ""

-- * Picklist

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection :: String -> Html -> Html
selection name =
  H.select ! A.id (fromString name) ! A.name (fromString name)
           ! A.multiple "false" ! A.required ""

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

valueSelection :: (o -> (String, String)) -> String -> [o] -> Html
valueSelection f n = selection n . mapM_ option'
  where
    option' o = let (v,n) = f o in option v n False

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

evalSelectionDiv :: EvaulationHook -> Html
evalSelectionDiv h = (H.div `withId` (evSelectionDivId h)) $ empty

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = linkText
