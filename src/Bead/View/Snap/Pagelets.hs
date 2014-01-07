{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Snap.Pagelets where

import Data.Maybe (isJust, fromJust)
import Data.String (IsString(..), fromString)
import Control.Monad (join, mapM_)

import Text.Blaze (ToMarkup(..), textTag)
import Text.Blaze.Html5 (Html, AttributeValue(..), (!))
--import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Bead.View.Snap.I18N (I18NHtml, translate)
import qualified Bead.View.Snap.I18N as I18N

import Bead.Domain.Types (Str(..))
import Bead.Domain.Entities
import Bead.Domain.Relationships
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf
import Bead.View.Snap.Style
import Bead.View.Snap.Dictionary (I18N)
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.I18N (IHtml, translate)
import qualified Bead.View.Snap.I18NHtml as H
#ifdef TEST
import Bead.Invariants (Invariants(..))
#endif

-- * Definitions

data Pagelet = Pagelet {
    struct :: I18NHtml
  }

emptyPagelet  = Pagelet { struct = return () }
onlyHtml h = Pagelet { struct = h }

structMap :: (I18NHtml -> I18NHtml) -> Pagelet -> Pagelet
structMap f p = p { struct = f . struct $ p }

css :: String -> IHtml
css c = H.link ! A.type_ "text/css" ! A.href (fromString c) ! A.rel "stylesheet"

js :: String -> IHtml
js j = H.script ! A.src (fromString j) $ empty

document :: IHtml -> IHtml -> IHtml
document headers body = H.docTypeHtml $ do
  H.head $ do
    H.title "BE-AD beadandókezelő"
    H.meta ! A.charset "UTF-8"
    H.link ! A.rel "shortcut icon" ! A.href "icon.ico"
    headers
    css "header.css"
  H.body $ body

dynamicDocument :: IHtml -> IHtml -> IHtml
dynamicDocument header = document
    (do css "jquery-ui.css"
        js "/jquery.js"
        js "/jquery-ui.js"
        js "/fay/DynamicContents.js"
        header)

runPagelet :: Pagelet -> I18N -> Html
runPagelet p i = translate i $ document (css "inside.css") (struct p)

runDynamicPagelet :: Pagelet -> I18N -> Html
runDynamicPagelet p i = translate i $
  dynamicDocument
    (css "inside.css")
    (struct p)

class BlazeTemplate b where
  template :: b -> Html

titleAndHead :: (IHtml -> IHtml -> IHtml) -> String -> IHtml -> IHtml
titleAndHead doc title content = doc
  (css "screen.css")
  (do H.div ! A.id "header" $ do
        H.div ! A.id "logo" $ "BE-AD"
        H.div ! A.id "title" $ (fromString title)
      H.div ! A.id "content" $ content)

dynamicTitleAndHead :: String -> IHtml -> IHtml
dynamicTitleAndHead = titleAndHead dynamicDocument

withTitleAndHead :: String -> IHtml -> IHtml
withTitleAndHead = titleAndHead document

withUserFrame :: UserState -> Pagelet -> Pagelet
withUserFrame s = structMap withUserFrame'
  where
    withUserFrame' content = do
      H.div ! A.id "header" $ pageHeader s
      pageStatus s
      H.div ! A.id "content" $ content

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

conditional :: Bool -> IHtml -> IHtml -> IHtml
conditional True _ visible = visible
conditional False text _   = text

nonEmpty :: [o] -> IHtml -> IHtml -> IHtml
nonEmpty os = conditional (not . null $ os)

-- * Input fields

charInput :: String -> String -> Int -> Maybe String -> IHtml
charInput t name size value =
  (H.input ! A.type_ (fromString t)
           ! A.id (fromString name)
           ! A.name (fromString name)
           ! A.size (fromString . show $ size))
  |> (withDefaultValue value)

textInput :: String -> Int -> Maybe String -> IHtml
textInput = charInput "text"

passwordInput :: String -> Int -> Maybe String -> IHtml
passwordInput = charInput "password"

textAreaInput :: String -> Maybe String -> IHtml
textAreaInput name value =
  (H.textarea ! A.name (fromString name)
              ! A.id   (fromString name)) value'
  where
    value' = fromString . maybe "" id $ value

hiddenInput :: String -> String -> IHtml
hiddenInput name value =
  H.input ! A.type_ "hidden"
          ! A.id (fromString name)
          ! A.name (fromString name)
          ! A.value (fromString value)

hiddenInputWithId :: String -> String -> IHtml
hiddenInputWithId n v = hiddenInput n v ! A.id (fromString n)

submitButton :: String -> String -> IHtml
submitButton i t = H.input ! A.id (fromString i) ! A.type_ "submit" ! A.value (fromString t)

withId :: (IHtml -> IHtml) -> String -> (IHtml -> IHtml)
withId f i = (f ! A.id (fromString i))

setHookClass c h = h ! A.class_ (className c)

required h = h ! A.required ""

-- * Form

postForm :: String -> IHtml -> IHtml
postForm action = H.form ! A.method "post" ! A.action (fromString action) ! A.acceptCharset "UTF-8"

getForm :: String -> IHtml -> IHtml
getForm action = H.form ! A.method "get" ! A.action (fromString action) ! A.acceptCharset "UTF-8"

-- * Table
table :: String -> String -> IHtml -> IHtml
table i c = H.table ! A.id (fromString i) ! A.class_ (fromString c)

tableLine :: String -> IHtml -> IHtml
tableLine title field = H.tr $ do
  H.td (fromString title)
  H.td field

hiddenTableLine :: IHtml -> IHtml
hiddenTableLine = H.tr . H.td

empty :: IHtml
empty = return ()

linkText :: (IsString s) => P.Page -> s
linkText P.Login      = fromString "Bejelentkezés"
linkText P.Logout     = fromString "Kijelentkezés"
linkText P.Home       = fromString "Főoldal"
linkText P.Profile    = fromString "Beállítások"
linkText P.Error      = fromString "Hiba"
linkText P.CourseAdmin = fromString "Tárgyi beállítások"
linkText P.Submission  = fromString "Beküldés"
linkText P.SubmissionList = fromString "Beadott megoldások"
linkText P.UserSubmissions = fromString "Megoldások"
linkText (P.ModifyEvaluation _ _) = fromString "Értékelés"
linkText (P.SubmissionDetails _ _) = fromString "Megoldás"
linkText P.Administration  = fromString "Adminisztráció"
linkText (P.Evaluation _)  = fromString "Értékelés"
linkText P.EvaluationTable = fromString "Értékelések"
linkText P.GroupRegistration = fromString "Tárgy vagy csoport felvétele"
linkText P.CreateCourse       = fromString "Tárgy létrehozása"
linkText P.UserDetails = fromString "Beállítások"
linkText P.AssignCourseAdmin = fromString "Tárgyfelelős hozzáadása"
linkText P.CreateGroup = fromString "Csoport létrehozása"
linkText P.AssignGroupAdmin = fromString "Oktató hozzáadása"
linkText P.NewGroupAssignment  = fromString "Új csoportszintű feladat"
linkText P.NewCourseAssignment = fromString "Új tárgyszintű feladat"
linkText P.ModifyAssignment = fromString "Feladat módosítása"
linkText P.ChangePassword = fromString "Jelszócsere"
linkText P.SetUserPassword = fromString "Hallgató jelszavának beállítása"
linkText (P.CommentFromEvaluation _) = fromString "Megjegyzés"
linkText (P.CommentFromModifyEvaluation _ _) = fromString "Megjegyzés"

linkToPage :: P.Page -> IHtml
linkToPage g = H.a ! A.href (routeOf g) ! A.id (fieldName g) $ linkText g

linkToPageWithText :: P.Page -> String -> IHtml
linkToPageWithText g t = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString t

link :: String -> String -> IHtml
link r t = H.a ! A.href (fromString r) $ fromString t

-- Produces a HTML-link with the given route text and title
linkWithTitle :: String -> String -> String -> IHtml
linkWithTitle route title text =
  H.a ! A.href (fromString route)
      ! A.title (fromString title)
      $ fromString text

linkToRoute :: String -> IHtml
linkToRoute = link "/"

navigationMenu :: UserState -> IHtml
navigationMenu s = do
  H.ul $ mapM_ (H.li . linkToPage) $ P.menuPages (role s) (page s)

pageHeader :: UserState -> IHtml
pageHeader s = do
  H.div ! A.id "logo" $ "BE-AD"
  H.div ! A.id "user" $ do
    (fromString . str . user $ s)
    H.br
    linkToPage P.Home
    H.br
    linkToPage P.Profile
    H.br
    linkToPage P.Logout
  H.div ! A.id "title" $ title s
  where
    title u@(UserState {}) = linkText . page $ u
    title _ = ""

pageStatus :: UserState -> IHtml
pageStatus = maybe noMessage message . status
  where
    noMessage = return ()
    message m = H.div ! A.id "status" # backgroundColor "yellow" $ H.span $ (fromString m)

-- * Picklist

option :: String -> String -> Bool -> IHtml
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection :: String -> IHtml -> IHtml
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

instance (SelectionValue v, SelectionText t) => SelectionValue (v,x,t) where
  selectionValue (v,_,_) = selectionValue v

instance (SelectionValue v, SelectionText t) => SelectionText (v,x,t) where
  selectionText (_,_,t) = selectionText t

instance SelectionText String where
  selectionText = id

defValueTextSelection :: (Eq s, SelectionValue s, SelectionText s) => String -> s -> [s] -> IHtml
defValueTextSelection name def = selection name . mapM_ option' where
  option' x = option (selectionValue x) (selectionText x) (x == def)

valueTextSelection :: (SelectionValue s, SelectionText s) => String -> [s] -> IHtml
valueTextSelection name = selection name . mapM_ option'
  where
    option' s = option (selectionValue s) (selectionText s) False

-- Creates a selection from enum values, starting enumeration from the
-- given value
enumSelection :: (Enum e, SelectionValue e, SelectionText e) => String -> e -> IHtml
enumSelection name start = valueTextSelection name [start .. ]

-- Creates a selection with a given name attribute and a default value
-- as the actively selected one.
defEnumSelection :: (Eq e, Enum e, SelectionValue e, SelectionText e) => String -> e -> IHtml
defEnumSelection name def = defValueTextSelection name def [toEnum 0 .. ]

valueSelection :: (o -> (String, String)) -> String -> [o] -> IHtml
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

instance SelectionValue TimeZone where
  selectionValue = show

instance SelectionText TimeZone where
  selectionText = show

evalSelectionDiv :: EvaluationHook -> IHtml
evalSelectionDiv h = (H.div `withId` (evSelectionDivId h)) $ empty

#ifdef TEST

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = linkText

#endif
