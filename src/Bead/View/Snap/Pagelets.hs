{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Snap.Pagelets where

import           Control.Monad (join)
import           Data.Char (isAlphaNum)
import           Data.String (IsString(..), fromString)
import           Data.Time.Clock

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext (UserState(..))
import           Bead.Domain.Entities
import           Bead.Domain.Relationships
import           Bead.Domain.Types (Str(..))
import           Bead.View.Snap.Fay.Hooks
import qualified Bead.View.Snap.I18N as I18N
import           Bead.View.Snap.I18N (IHtml, translate, getI18N)
import           Bead.View.Snap.RouteOf
import           Bead.View.Snap.Style
import           Bead.View.Snap.TemplateAndComponentNames
import           Bead.View.Snap.Translation
#ifdef TEST
import           Bead.Invariants (Invariants(..))
#endif

-- * Definitions

css :: String -> Html
css c = H.link ! A.type_ "text/css" ! A.href (fromString c) ! A.rel "stylesheet"

js :: String -> Html
js j = H.script ! A.src (fromString j) $ empty

document :: Html -> IHtml -> IHtml
document headers body' = do
  body <- body'
  return $ do
    H.docTypeHtml $ do
      H.head $ do
        H.title "BE-AD beadandókezelő"
        H.meta ! A.charset "UTF-8"
        H.link ! A.rel "shortcut icon" ! A.href "icon.ico"
        headers
        css "header.css"
      H.body $ body

dynamicDocument :: Html -> IHtml -> IHtml
dynamicDocument header body = document
    (do css "jquery-ui.css"
        js "/jquery.js"
        js "/jquery-ui.js"
        js "/fay/DynamicContents.js"
        header)
    body

runPagelet :: IHtml -> I18N -> Html
runPagelet p i = translate i $ document (css "inside.css") p

runDynamicPagelet :: IHtml -> I18N -> Html
runDynamicPagelet p i = translate i $ dynamicDocument (css "inside.css") p

titleAndHead :: (Html -> IHtml -> IHtml) -> Translation String -> IHtml -> IHtml
titleAndHead doc title content = doc
  (css "screen.css")
  (do msg <- getI18N
      content <- content
      return $ do
        H.div ! A.id "header" $ do
          H.div ! A.id "logo" $ "BE-AD"
          H.div ! A.id "title" $ fromString $ msg title
        H.div ! A.id "content" $ content)

dynamicTitleAndHead :: Translation String -> IHtml -> IHtml
dynamicTitleAndHead = titleAndHead dynamicDocument

withTitleAndHead :: Translation String -> IHtml -> IHtml
withTitleAndHead = titleAndHead document

withUserFrame :: UserState -> IHtml -> Int -> IHtml
withUserFrame s content secs = withUserFrame' content
  where
    withUserFrame' content = do
      header <- pageHeader s secs
      content <- content
      status <- pageStatus s
      return $ do
        H.div ! A.id "header" $ header
        status
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

fileInput :: String -> Html
fileInput name =
  H.input ! A.type_ "file"
          ! A.id (fromString name)
          ! A.name (fromString name)

submitButton :: String -> String -> Html
submitButton i t = H.input ! A.id (fromString i) ! A.type_ "submit" ! A.value (fromString t)

checkBox :: String -> String -> Bool -> Html
checkBox n v c =
  (H.input ! A.name (fromString n)
           ! A.type_ "checkbox"
           ! A.value (fromString v))
  |> if c then (! A.checked "") else id

withId :: (Html -> Html) -> String -> (Html -> Html)
withId f i = (f ! A.id (fromString i))

setHookClass c h = h ! A.class_ (className c)

required h = h ! A.required ""

-- * Form

postForm :: String -> Html -> Html
postForm action = H.form ! A.method "post" ! A.action (fromString action) ! A.acceptCharset "UTF-8"

getForm :: String -> Html -> Html
getForm action = H.form ! A.method "get" ! A.action (fromString action) ! A.acceptCharset "UTF-8"

-- Creates a POST form with multiple choices of actions. Each action is described in a pair
-- (action, buttonText), all the buttons are added after the given html
-- NOTE: JavaScript functions are named after the given id, except that all non
-- alphanumeric characters are stripped out.
multiActionPostForm :: String -> [(String, String)] -> Html -> Html
multiActionPostForm id actions html =
  H.form ! A.id (fromString id) ! A.method "post" ! A.acceptCharset "UTF-8" $ do
    html
    mapM_ actionButton ([1..] `zip` actions)
  where
    actionButton (i,(action, button)) = do
      let fname = jsFunctionName $ concat [id, show i, "onClick"]
      H.p $ do
        H.input ! A.type_ "button" ! A.onclick (fromString (fname ++ "()")) ! A.value (fromString button)
        H.script $ fromString $ concat
          [ "function ", fname, "(){"
          ,    "document.getElementById(\"",id,"\").setAttribute(\"action\",\"",action,"\");"
          ,    "document.getElementById(\"",id,"\").submit();"
          , "}"
          ]

-- Returns a string which contains only alphanum caracters
jsFunctionName :: String -> String
jsFunctionName = filter isAlphaNum

-- Creates an HTML div which represents a countdown timer, showing
-- the time left in days hours:min:secs format from the given
-- now time beetwen the given until time.
startEndCountdownDiv :: String -> String -> String -> UTCTime -> UTCTime -> Html
startEndCountdownDiv divId daystr overstr now until =
  countdownDiv divId daystr overstr True (floor $ diffUTCTime until now)

-- Creates an HTML dic which represents a countdown timer, showing
-- the time left in hours:min format for the given seconds, mainly
-- to show
minSecCountdown :: String -> String -> Int -> Html
minSecCountdown divId overstr secs =
  countdownDiv divId "" overstr False secs

-- Creates an HTML div which represents a countdown timer, showing
-- the ETA time in days hours:min:secs format from the given
-- now time in seconds.
countdownDiv :: String -> String -> String -> Bool -> Int -> Html
countdownDiv divId daystr overstr showDays seconds = do
  H.div ! A.id (fromString divId) $ fromString $
    if showDays
         then concat ["-", daystr, " --:--:--"]
         else "--:--"
  H.script $ fromString countdown
  where
    fname = jsFunctionName (divId ++ "countdown")

    countdown = concat
      [ fname, "();"
      , "function ", fname, "() {"
      ,    "var minsecs = 60;"
      ,    "var hoursecs = minsecs * 60;"
      ,    "var daysecs = hoursecs* 24;"
      ,    "var mstime = ", show seconds, " * 1000;"
      ,    "var timestamp = new Date;"
      ,    "var interval = setInterval(function() {"
      ,       "var el = document.getElementById(\"", divId, "\");"
      ,       "var time = Math.round( mstime / 1000 );"
      ,       "if(time < 0) {"
      ,           "el.innerHTML = \"",overstr,"\";"
      ,           "clearInterval(interval);"
      ,           "return;"
      ,       "}"
      ,       "var edays   = Math.floor( time / daysecs );"
      ,       "var ehours1 = time % daysecs;"
      ,       "var ehours  = Math.floor( ehours1 / hoursecs );"
      ,       "if (ehours < 10) ehours = \"0\" + ehours;"
      ,       "var emins1  = ehours1 % hoursecs;"
      ,       "var emins   = Math.floor( emins1 / minsecs );"
      ,       "if (emins < 10) emins = \"0\" + emins;"
      ,       "var esecs   = emins1 % minsecs;"
      ,       "if (esecs < 10) esecs = \"0\" + esecs;"
      ,       if showDays
                  then concat
                     [ "var text=\"\";"
                     , "if(edays == 0) {"
                     ,    "if(ehours == 0) {"
                     ,       "text = emins + ':' + esecs;"
                     ,    "} else {"
                     ,       "text = ehours + ':' + emins + ':' + esecs;"
                     ,    "}"
                     , "} else {"
                     ,    "text = edays + \" ", daystr, " \" + ehours + ':' + emins + ':' + esecs;"
                     , "}"
                     ]
                  else "var text = emins + ':' + esecs;"
      ,       "el.innerHTML = text;"
      ,       "var now = new Date;"
      ,       "var dt = now - timestamp;"
      ,       "timestamp = now;"
      ,       "mstime = mstime - dt;"
      ,       "}, 1000);"
      , "}"
      ]

-- * Table
table :: String -> String -> Html -> Html
table i c = H.table ! A.id (fromString i) ! A.class_ (fromString c)

tableLine :: String -> Html -> Html
tableLine title field = do
  H.tr $ do
    H.td (fromString title)
    H.td field

hiddenTableLine :: Html -> Html
hiddenTableLine value = H.tr . H.td $ value

empty :: Html
empty = return ()

linkText :: P.Page -> Translation String
linkText = P.pageCata
  (Msg_LinkText_Login "Login")
  (Msg_LinkText_Logout "Logout")
  (Msg_LinkText_Home "Home")
  (Msg_LinkText_Profile "Profile")
  (Msg_LinkText_Error "Error")
  (Msg_LinkText_Administration "Administration")
  (Msg_LinkText_CourseAdministration "Course Settings")
  (const $ Msg_LinkText_CourseOverview "Course Overview")
  (Msg_LinkText_EvaluationTable "Evaluations")
  (const $ Msg_LinkText_Evaluation "Evaluation")
  (const2 $ Msg_LinkText_ModifyEvaluation "Modify Evaluation")
  (const $ Msg_LinkText_NewGroupAssignment "New Group Assignment")
  (const $ Msg_LinkText_NewCourseAssignment "New Course Assignment")
  (const $ Msg_LinkText_ModifyAssignment "Modify Assignment")
  (const $ Msg_LinkText_ViewAssignment "View Assignment")
  (const $ Msg_LinkText_NewGroupAssignmentPreview "New Group Assignment")
  (const $ Msg_LinkText_NewCourseAssignmentPreview "New Course Assignment")
  (const $ Msg_LinkText_ModifyAssignmentPreview "Modify Assignment")
  (Msg_LinkText_Submission "Submit")
  (Msg_LinkText_SubmissionList "Submissions")
  (const2 $ Msg_LinkText_SubmissionDetails "Submission Details")
  (Msg_LinkText_GroupRegistration "Group Registration")
  (Msg_LinkText_UserDetails "User Details")
  (Msg_LinkText_UserSubmissions "Submissions")
  (Msg_LinkText_NewTestScript "New Test")
  (const $ Msg_LinkText_ModifyTestScript "Modify Test Script")
  (Msg_LinkText_UploadFile "Upload file")
  (Msg_LinkText_CreateCourse "Create Course")
  (Msg_LinkText_CreateGroup "Create Group")
  (Msg_LinkText_AssignCourseAdmin "Assign Course Admin")
  (Msg_LinkText_AssignGroupAdmin "Assign Teacher")
  (Msg_LinkText_ChangePassword "Change Password")
  (Msg_LinkText_SetUserPassword "Set Student Password")
  (const $ Msg_LinkText_CommentFromEvaluation "Comment")
  (const2 $ Msg_LinkText_CommentFromModifyEvaluation "Comment")
  (const $ Msg_LinkText_DeleteUsersFromCourse "Remove Students")
  (const $ Msg_LinkText_DeleteUsersFromGroup "Remove Students")
  (const $ Msg_LinkText_UnsubscribeFromCourse "Unregister")
  where
    const2 = const . const

linkToPage :: P.Page -> IHtml
linkToPage g = do
  msg <- getI18N
  return $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString $ msg $ linkText g

linkToPageBlank :: P.Page -> IHtml
linkToPageBlank g = do
  msg <- getI18N
  return $ H.a ! A.href (routeOf g) ! A.target "_blank" ! A.id (fieldName g) $ fromString $ msg $ linkText g

linkToPageWithText :: P.Page -> String -> Html
linkToPageWithText g t = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString t

link :: String -> String -> Html
link r t = H.a ! A.href (fromString r) $ (fromString t)

linkWithText :: String -> String -> Html
linkWithText r t = H.a ! A.href (fromString r) $ (fromString t)

-- Produces a HTML-link with the given route text and title
linkWithTitle :: String -> String -> String -> Html
linkWithTitle route title text =
  H.a ! A.href (fromString route)
      ! A.title (fromString title)
      $ fromString text

linkToRoute :: String -> Html
linkToRoute = link "/"

-- Html text in span tag with title attribute
spanWithTitle :: String -> String -> Html
spanWithTitle title text = H.span ! A.title (fromString title) $ fromString text

navigationMenu :: UserState -> IHtml
navigationMenu s = do
  msg <- getI18N
  return $ H.ul $ mapM_ (H.li . (I18N.i18n msg . linkToPage)) $ P.menuPages (role s) (page s)

pageHeader :: UserState -> Int -> IHtml
pageHeader s secs = do
  msg <- getI18N
  return $ do
    H.div ! A.id "logo" $ "BE-AD"
    H.div ! A.id "user" $ do
      minSecCountdown "hdctd" "--:--" secs
      (fromString . str . user $ s)
      H.br
      (I18N.i18n msg $ linkToPage P.Home)
      H.br
      (I18N.i18n msg $ linkToPage P.Profile)
      H.br
      (I18N.i18n msg $ linkToPage P.Logout)
    H.div ! A.id "title" $ title msg s
  where
    title msg u@(UserState {}) = fromString . msg . linkText . page $ u
    title _ _ = ""

pageStatus :: UserState -> IHtml
pageStatus = maybe noMessage message . status
  where
    noMessage = return $ return ()
    message m = do
      msg <- getI18N
      return $ H.div ! A.id "status" # backgroundColor "yellow" $ H.span $ (fromString $ msg m)

-- * Picklist

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection :: String -> Html -> Html
selection name =
    H.select ! A.id (fromString name)
             ! A.name (fromString name)
             ! A.required ""

class SelectionValue v where
  selectionValue :: v -> String

-- TODO: Change to I18N
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

defValueTextSelection :: (Eq s, SelectionValue s, SelectionText s) => String -> s -> [s] -> Html
defValueTextSelection name def = selection name . mapM_ option' where
  option' x = option (selectionValue x) (selectionText x) (x == def)

valueTextSelection :: (SelectionValue s, SelectionText s) => String -> [s] -> Html
valueTextSelection name = selection name . mapM_ option'
  where
    option' s = option (selectionValue s) (selectionText s) False

-- Creates a selection from enum values, starting enumeration from the
-- given value
enumSelection :: (Enum e, SelectionValue e, SelectionText e) => String -> e -> Html
enumSelection name start = valueTextSelection name [start .. ]

-- Creates a selection with a given name attribute and a default value
-- as the actively selected one.
defEnumSelection :: (Eq e, Enum e, SelectionValue e, SelectionText e) => String -> e -> Html
defEnumSelection name def = defValueTextSelection name def [toEnum 0 .. ]

valueSelection :: (o -> (String, String)) -> String -> [o] -> Html
valueSelection f n = selection n . mapM_ option'
  where
    option' o = let (v,n) = f o in option v n False

-- Creates a selection from a descriptive function, which converts a value to a selection value and text pair
-- from an HTML identifier name, a function that describes if the value is selected or not
-- and a list of values
valueSelectionWithDefault :: (o -> (String, String)) -> String -> (o -> Bool) -> [o] -> Html
valueSelectionWithDefault f n d = selection n . mapM_ option'
  where
    option' o =
      let (v,n) = f o
      in option v n (d o)

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

instance SelectionValue TestScriptType where
  selectionValue = show

instance SelectionText TestScriptType where
  selectionText = show

instance SelectionText AssignmentType where
  selectionText = show

instance SelectionValue AssignmentType where
  selectionValue = show

instance SelectionValue TimeZone where
  selectionValue = show

instance SelectionText TimeZone where
  selectionText = show

evalSelectionDiv :: EvaluationHook -> Html
evalSelectionDiv h = ((H.div `withId` (evSelectionDivId h)) $ empty)

#ifdef TEST

-- * Invariants

invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page -> String
      linkText' = trans . linkText

#endif

