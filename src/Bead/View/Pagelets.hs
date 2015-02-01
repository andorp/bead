{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Pagelets where

import           Prelude hiding (span)

import           Data.Char (isAlphaNum)
import           Data.Data
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString(..), fromString)
import           Data.Time.Clock

import           Text.Blaze.Html5 hiding (link, option)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Attributes hiding (id, span)

import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext as ServiceContext (UserState(..))
import           Bead.Domain.Entities as Entity (statusMessage, usernameCata, uid)
import           Bead.View.Fay.Hooks
import           Bead.View.Fay.JSON.ServerSide
import qualified Bead.View.I18N as I18N
import           Bead.View.I18N (IHtml, translate, getI18N)
import           Bead.View.RouteOf
import           Bead.View.Style
import           Bead.View.TemplateAndComponentNames
import           Bead.View.Translation
import qualified Bead.View.Content.Bootstrap as Bootstrap
#ifdef TEST
import           Bead.Invariants (Invariants(..))
#endif

-- * Definitions

css :: String -> Html
css c = H.link ! A.type_ "text/css" ! A.href (fromString c) ! A.rel "stylesheet"

js :: String -> Html
js j = H.script ! A.src (fromString j) $ empty

bootStrapDocument :: IHtml -> IHtml
bootStrapDocument body' = do
  body <- body'
  return $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "BE-AD beadandókezelő"
        H.link ! A.rel "shortcut icon" ! A.href "icon.ico"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.meta ! A.name "description" ! A.content ""
        H.meta ! A.name "author" ! A.content ""
        js "/jquery.js"
        css "/jquery-ui.css"
        js "/jquery-ui.js"
        js "/moment.js"
        css "/bootstrap.min.css"
        css "/bootstrap.custombutton.css"
        js "/bootstrap.min.js"
        css "/bootstrap-combobox.css"
        js "/bootstrap-combobox.js"
        css "/bootstrap-datetimepicker.min.css"
        js "/bootstrap-datetimepicker.min.js"
        js "/fay/DynamicContents.js"
    H.body $ body

runBootstrapPage :: IHtml -> I18N -> Html
runBootstrapPage p i = translate i $ bootStrapDocument p

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

bootstrapUserFrame :: UserState -> IHtml -> Int -> IHtml
bootstrapUserFrame s content secs = withUserFrame' content
  where
    withUserFrame' content = do
      header <- bootStrapHeader s secs
      content <- content
      status <- bootStrapStatus s
      msg <- getI18N
      return $ do
        header
        Bootstrap.container $ do
          Bootstrap.rowColMd12 $ hr
          Bootstrap.rowColMd12 $ Bootstrap.pageHeader $ h2 $
            fromString $ msg $ linkText $ page s
          content
          Bootstrap.rowColMd12 $ hr
        status

-- | Places a given content in a public frame
publicFrame :: IHtml -> IHtml
publicFrame content = do
  header <- publicHeader
  content <- content
  return $ do
    header
    Bootstrap.container content

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

-- Creates a number input with the given minimum and maximum, if the value is given
-- set as the default value
numberInput :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Html
numberInput name min_ max_ val_ = do
  let val x = maybe (! (A.value x)) (\y -> (! (A.value . fromString $ show y))) val_
  let mn = maybe id (\m -> let m' = fromString $ show m
                           in (! (A.min m')) . (val m')) min_
  let mx = maybe id (\m -> let m' = fromString $ show m
                           in (! (A.max m')) . (val m')) max_
  mn $ mx $ H.input ! A.type_ "number" ! A.name (fromString name)

submitButton :: String -> String -> Html
submitButton i t = H.input ! A.id (fromString i) ! A.type_ "submit" ! A.value (fromString t)

submitButtonDanger :: String -> String -> Html
submitButtonDanger i t =
  H.input ! A.id (fromString i)
          ! A.type_ "submit"
          ! A.class_ "btn btn-danger"
          ! A.value (fromString t)

checkBox :: String -> String -> Bool -> Html
checkBox n v c =
  (H.input ! A.name (fromString n)
           ! A.type_ "checkbox"
           ! A.value (fromString v))
  |> if c then (! A.checked "") else id

checkBox' :: (Show a, Data a) => String -> Bool -> a -> String -> Html
checkBox' name checked value text = do
  checkBox name (encodeToFay' "checkBox" value) checked
  fromString text

withId :: (Html -> Html) -> String -> (Html -> Html)
withId f i = (f ! A.id (fromString i))

setHookClass c h = h ! A.class_ (className c)

required h = h ! A.required ""

-- * Form

-- Form that represents input for ajax requests generated on the client side
fayaxForm :: String -> String -> Html -> Html
fayaxForm id action = H.form ! A.action (fromString action) ! A.id (fromString id)

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
  H.a ! A.id (fromString divId) $ fromString $
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
      ,       "var now = new Date;"
      ,       "var dt = now - timestamp;"
      ,       "timestamp = now;"
      ,       "mstime = mstime - dt;"
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

linkText :: P.Page a b c d e -> Translation String
linkText = P.pageCata
  (c $ msg_LinkText_Login "Login")
  (c $ msg_LinkText_Logout "Logout")
  (c $ msg_LinkText_Home "Home")
  (c $ msg_LinkText_Profile "Profile")
  (c $ msg_LinkText_Administration "Administration")
  (c $ msg_LinkText_CourseAdministration "Course Settings")
  (c2 $ msg_LinkText_CourseOverview "Course Overview")
  (c $ msg_LinkText_EvaluationTable "Evaluations")
  (c2 $ msg_LinkText_Evaluation "Evaluation")
  (c3 $ msg_LinkText_ModifyEvaluation "Modify Evaluation")
  (c2 $ msg_LinkText_NewGroupAssignment "New Group Assignment")
  (c2 $ msg_LinkText_NewCourseAssignment "New Course Assignment")
  (c2 $ msg_LinkText_ModifyAssignment "Modify Assignment")
  (c2 $ msg_LinkText_ViewAssignment "View Assignment")
  (c2 $ msg_LinkText_NewGroupAssignmentPreview "New Group Assignment")
  (c2 $ msg_LinkText_NewCourseAssignmentPreview "New Course Assignment")
  (c2 $ msg_LinkText_ModifyAssignmentPreview "Modify Assignment")
  (c $ msg_LinkText_Submission "Submission")
  (c $ msg_LinkText_SubmissionList "Submissions")
  (c3 $ msg_LinkText_SubmissionDetails "Submission Details")
  (c $ msg_LinkText_GroupRegistration "Group Registration")
  (c $ msg_LinkText_UserDetails "User Details")
  (c $ msg_LinkText_UserSubmissions "Submissions")
  (c $ msg_LinkText_NewTestScript "New Test")
  (c2 $ msg_LinkText_ModifyTestScript "Modify Test Script")
  (c $ msg_LinkText_UploadFile "Upload file")
  (c $ msg_LinkText_CreateCourse "Create Course")
  (c $ msg_LinkText_CreateGroup "Create Group")
  (c $ msg_LinkText_AssignCourseAdmin "Assign Course Admin")
  (c $ msg_LinkText_AssignGroupAdmin "Assign Teacher")
  (c $ msg_LinkText_ChangePassword "Change Password")
#ifndef LDAPEnabled
  (c $ msg_LinkText_SetUserPassword "Set Student Password")
#endif
  (c2 $ msg_LinkText_DeleteUsersFromCourse "Remove Students")
  (c2 $ msg_LinkText_DeleteUsersFromGroup "Remove Students")
  (c2 $ msg_LinkText_UnsubscribeFromCourse "Unregister")
  (c2 $ msg_LinkText_GetSubmission "Download Submission")
  where
    c = const
    c2 = c . const
    c3 = c2 . const

linkToPage :: P.Page a b c d e -> IHtml
linkToPage g = do
  msg <- getI18N
  return $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString $ msg $ linkText g

linkButtonToPageBS :: P.Page a b c d e -> IHtml
linkButtonToPageBS g = do
  msg <- getI18N
  return $ H.a ! A.href (routeOf g) ! A.class_ "btn btn-default" $ fromString $ msg $ linkText g

linkToPageBlank :: P.Page a b c d e -> IHtml
linkToPageBlank g = do
  msg <- getI18N
  return $ H.a ! A.href (routeOf g) ! A.target "_blank" ! A.id (fieldName g) $ fromString $ msg $ linkText g

linkToPageWithText :: P.Page a b c d e -> String -> Html
linkToPageWithText g t = H.p $ H.a ! A.href (routeOf g) ! A.id (fieldName g) $ fromString t

link :: String -> String -> Html
link r t = H.a ! A.href (fromString r) $ (fromString t)

linkWithText :: String -> String -> Html
linkWithText r t = H.a ! A.href (fromString r) $ (fromString t)

linkWithHtml :: String -> Html -> Html
linkWithHtml r t = H.a ! A.href (fromString r) $ t

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

publicHeader :: IHtml
publicHeader = do
  msg <- getI18N
  return $ do
    H.div ! class_ "navbar navbar-default" $ do
      H.style ".body{padding-top:70px}"
      H.div ! class_ "container" $ do
        H.div ! class_ "navbar-header" $ do
         span ! class_ "navbar-brand" $ "BE-AD"

bootStrapHeader :: UserState -> Int -> IHtml
bootStrapHeader s secs = do
  msg <- getI18N
  return $ do
        H.div ! class_ "navbar navbar-default navbar-fixed-top" $ do
            H.style ".body{padding-top:70px}"
            H.div ! class_ "container" $ do
                H.div ! class_ "navbar-header" $ do
                    a ! class_ "navbar-brand" ! href (routeOf home) $ "BE-AD"
                    button ! type_ "button" ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
                        H.span ! class_ "sr-only" $ "Toggle navigation"
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                H.div ! class_ "collapse navbar-collapse navbar-ex1-collapse" $ do
                    ul ! class_ "nav navbar-nav navbar-right" $ do
                        li $ minSecCountdown "hdctd" "--:--" secs
                        li $ H.a userId
                        li $ (I18N.i18n msg $ linkToPage profile)
                        li $ (I18N.i18n msg $ linkToPage logout)
  where
    logout = P.logout ()
    profile = P.profile ()
    home = P.home ()
    userId = fromString $ concat [usernameCata id . user $ s, " / ", Entity.uid id . ServiceContext.uid $ s]

bootStrapStatus :: UserState -> IHtml
bootStrapStatus = maybe noMessage message . status
  where
    noMessage = return $ return ()
    message m = do
      msg <- getI18N
      let labelMessage = statusMessage
            (Bootstrap.fadeOutFooterWarningButton "title" . msg)
            (Bootstrap.fadeOutFooterDangerButton "title" . msg)
            m
      return $ do
        Bootstrap.footer
          $ Bootstrap.container $ Bootstrap.rowColMd12 $ Bootstrap.buttonGroupJustified $ labelMessage
        Bootstrap.fadeOutFooter 30

-- * Picklist

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection' :: String -> Html -> Html
selection' name =
    H.select ! A.id (fromString name)
             ! A.name (fromString name)
             ! A.required ""

-- Encodes the value to Fay JSON representation or throw an error for the given name
encodeToFay' :: (Data a, Show a, IsString s) => String -> a -> s
encodeToFay' name value = fromString $ fromMaybe (name ++ ": error encoding value") (encodeToFay value)

selection :: (Show a, Data a) => String -> [(a,String)] -> Html
selection name = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t False where

selectionWithDefault :: (Eq a, Show a, Data a) => String -> a -> [(a,String)] -> Html
selectionWithDefault name def = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (v == def) where

selectionWithDefault' :: (Show a, Data a) => String -> (a -> Bool) -> [(a, String)] -> Html
selectionWithDefault' name def = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (def v) where

selectionWithDefAndAttr :: (Show a, Data a) =>
  String -> [Attribute] -> (a -> Bool) -> [(a, String)] -> Html
selectionWithDefAndAttr name attrs def = foldl (!) (selection' name) attrs . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (def v) where

evalSelectionDiv :: EvaluationHook -> Html
evalSelectionDiv h = ((H.div `withId` (evSelectionDivId h)) $ empty)

-- * Radio buttons

-- One radiobutton for the json encodeable value with the given parameter name
radioButton :: (Show a, Data a) => String -> (a,String) -> Html
radioButton name (value,text) = do
  H.input ! A.type_ "radio" ! A.name (fromString name) ! A.value (encodeToFay' "radioButton" value)
  fromString text

-- Radio buttons placed horizontally for the given parameter name and values
horizontalRadioButtons :: (Show a, Data a) => String -> [(a, String)] -> Html
horizontalRadioButtons name values = mapM_ (radioButton name) values

-- Radio buttons placed horizontally for the given parameter name and values, checked the default nth
horizontalRadioButtonsDef :: (Show a, Data a) => String -> Int -> [(a, String)] -> Html
horizontalRadioButtonsDef name nth values = mapM_ (radioButton' name) (values `zip` [0..])
  where
    radioButton' n (v,i) =
      (if i == nth then (! A.checked "") else id) $ radioButton n v

-- Radio buttons placed vertically for the given parameter name and values
verticalRadioButtons :: (Show a, Data a) => String -> [(a, String)] -> Html
verticalRadioButtons name values = mapM_ button values
  where
    button v = do { radioButton name v; H.br }

#ifdef TEST

-- * Invariants

invariants :: Invariants P.PageDesc
invariants = Invariants [
    ("Page link text must be defined: ", \p -> length (linkText' p) > 0)
  ] where
      linkText' :: P.Page a b c d e -> String
      linkText' = trans . linkText

#endif
