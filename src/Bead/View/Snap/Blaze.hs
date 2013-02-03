{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Blaze where

import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as P

-- Definitions --

class BlazeTemplate b where
  template :: b -> Html

base :: Html -> Maybe Html -> Html
base content loggedInContent = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "content" $ content
    case loggedInContent of
      Nothing -> return ()
      Just inner  -> H.div $ do
        H.div ! A.id "menu1" $ do
          inner

admin :: Html
admin = base "Admin page is not defined" Nothing

closedExam :: Html
closedExam = base "Closed exam page is not defined" Nothing

course :: Html
course = base "Course page is not defined" Nothing

errorPage :: Html
errorPage = base "Error page is not defined" Nothing

evaulation :: Html
evaulation = base "Evaulation page is not defined" Nothing

group :: Html
group = base "Group page is not defined" Nothing

home :: Html
home = base "Home page is not definied" Nothing

index :: Maybe Html -> Html
index loggedIn = do
  H.p $ do
    "This is a simple demo page served using "
    H.a ! A.href "http://snapframework.com/docs/tutorials/heist" $ "Heist"
    "and the "
    H.a ! A.href "http://snapframework.com/" $ "Snap"
    "web framework."
  case loggedIn of
    Nothing -> return ()
    Just h  -> H.p $ do
      "Congrats! You're logged in as "
      h

empty :: Html
empty = return ()

userForm :: AttributeValue -> AttributeValue -> Html
userForm act submitText = do
  H.form ! A.method "post" ! A.action act $ do
    H.table ! A.id "info" $ do
      H.tr $ do
        H.td "Login:"
        H.td $ H.input ! A.type_ "text" ! A.name "login" ! A.size "20"
    H.tr $ do
      H.td "Password:"
      H.td $ H.input ! A.type_ "password" ! A.name "password" ! A.size "20"
    H.tr $ do
      H.td $ return ()
      H.td $ H.input ! A.type_ "submit" ! A.value submitText

loginError :: Html
loginError = return ()

login :: Html
login = base content Nothing
  where
    content = do
      H.h1 $ "Login"
      H.p $ loginError
      userForm "/login" "Login"
      H.p $ do
        "Don't have a login yet? "
        H.a ! A.href "/new_user" $ "Create new user"

menu :: Html
menu =
  mapM_ (\(link,text) -> (H.p $ H.a ! A.href link $ text)) $ [
      ("/logout"    , "Logout")
    , ("/admin"     , "Admin")
    , ("/profile"   , "Profile")
    , ("/evaulation", "Evaulation")
    , ("/group"     , "Group")
    , ("/home"      , "Home")
    , ("/openexam"  , "Open Exam")
    , ("/closedexam", "Closed Exam")
    , ("/submitexam", "Submit Exam")
    , ("/training"  , "Training")
    ]

newUser :: Html
newUser = base content Nothing
  where
    content = do
      H.h1 $ "Register a new user"
      registrationForm "/new_user" "Add User"

registrationForm :: AttributeValue -> AttributeValue -> Html
registrationForm postAction submitText = do
  H.form ! A.method "post" ! A.action postAction $ do
    H.table ! A.id "registration" $ do
      -- Fields
      mapM_ field [ ("text","login")
                  , ("password","password")
                  , ("text","reg_email_address")
                  , ("text","reg_family_name")
                  ]
      -- Submit button
      H.tr $ do
        H.td empty
        H.td $ H.input ! A.type_ "submit" ! A.value submitText
  where
    field (t,n) = do
      H.tr $ do
        H.td "Login:"
        H.td $ H.input ! A.type_ t ! A.name n ! A.size "20"

openExam :: Html
openExam = base "Open exam page is not defined" Nothing

submitExam :: Html
submitExam = base "Submit exam page is not defined" Nothing

training :: Html
training = base "Training page is not defined" Nothing

profile :: Html
profile = base "Profile page is not defined" Nothing

instance BlazeTemplate P.Page where
  template = t where
    t P.Login      = login
    t P.Home       = home
    t P.Profile    = profile
    t P.Course     = course
    t P.Group      = group
    t P.OpenExam   = openExam
    t P.ClosedExam = closedExam
    t P.Error      = errorPage
    t P.SubmitExam = submitExam
    t P.Evaulation = evaulation
    t P.Training   = training
    t P.Admin      = admin
