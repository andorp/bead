{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Blaze where

import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import Bead.View.Snap.Pagelets
import qualified Bead.Controller.Pages as P

-- Definitions --

index :: Maybe Html -> Html
index Nothing = do
  H.p "User is not logged in"

index (Just u) = do
  H.div ! A.id "header" $
    H.p $ do
      "You are logged in as "
      u
  H.div ! A.id "menu" $
    H.p $ menu

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
      mapM_ field [ ("Login:", "text","login")
                  , ("Password:", "password","password")
                  , ("Email address:", "text","reg_email_address")
                  , ("Family name:", "text","reg_family_name")
                  ]
      -- Submit button
      H.tr $ do
        H.td empty
        H.td $ H.input ! A.type_ "submit" ! A.value submitText
  where
    field (f,t,n) = do
      H.tr $ do
        H.td f
        H.td $ H.input ! A.type_ t ! A.name n ! A.size "20"
