{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SetUserPassword (
    setUserPassword
  ) where

import Bead.Controller.Pages (Page(..))
import Bead.Controller.UserStories (courseOrGroupStudent)
import Bead.View.Snap.Content
import qualified Bead.View.Snap.ResetPassword as P
import qualified Bead.View.Snap.DataBridge as B
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

setUserPassword :: Content
setUserPassword = getPostContentHandler setUserPasswordPage setUsrPwd

setUserPasswordPage :: GETContentHandler
setUserPasswordPage = withUserState $ \s -> do
  renderDynamicPagelet $ withUserFrame s setUserPasswordContent

setUsrPwd :: POSTContentHandler
setUsrPwd = do
  user <- getParameter usernamePrm
  newPwd <- getParameter studentNewPwdPrm
  isStudentOfMe <- runStoryE (courseOrGroupStudent user)
  case isStudentOfMe of
    False -> do
      let username = usernameCata id user
      return $ StatusMessage $ concat [username, " is not a student in your groups or courses."]
    True -> do
      msg <- P.setUserPassword user newPwd
      return $ StatusMessage msg

setUserPasswordContent :: Pagelet
setUserPasswordContent = onlyHtml $ mkI18NHtml $ \i -> do
  postForm (routeOf SetUserPassword) `withId` (rFormId setStudentPwdForm) $ do
    table (fieldName changePasswordTable) (fieldName changePasswordTable) # centerTable $ do
      tableLine (i "Usename: ") $ textInput (B.name usernamePrm) 20 Nothing ! A.required ""
      tableLine (i "New Password: ") $ passwordInput (B.name studentNewPwdPrm) 20 Nothing ! A.required ""
      tableLine (i "New Password again: ") $ passwordInput (B.name studentNewPwdAgainPrm) 20 Nothing ! A.required ""
    submitButton (fieldName changePasswordBtn) (i "Set password")
