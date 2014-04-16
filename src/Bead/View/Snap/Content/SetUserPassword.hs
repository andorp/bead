{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SetUserPassword (
    setUserPassword
  ) where

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (isStudentOfMine)
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.ResetPassword as P
import qualified Bead.View.Snap.DataBridge as B

setUserPassword = ViewModifyHandler setUserPasswordPage setUsrPwd

setUserPasswordPage :: GETContentHandler
setUserPasswordPage = withUserState $ \s -> do
  renderDynamicPagelet $ withUserFrame s setUserPasswordContent

setUsrPwd :: POSTContentHandler
setUsrPwd = do
  user <- getParameter usernamePrm
  newPwd <- getParameter studentNewPwdPrm
  ok <- userStory (isStudentOfMine user)
  if ok
    then do
      msg <- P.setUserPassword user newPwd
      return $ StatusMessage msg
    else do
      let username = usernameCata id user
      return . StatusMessage $ Msg_SetUserPassword_NonRegisteredUser
        "This user is not registered in neither of your courses nor your groups."

setUserPasswordContent :: IHtml
setUserPasswordContent = do
  msg <- getI18N
  return $ H.div # textAlign "left" $ do
    postForm (routeOf setUserPassword) `withId` (rFormId setStudentPwdForm) $ do
      table (fieldName changePasswordTable) (fieldName changePasswordTable) $ do
        tableLine (msg $ Msg_SetUserPassword_User "Username: ") $ textInput (B.name usernamePrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_SetUserPassword_NewPassword "New password: ") $ passwordInput (B.name studentNewPwdPrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_SetUserPassword_NewPasswordAgain "New password (again): ") $ passwordInput (B.name studentNewPwdAgainPrm) 20 Nothing ! A.required ""
      submitButton (fieldName changePasswordBtn) (msg $ Msg_SetUserPassword_SetButton "Update")
  where
    setUserPassword = Pages.setUserPassword ()
