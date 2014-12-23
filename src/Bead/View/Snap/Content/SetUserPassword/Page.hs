{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SetUserPassword.Page (
    setUserPassword
  ) where

import           Data.String (fromString)
import           Text.Blaze.Html5 as H hiding (id)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (isStudentOfMine)
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import qualified Bead.View.Snap.ResetPassword as P
import qualified Bead.View.Snap.DataBridge as B

setUserPassword = ViewModifyHandler setUserPasswordPage setUsrPwd

setUserPasswordPage :: GETContentHandler
setUserPasswordPage = return setUserPasswordContent

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
  return $ do
    Bootstrap.row $ Bootstrap.colMd Bootstrap.colSize4 Bootstrap.colOffset4 $
      postForm (routeOf setUserPassword) `withId` (rFormId setStudentPwdForm) $ do
        Bootstrap.textInput     (B.name usernamePrm)           (msg $ Msg_SetUserPassword_User "Username: ") ""
        Bootstrap.passwordInput (B.name studentNewPwdPrm)      (msg $ Msg_SetUserPassword_NewPassword "New password: ")
        Bootstrap.passwordInput (B.name studentNewPwdAgainPrm) (msg $ Msg_SetUserPassword_NewPasswordAgain "New password (again): ")
        Bootstrap.submitButton  (fieldName changePasswordBtn)  (msg $ Msg_SetUserPassword_SetButton "Update")
  where
    setUserPassword = Pages.setUserPassword ()
