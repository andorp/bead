
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SetUserPassword (
    setUserPassword
  ) where

import Bead.Controller.Pages (Page(..))
import Bead.Controller.UserStories (courseOrGroupStudent)
import Bead.View.Snap.Content
import qualified Bead.View.Snap.ResetPassword as P
import qualified Bead.View.Snap.DataBridge as B
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Printf (printf)

setUserPassword :: Content
setUserPassword = getPostContentHandler setUserPasswordPage setUsrPwd

setUserPasswordPage :: GETContentHandler
setUserPasswordPage = withUserState $ \s -> do
  renderDynamicPagelet $ withUserFrame s setUserPasswordContent

-- TODO: I18N
setUsrPwd :: POSTContentHandler
setUsrPwd = do
  user <- getParameter usernamePrm
  newPwd <- getParameter studentNewPwdPrm
  isStudentOfMe <- userStory (courseOrGroupStudent user)
  case isStudentOfMe of
    False -> do
      let username = usernameCata id user
      return . StatusMessage $ Msg_SetUserPassword_NonRegisteredUser
        "This user is not registered in neither of your courses nor your groups."
    True -> do
      msg <- P.setUserPassword user newPwd
      return $ StatusMessage msg

setUserPasswordContent :: IHtml
setUserPasswordContent = do
  msg <- getI18N
  return $ H.div # textAlign "left" $ do
    postForm (routeOf SetUserPassword) `withId` (rFormId setStudentPwdForm) $ do
      table (fieldName changePasswordTable) (fieldName changePasswordTable) $ do
        tableLine (msg $ Msg_SetUserPassword_User "Username: ") $ textInput (B.name usernamePrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_SetUserPassword_NewPassword "New password: ") $ passwordInput (B.name studentNewPwdPrm) 20 Nothing ! A.required ""
        tableLine (msg $ Msg_SetUserPassword_NewPasswordAgain "New password (again): ") $ passwordInput (B.name studentNewPwdAgainPrm) 20 Nothing ! A.required ""
      submitButton (fieldName changePasswordBtn) (msg $ Msg_SetUserPassword_SetButton "Update")
