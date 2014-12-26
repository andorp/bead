{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Login (
    login
  , loginSubmit
  , changeLanguage
  ) where

import           Data.ByteString.Char8 hiding (index, putStrLn)
import           Data.Maybe (isNothing)
import qualified Data.Text as T
import           Prelude as P

import           Snap.Snaplet.Auth as A
import           Snap.Snaplet.Session

import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as S
import           Bead.View.Snap.BeadContext
import           Bead.View.Snap.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Snap.Content.Public.Login as View
import           Bead.View.Snap.ContentHandler
import           Bead.View.Snap.ErrorPage
import           Bead.View.Snap.Session


-- * Login and Logout handlers

login :: Maybe AuthFailure -> BeadHandler' b ()
login authError = do
  -- Set the default language in session if no information is found
  languages <- withTop dictionaryContext dcGetDictionaryInfos
  mLangInSession <- withTop sessionManager languageFromSession
  when (isNothing mLangInSession) . withTop sessionManager $ do
    defaultLang <- withTop dictionaryContext configuredDefaultDictionaryLanguage
    setLanguageInSession defaultLang
    commitSession

  -- Render the page content
  renderBootstrapPublicPage . publicFrame $ do
    msg <- getI18N
    View.login (authError >>= visibleFailure msg) languages

loginSubmit :: BeadHandler' b ()
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  user <- getParameter loginUsernamePrm
  pwd  <- getParameter loginPasswordPrm
  loggedIn <- lift $ loginByUsername
    (usernameCata T.pack user)
    (ClearText . pack $ pwd)
    False
  case loggedIn of
    Left failure -> lift . login $ Just failure
    Right authUser -> lift $ do
      context <- withTop serviceContext getServiceContext
      token   <- sessionToken
      let unameFromAuth = usernameFromAuthUser authUser
          mpasswFromAuth = passwordFromAuthUser authUser
      case mpasswFromAuth of
        Nothing -> do logMessage ERROR "No password was given"
                      withTop debugLoggerContext $ debugMessage "Login.loginSubmit"
                      A.logout
        Just _passwFromAuth -> do
          i18n <- i18nH
          result <- liftIO $ S.runUserStory context i18n UserNotLoggedIn $ do
            S.login unameFromAuth token
            S.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ S.translateUserError trans err
              withTop debugLoggerContext $ debugMessage "Login.loginSubmit2"
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              A.logout
              withTop sessionManager $ commitSession
              translationErrorPage
                (Msg_Login_PageTitle "Login")
                (Msg_Login_InternalError
                   "Some internal error happened, please contact the administrators.")
            Right (user,userState) -> do
              initSessionValues (page userState) unameFromAuth (u_language user)
              withTop sessionManager $ commitSession
              redirect "/"
  return ()
  where
    handleError m =
      m >>= (either (login . Just . AuthError . contentHandlerErrorMsg) (const $ return ()))

    initSessionValues :: P.PageDesc -> Username -> Language -> BeadHandler' b ()
    initSessionValues page username language = do
      withTop sessionManager $ do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username
      withTop serviceContext $ do
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page

visibleFailure :: I18N -> AuthFailure -> Maybe AuthFailure
visibleFailure _   (AuthError e)     = Just $ AuthError e
visibleFailure msg IncorrectPassword = Just . AuthError . msg $ Msg_Login_InvalidPasswordOrUser "Invalid user or password!"
visibleFailure msg UserNotFound      = Just . AuthError . msg $ Msg_Login_InvalidPasswordOrUser "Invalid user or password!"
visibleFailure _   _ = Nothing

-- * Change language in the session

changeLanguage :: BeadHandler ()
changeLanguage = method GET setLanguage <|> method POST (redirect "/") where
  setLanguage = withTop sessionManager $ do
    elang <- getParameterOrError changeLanguagePrm
    either
      (liftIO . putStrLn)
      (\l -> withTop sessionManager $ do
                setLanguageInSession l
                commitSession)
      elang -- TODO: Log the error message
    redirect "/"
