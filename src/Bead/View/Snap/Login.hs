{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Login (
    login
  , loginSubmit
  , changeLanguage
  ) where

import           Data.ByteString.Char8 hiding (index, putStrLn)
import           Data.Maybe (isNothing)
import           Data.String
import qualified Data.Text as T
import           Prelude as P

import           Snap.Snaplet.Auth as A
import           Snap.Snaplet.Session
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as S
import           Bead.View.Snap.Application
import           Bead.View.Snap.Content hiding (BlazeTemplate, template)
import           Bead.View.Snap.Dictionary
import           Bead.View.Snap.ErrorPage
import           Bead.View.Snap.HandlerUtils
import           Bead.View.Snap.RouteOf
import           Bead.View.Snap.Session


-- * Login and Logout handlers

login :: Maybe AuthFailure -> Handler App b ()
login authError = do
  -- Set the default language in session if no information is found
  languages <- withTop dictionaryContext dcGetDictionaryInfos
  mLangInSession <- withTop sessionManager languageFromSession
  when (isNothing mLangInSession) . withTop sessionManager $ do
    defaultLang <- withTop dictionaryContext configuredDefaultDictionaryLanguage
    setLanguageInSession defaultLang
    commitSession

  -- Render the page content
  renderPublicPage $ loginPage authError languages

loginSubmit :: Handler App b ()
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  user <- getParameter loginUsernamePrm
  pwd  <- getParameter loginPasswordPrm
  loggedIn <- lift $ loginByUsername
    (usernameCata T.pack user)
    (ClearText . pack $ pwd)
    False
  case loggedIn of
    Left failure -> lift $ login $ visibleFailure $ failure
    Right authUser -> lift $ do
      context <- withTop serviceContext getServiceContext
      token   <- sessionToken
      let unameFromAuth = usernameFromAuthUser authUser
          mpasswFromAuth = passwordFromAuthUser authUser
      case mpasswFromAuth of
        Nothing -> do logMessage ERROR "No password was given"
                      A.logout
        Just passwFromAuth -> do
          i18n <- i18nH
          result <- liftIO $ S.runUserStory context i18n UserNotLoggedIn $ do
            S.login unameFromAuth token
            S.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ S.translateUserError trans err
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

    initSessionValues :: P.PageDesc -> Username -> Language -> Handler App b ()
    initSessionValues page username language = do
      withTop sessionManager $ do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username
      withTop serviceContext $ do
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page

-- * Blaze --

userForm :: String -> IHtml
userForm act = do
  msg <- getI18N
  return $ postForm act $ do
    table (formId loginForm) (formId loginForm) $ do
      return ()
      tableLine (msg $ Msg_Login_Username "Username:") (textInput (fieldName loginUsername) 20 Nothing ! A.required "")
      tableLine (msg $ Msg_Login_Password "Password:") (passwordInput (fieldName loginPassword) 20 Nothing ! A.required "")
    submitButton (fieldName loginSubmitBtn) (msg $ Msg_Login_Submit "Login")

loginPage :: Maybe AuthFailure -> DictionaryInfos -> IHtml
loginPage err langInfos = withTitleAndHead (Msg_Login_Title "Login") content
  where
    content = do
      msg <- getI18N
      return $ do
        i18n msg $ userForm "/login"
        maybe (return ())
              ((H.p ! A.style "font-size: smaller") . fromString . show)
              err
        H.p $ do
          H.a ! A.href "/reg_request" $ fromString $ msg $ Msg_Login_Registration "Registration"
          H.br
          H.a ! A.href "/reset_pwd" $ fromString $ msg $ Msg_Login_Forgotten_Password "Forgotten password"
        when ((P.length langInfos) > 1) $ do
          H.p $ do
            forM_ langInfos $ \(language,info) -> do
              -- TODO: I18N put the icon
              H.a ! A.href (queryString changeLanguagePath [requestParam language])
                $ (dictionaryInfoCata (\_icon -> fromString) info)
              H.br

-- TODO: I18N
-- Keeps only the authentication failures which are
-- visible for the user
visibleFailure :: AuthFailure -> Maybe AuthFailure
visibleFailure (AuthError e)     = Just (AuthError e)
visibleFailure IncorrectPassword = Just (AuthError "Hibás jelszó!")
visibleFailure UserNotFound      = Just (AuthError "A felhasználó nem található!")
visibleFailure _ = Nothing

-- * Change language in the session

changeLanguage :: Handler App b ()
changeLanguage = method GET setLanguage <|> method POST (redirect "/") where
  setLanguage = withTop sessionManager $ do
    elang <- getParameterOrError changeLanguagePrm
    either
      (liftIO . putStrLn)
      (\l -> do setLanguageInSession l
                withTop sessionManager commitSession)
      elang -- TODO: Log the error message
    redirect "/"
