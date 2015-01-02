{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Login (
    login
  , loginSubmit
  , changeLanguage
  ) where

import           Data.ByteString.Char8 hiding (index, putStrLn)
import           Data.Maybe
import qualified Data.Text as Text
import           Prelude as P

import           Snap.Snaplet.Auth as Auth
import           Snap.Snaplet.Session

import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as Story
import           Bead.View.BeadContext
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Public.Login as View
import           Bead.View.ContentHandler
import           Bead.View.ErrorPage
import           Bead.View.Session


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

data LDAPResult
  = LDAPError
  | LDAPInvalidAuth
  | LDAPUser (Username, Email, String, TimeZoneName, Language)
  deriving (Eq, Show)

ldapResult
  ldapError
  ldapInvalidAuth
  ldapUser
  l = case l of
    LDAPError -> ldapError
    LDAPInvalidAuth -> ldapInvalidAuth
    LDAPUser user -> ldapUser user

loginSubmit :: BeadHandler' b ()
#ifdef LDAP
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  username <- getParameter loginUsernamePrm
  pwd      <- getParameter loginPasswordPrm
  lResult  <- ldapLogin user pwd
  ldapResult (ldapError username pwd) ldapInvalidUser (ldapUser username pwd) lResult
  where
    ldapLogin _ _ = return LDAPError

    -- Looks up the user in the auth module and throws an error it the user is not found
    lookupUser username = do
      authUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameCata Text.pack username)
      when (isNothing authUser) . throwError . strMsg $ "User was not created in the Snap Auth module"
      return $ fromJust authUser

    -- Runs a user story in the registration context, and throws an error if the registration
    -- story has failed
    regStory story = checkFailure =<< (lift $ registrationStory story)

    -- Falls back to local credentials
    ldapError username pwd = beadLogin username pwd

    ldapInvalidUser = lift . login . Just $ IncorrectPassword

    ldapUser ldapUsername pwd (username,email,name,timezone,lang) = do
      -- Check if the user exist
      let packedPwd = pack pwd
      let user role = User role username email name timezone lang
      exist <- regStory (Story.doesUserExist username)
      case exist of
        False -> do
          -- If the user does not exist, create a user with the given profile related informations
          -- Registers the user in the Snap authentication module
          lift $ registerUser (usernameCata pack ldapUsername) packedPwd
          -- Check if the Snap Auth registration went fine
          snapAuthUser <- lookupUser username
          when (isNothing . passwordFromAuthUser $ snapAuthUser) . throwError . strMsg $ "Snap Auth: no password is created"
          let snapAuthPwd = fromJust . passwordFromAuthUser $ snapAuthUser
          -- Creates the user in the persistence layer
          _ <- regStory (Story.createUser $ user Student)
          return ()

        True -> do
          -- If the user exists update its profile and password
          authUser <- lookupUser username
          beadUser <- regStory (Story.loadUser username)
          lift $ liftIO $ Auth.setPassword authUser packedPwd
          _ <- regStory $ Story.updateUser (user $ u_role beadUser)
          return ()

      beadLogin username pwd

    -- Tries to make log in the user with the given password in the snap auth module and in the service context
    beadLogin username pwd = do
      -- Force login on the user
      result <- lift $ loginByUsername (usernameCata Text.pack username) (ClearText $ pack pwd) False
      case result of
        Left fail -> throwError . strMsg $ show fail
        Right _ -> return ()
      i18n    <- lift $ i18nH
      context <- lift $ withTop serviceContext getServiceContext
      token   <- lift $ sessionToken
      result  <- liftIO $ Story.runUserStory context i18n UserNotLoggedIn $ do
        Story.login username token
        Story.currentUser
      lift $ case result of
        Left err -> do
          logMessage ERROR $ "Error happened processing user story: " ++ Story.translateUserError trans err
          -- Service context authentication
          liftIO $ (userContainer context) `userLogsOut` (userToken (username, token))
          Auth.logout
          withTop sessionManager $ commitSession
          translationErrorPage
            (Msg_Login_PageTitle "Login")
            (Msg_Login_InternalError "Some internal error happened, please contact the administrators.")
        Right (user,userState) -> do
          initSessionValues (page userState) username (u_language user)
          withTop sessionManager $ commitSession
          redirect "/"

    -- Checks if the result of a story is failure, in the case of failure
    -- it throws an exception, otherwise lift's the result into the monadic
    -- calculation
    checkFailure (Left _)  = throwError $ strMsg "User story failed"
    checkFailure (Right x) = return x

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
#else
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  user <- getParameter loginUsernamePrm
  pwd  <- getParameter loginPasswordPrm
  loggedIn <- lift $ loginByUsername
    (usernameCata Text.pack user)
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
                      Auth.logout
        Just _passwFromAuth -> do
          i18n <- i18nH
          result <- liftIO $ Story.runUserStory context i18n UserNotLoggedIn $ do
            Story.login unameFromAuth token
            Story.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ Story.translateUserError trans err
              withTop debugLoggerContext $ debugMessage "Login.loginSubmit2"
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              Auth.logout
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
#endif

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
