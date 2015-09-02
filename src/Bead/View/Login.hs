{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Login (
    login
  , loginSubmit
  , changeLanguage
  ) where

import           Data.ByteString.Char8 hiding (index, putStrLn)
import           Data.Char
import           Data.Maybe
import qualified Data.Text as Text
import           Prelude as P
import           Text.Printf

import           Snap.Snaplet.Auth as Auth
import           Snap.Snaplet.Session

import qualified Bead.Config as Config
import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as Story
#ifdef SSO
import           Bead.Daemon.LDAP.Result
#endif
import           Bead.View.BeadContext
import           Bead.View.Common
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Public.Login as View
import           Bead.View.ContentHandler
import           Bead.View.Headers.AcceptLanguage
import           Bead.View.ErrorPage
import           Bead.View.Session


-- * Login and Logout handlers

login :: Maybe AuthFailure -> BeadHandler' b ()
#ifdef SSO
login authError = do
  renderBootstrapPublicPage . publicFrame $ do
    msg <- getI18N
    View.login (authError >>= visibleFailure msg)
#else
login authError = do
  languages <- setDefaultLanguage
  renderBootstrapPublicPage . publicFrame $ do
    msg <- getI18N
    View.login (authError >>= visibleFailure msg) languages
#endif

loginSubmit :: BeadHandler' b ()
#ifdef SSO
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  cfg <- lift getConfiguration
  username <-
    if (Config.sSODeveloperMode $ Config.loginConfig cfg)
      then getParameter loginUsernamePrm
      else do
        headers <- getHeaders "X-Forwarded-User" <$> getRequest
        case headers of
          Just [hs] -> return (Username $ toUpper <$> unpack hs)
          other     -> do
            lift $ logMessage ERROR $ join ["[FORWARDED USER] Forwarded user is not unique, but: ", reason]
            i18n <- lift i18nH
            throwError . strMsg $ i18n $ msg_Login_Error_NoUser "User is unknown"
            where
              reason = maybe "nothing found" show other

  lResult <- lift $ ldapQuery username
  ldapResult
    (ldapError username)
    (ldapInvalidUser username)
    (ldapAttrMapError username)
    (ldapUser username)
    lResult
  where
    -- Looks up the user in the auth module and throws an error it the user is not found
    lookupUser username errorMsg = do
      authUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameCata Text.pack username)
      when (isNothing authUser) . throwError . strMsg $ errorMsg
      return $ fromJust authUser

    -- Saves the user to the user database
    saveUser authUser = do
      lift $ withBackend $ \r -> liftIO $ save r authUser

    -- Runs a user story in the registration context, and throws an error if the registration
    -- story has failed
    regStory story = checkFailure =<< (lift $ registrationStory story)

    -- Falls back to local credentials
    ldapError username msg = do
      lift $ logMessage ERROR $ join ["[LDAP] Query failed, falling back to normal login for ", usernameCata id username, ", reason: ", msg]
      beadLogin username

    ldapInvalidUser username = lift $ do
      logMessage ERROR $ join ["[LDAP] Invalid user: ", usernameCata id username]
      login . Just $ IncorrectPassword

    -- Logs error and authenticates with the fallback
    ldapAttrMapError username = do
      lift $ logMessage ERROR $ join ["[LDAP] Attributes cannot be mapped, falling back to normal login for ", usernameCata id username]
      beadLogin username

    ldapUser ldapUsername (uid,email,name) = do
      -- Check if the user exist
      packedPwd <- pack <$> lift getRandomPassword
      let username = ldapUsername
      let user role timezone lang = User role ldapUsername email name timezone lang uid
      exist <- regStory (Story.doesUserExist username)
      case exist of
        False -> do
          lift $ logMessage INFO $ join [usernameCata id ldapUsername, " registers."]
          -- If the user does not exist, create a user with the given profile related informations
          -- Registers the user in the Snap authentication module
          result <- lift $ createUser (usernameCata Text.pack ldapUsername) packedPwd
          case result of
            Left err -> throwError . strMsg $ show err
            Right _auth -> return ()
          -- Check if the Snap Auth registration went fine
          i18n <- lift i18nH
          snapAuthUser <- lookupUser username $
            printf (i18n $ msg_Login_Error_NoSnapCache "User %s could not be cached by Snap")
                   (usernameCata id username)
          when (isNothing . passwordFromAuthUser $ snapAuthUser) . throwError . strMsg $ "No password is created in the Snap Auth module"
          let snapAuthPwd = fromJust . passwordFromAuthUser $ snapAuthUser
          -- Creates the user in the persistence layer
          timezone <- fmap getTimeZone $ lift getConfiguration
          lang <- fmap (fromMaybe (Language "en")) $ lift languageFromSession
          _ <- regStory (Story.createUser $ user Student timezone lang)
          return ()
          where
            getTimeZone = TimeZoneName . Config.defaultRegistrationTimezone

        True -> do
          -- If the user exists update its profile and password
          i18n <- lift i18nH
          authUser <- lookupUser username $
            printf (i18n $ msg_Login_Error_NoSnapUpdate "User %s could not be updated by Snap")
            (usernameCata id username)
          authUser <- lift $ liftIO $ Auth.setPassword authUser packedPwd
          saveUser authUser
          beadUser <- regStory (Story.loadUser username)
          _ <- regStory $ Story.updateUser $ user (u_role beadUser) (u_timezone beadUser) (u_language beadUser)
          return ()

      beadLogin username

    -- Tries to make log in the user with the given password in the snap auth module and in the service context
    beadLogin username = do
      -- Force login on the user
      i18n <- lift i18nH
      authUser <- lookupUser username $
        printf (i18n $ msg_Login_Error_NoLDAPAttributes "Could not get LDAP attributes for user %s")
        (usernameCata id username)
      result <- lift $ forceLogin authUser
      case result of
        Left fail -> throwError . strMsg $ join [usernameCata id username, ": ", show fail]
        _         -> return ()
      context <- lift $ getServiceContext
      token   <- lift $ sessionToken
      result  <- liftIO $ Story.runUserStory context i18n UserNotLoggedIn $ do
        Story.login username token
        Story.currentUser
      lift $ case result of
        Left err -> do
          logMessage ERROR $ "Error happened processing user story: " ++ Story.translateUserError trans err
          -- Service context authentication
          liftIO $ (userContainer context) `userLogsOut` (userToken (username, token))
          logoutTop
          commitSessionTop
          translationErrorPage
            (msg_Login_PageTitle "Login")
            (msg_Login_InternalError "Some internal error happened, please contact the administrators.")
        Right (user,userState) -> do
          initSessionValues (page userState) username (u_language user)
          commitSessionTop
          redirect "/"

    -- Checks if the result of a story is failure, in the case of failure
    -- it throws an exception, otherwise lift's the result into the monadic
    -- calculation
    checkFailure (Left _)  = throwError $ strMsg "User story failed"
    checkFailure (Right x) = return x

    -- The error is logged, but it is not disposed to the user, instead
    -- a false incorrect login is rendered.
    handleError m =
      m >>= (either (\msg -> do logMessage ERROR $ join ["Error during login: ", contentErrorMsg msg]
                                login $ Just $ AuthError $ contentErrorMsg msg)
                    (const $ return ()))

    contentErrorMsg = contentError "Unknown" id

    initSessionValues :: P.PageDesc -> Username -> Language -> BeadHandler' b ()
    initSessionValues page username language = do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username
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
      context <- getServiceContext
      token   <- sessionToken
      let unameFromAuth = usernameFromAuthUser authUser
          mpasswFromAuth = passwordFromAuthUser authUser
      case mpasswFromAuth of
        Nothing -> do logMessage ERROR "No password was given"
                      Auth.logout
        Just _passwFromAuth -> do
          i18n <- i18nH
          result <- liftIO $ Story.runUserStory context i18n UserNotLoggedIn $ do
            Story.login unameFromAuth token
            Story.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ Story.translateUserError trans err
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              Auth.logout
              commitSessionTop
              translationErrorPage
                (msg_Login_PageTitle "Login")
                (msg_Login_InternalError
                   "Some internal error happened, please contact the administrators.")
            Right (user,userState) -> do
              initSessionValues (page userState) unameFromAuth (u_language user)
              commitSessionTop
              redirect "/"
  return ()
  where
    handleError m =
      m >>= (either (login . Just . AuthError . contentHandlerErrorMsg) (const $ return ()))

    initSessionValues :: P.PageDesc -> Username -> Language -> BeadHandler' b ()
    initSessionValues page username language = do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page
#endif

visibleFailure :: I18N -> AuthFailure -> Maybe AuthFailure
visibleFailure _   (AuthError e)     = Just $ AuthError e
visibleFailure msg IncorrectPassword = Just . AuthError . msg $ msg_Login_InvalidPasswordOrUser "Invalid user or password!"
visibleFailure msg UserNotFound      = Just . AuthError . msg $ msg_Login_InvalidPasswordOrUser "Invalid user or password!"
visibleFailure _   _ = Nothing

-- * Change language in the session

changeLanguage :: BeadHandler ()
changeLanguage = method GET setLanguage <|> method POST (redirect "/") where
  setLanguage = do
    elang <- getParameterOrError changeLanguagePrm
    either
      (logMessage ERROR . ("Change language " ++))
      (\l -> do setLanguageInSession l
                commitSessionTop)
      elang
    redirect "/"
