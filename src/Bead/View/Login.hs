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

import qualified Bead.Config as Config
import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as Story
#ifdef LDAPEnabled
import           Bead.Daemon.LDAP.Result
#endif
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
  languages <- dcGetDictionaryInfos
  mLangInSession <- languageFromSession
  when (isNothing mLangInSession) $ do
    defaultLang <- configuredDefaultDictionaryLanguage
    setLanguageInSession defaultLang
    commitSessionTop

  -- Render the page content
  renderBootstrapPublicPage . publicFrame $ do
    msg <- getI18N
    View.login (authError >>= visibleFailure msg) languages

loginSubmit :: BeadHandler' b ()
#ifdef LDAPEnabled
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  username <- getParameter loginUsernamePrm
  pwd      <- getParameter loginPasswordPrm
  needsLDAPAuth <- lift $ isLDAPUser username
  case needsLDAPAuth of
    False -> do
      beadLogin username pwd
    True -> do
      lResult <- lift $ ldapAuthenticate username pwd
      ldapResult
        (ldapError username pwd)
        (ldapInvalidUser username)
        (ldapAttrMapError username pwd)
        (ldapUser username pwd)
        lResult
  where
    -- Looks up the user in the auth module and throws an error it the user is not found
    lookupUser username = do
      authUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameCata Text.pack username)
      when (isNothing authUser) . throwError . strMsg $ "User was not created in the Snap Auth module"
      return $ fromJust authUser

    -- Saves the user to the user database
    saveUser authUser = do
      lift $ withBackend $ \r -> liftIO $ save r authUser

    -- Runs a user story in the registration context, and throws an error if the registration
    -- story has failed
    regStory story = checkFailure =<< (lift $ registrationStory story)

    -- Falls back to local credentials
    ldapError username pwd msg = do
      lift $ logMessage ERROR $ join ["LDAP ERROR fall back to normal login for ", usernameCata id username, " reason: ", msg]
      beadLogin username pwd

    ldapInvalidUser username = lift $ do
      logMessage ERROR $ join ["LDAP ERROR invalid user: ", usernameCata id username]
      login . Just $ IncorrectPassword

    -- Logs error and authenticates with the fallback
    ldapAttrMapError username pwd = do
      lift $ logMessage ERROR $ join ["LDAP ATTR MAPPING ERROR fall back to normal login for ", usernameCata id username]
      beadLogin username pwd

    ldapUser ldapUsername pwd (uid,email,name) = do
      -- Check if the user exist
      let packedPwd = pack pwd
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
          snapAuthUser <- lookupUser username
          when (isNothing . passwordFromAuthUser $ snapAuthUser) . throwError . strMsg $ "Snap Auth: no password is created"
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
          authUser <- lookupUser username
          authUser <- lift $ liftIO $ Auth.setPassword authUser packedPwd
          saveUser authUser
          beadUser <- regStory (Story.loadUser username)
          _ <- regStory $ Story.updateUser $ user (u_role beadUser) (u_timezone beadUser) (u_language beadUser)
          return ()

      beadLogin username pwd

    -- Tries to make log in the user with the given password in the snap auth module and in the service context
    beadLogin username pwd = do
      -- Force login on the user
      result <- lift $ loginByUsername (usernameCata Text.pack username) (ClearText $ pack pwd) False
      case result of
        Left fail -> throwError . strMsg $ join [usernameCata id username, ": ", show fail]
        Right _   -> return ()
      i18n    <- lift $ i18nH
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
            (Msg_Login_PageTitle "Login")
            (Msg_Login_InternalError "Some internal error happened, please contact the administrators.")
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
                                login $ Just IncorrectPassword)
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
                (Msg_Login_PageTitle "Login")
                (Msg_Login_InternalError
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
visibleFailure msg IncorrectPassword = Just . AuthError . msg $ Msg_Login_InvalidPasswordOrUser "Invalid user or password!"
visibleFailure msg UserNotFound      = Just . AuthError . msg $ Msg_Login_InvalidPasswordOrUser "Invalid user or password!"
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
