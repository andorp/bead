{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Registration (
    createAdminUser
  , createUser
#ifdef EMAIL_REGISTRATION
  , registrationRequest
  , finalizeRegistration
#else
  , registration
#endif
  , backToMain
  ) where

-- Bead imports

import Bead.Controller.Logging as L

import qualified Bead.Controller.UserStories as S
import qualified Bead.Controller.Pages as P (Page(Login))
import Bead.Configuration (Config(..))
import Bead.View.Snap.Application
import Bead.View.Snap.Session
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.DataBridge
import Bead.View.Snap.ErrorPage (errorPageWithTitle)
import Bead.View.Snap.RouteOf (requestRoute)
import Bead.View.Snap.EmailTemplate
import qualified Bead.Persistence.Persist as P (Persist(..), runPersist)

import Bead.View.Snap.Content hiding (
    BlazeTemplate, name, template, empty, method
  )

-- Haskell imports

import Data.Maybe (fromJust, isNothing)
import Data.String (fromString)
import Data.Time
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Network.Mail.Mime
import Text.Printf (printf)

-- Snap and Blaze imports

import Snap hiding (Config(..), get)
import Snap.Snaplet.Auth as A hiding (createUser)
import Snap.Snaplet.Auth.Backends.JsonFile (mkJsonAuthMgr)

import Text.Blaze (textTag)
import Text.Blaze.Html5 (Html, (!)) -- hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)

createUser :: P.Persist -> FilePath -> User -> String -> IO ()
createUser persist usersdb user password = do
  let name = usernameCata id $ u_username user
  mgr <- mkJsonAuthMgr usersdb
  pwd <- encryptPassword . ClearText . fromString $ password
  let authUser = defAuthUser {
      userLogin    = fromString name
    , userPassword = Just pwd
    }
  save mgr authUser
  createdUser <- lookupByLogin mgr (T.pack name)
  case createdUser of
    Nothing -> error "Nem jött létre felhasználó!"
    Just u' -> case passwordFromAuthUser u' of
      Nothing  -> error "Nem lett jelszó megadva!"
      Just pwd -> P.runPersist $ P.saveUser persist user
  return ()

createAdminUser :: P.Persist -> FilePath -> String -> String -> IO ()
createAdminUser persist usersdb name password =
  let usr = User {
      u_role = Admin
    , u_username = Username name
    , u_email = Email ""
    , u_name = ""
    , u_timezone = UTC
    }
  in createUser persist usersdb usr password
-- * User registration handler

data RegError
  = RegError LogLevel String
  | RegErrorUserExist Username

instance Error RegError where
  noMsg      = RegError DEBUG ""
  strMsg msg = RegError DEBUG msg

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . B.pack . name $ param
  return (reqParam >>= decode param . T.unpack . decodeUtf8)

backToMain :: String
backToMain = "Vissza a bejelentkezéshez"

registrationTitle :: String
registrationTitle = "Regisztráció"

#ifndef EMAIL_REGISTRATION
registration :: Handler App (AuthManager App) ()
registration = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = blaze $ newUser

    -- Registers a user as a Student, the administrator can grant
    -- better access to the user
    handleFormSubmit = do
      regResult <- runErrorT $ do
        -- Check if the user does not exist in the system
        uname <- readParameter regUsernamePrm
        when (isNothing uname) $ throwError (RegError ERROR "Username is not found in the request parameters")
        let username = fromJust uname
        context <- lift $ withTop serviceContext $ getServiceContext
        exist   <- liftIO $ S.runUserStory context Registration (S.doesUserExist username)
        case exist of
          Left _          -> throwError (RegError ERROR "User story failed")
          Right (True, _) -> throwError (RegErrorUserExist username)
          _               -> return ()

        -- Register the user in the Snap auth module
        lift $ registerUser
          (fieldName loginUsername)
          (fieldName loginPassword)
        -- Register the user in the service context module
        passwBS    <- readParameter regPasswordPrm -- (fieldName loginPassword)
        email      <- readParameter regEmailPrm    -- (fieldName regEmailAddress)
        fullname   <- getParam (fieldName regFullName)
        case (uname,email,fullname, passwBS) of
          (Just u,Just e, Just f, Just p) -> do
            -- Create a user info for the service context and
            -- read the created user info from the Snap auth service context
            -- and run the registration user story in the service context
            -- with a freshly created (and encrypted) user password
            -- in the user context
            let usr = User {
                  u_role = Student
                , u_username = u
                , u_email = e
                , u_name = unpack f
                , u_timezone = UTC
                }
            createdUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameFold T.pack u)
            when (isNothing createdUser) $ throwError (RegError ERROR "User was not created at the first stage")
            let user = fromJust createdUser
                mpwd = passwordFromAuthUser user
            when (isNothing mpwd) $ throwError (RegError ERROR "No password was given")
            let pwd = fromJust mpwd
            result <- liftIO $ S.runUserStory context Registration (S.createUser usr pwd)
            case result of
              Left err -> throwError (RegError ERROR (show err))
              _        -> throwError (RegError INFO "Everything went fine. The user is created.")

          _ -> throwError (RegError ERROR "Username, email, or family name was not provided by the form")
      case regResult of
        Left (RegErrorUserExist username) ->
          do withTop serviceContext $ logMessage INFO $ (usernameFold ("User already exist: "++) username)
             redirect "/"
        Left (RegError lvl msg) ->
          do withTop serviceContext $ logMessage lvl msg
             redirect "/"
        Right _ ->
          do redirect "/"

-- * Blaze

newUser :: Html
newUser = dynamicTitleAndHead registrationTitle content
  where
    content = do
      registrationForm "/new_user"
      linkToRoute backToMain

registrationForm :: String -> Html
registrationForm postAction = do
  postForm postAction ! (A.id . formId $ regForm) $ do
    table (fieldName registrationTable) (fieldName registrationTable) $ do
      tableLine "NEPTUN:"      $ textInput (fieldName loginUsername)     20 Nothing ! A.required ""
      tableLine "Jelszó:"      $ passwordInput (fieldName loginPassword) 20 Nothing ! A.required ""
      tableLine "Email cím:"   $ textInput (fieldName regEmailAddress)   20 Nothing ! A.required ""
      tableLine "Teljes név:"  $ textInput (fieldName regFullName)       20 Nothing ! A.required ""
    submitButton (fieldName regSubmitBtn) "Regisztráció"

#else
-- * New registration method

{-
User registration request
- On GET request it renders the HTML registration form with
  username, email address, and full name input fields, that
  has the proper JavaScript validation method.
- On POST request it validates the input fields.
  It the input field values are incorrect renders the error page, otherwise
  runs the User story to create a UserRegistration
  data in the persistence layer, after send the information via email
-}
registrationRequest :: Config -> Handler App App ()
registrationRequest config = method GET renderForm <|> method POST saveUserRegData where

  -- Creates a timeout days later than the given time
  timeout :: Integer -> UTCTime -> UTCTime
  timeout days = addUTCTime (fromInteger (60 * 60 * 24 * days))

  createUserRegData :: Username -> Email -> String -> IO UserRegistration
  createUserRegData user email name = do
    now <- getCurrentTime
    -- TODO random token
    return $ UserRegistration {
      reg_username = usernameCata id user
    , reg_email    = emailFold    id email
    , reg_name     = name
    , reg_token    = "token"
    , reg_timeout  = timeout 2 now
    }

  renderForm = blaze $ dynamicTitleAndHead registrationTitle $ do
    postForm "/reg_request" ! (A.id . formId $ regForm) $ do
      table (fieldName registrationTable) (fieldName registrationTable) $ do
        tableLine "NEPTUN:"     $ textInput (name regUsernamePrm)      20 Nothing ! A.required ""
        tableLine "Email cím:"  $ textInput (name regEmailPrm)    20 Nothing ! A.required ""
        tableLine "Teljes név:" $ textInput (name regFullNamePrm) 20 Nothing ! A.required ""
      submitButton (fieldName regSubmitBtn) "Regisztráció"
    linkToRoute backToMain

  saveUserRegData = do
    u <- readParameter regUsernamePrm
    e <- readParameter regEmailPrm
    f <- readParameter regFullNamePrm

    case (u,e,f) of
      (Nothing, _, _) -> errorPageWithTitle registrationTitle "Hibás NEPTUN-kód"
      (Just username, Just email, Just fullname) -> do
        userRegData <- liftIO $ createUserRegData username email fullname
        result <- registrationStory (S.createUserReg userRegData)
        case result of
          Left _ -> errorPageWithTitle registrationTitle "A regisztráció nem lett elmentve!"
          Right key -> do
             -- TODO: Send the email template
            withTop sendEmailContext $

              sendEmail
                email
                "BE-AD: Regisztráció"
                RegTemplate {
                    regUsername = reg_username userRegData
                  , regUrl = createUserRegAddress key userRegData
                  }
            pageContent
      _ -> errorPageWithTitle registrationTitle "Valamelyik request paraméter hiányzik!"

  createUserRegAddress :: UserRegKey -> UserRegistration -> String
  createUserRegAddress key reg =
    -- TODO: Add the correct address of the server
    requestRoute (join [emailHostname config, "/reg_final"])
                 [ requestParameter regUserRegKeyPrm key
                 , requestParameter regTokenPrm      (reg_token reg)
                 , requestParameter regUsernamePrm   (Username . reg_username $ reg)
                 ]

{-
Registration finalization
- On GET request: The user gets and email from the BE-AD this email contains
  the necessary code and token in for to finalize the registration
  The system reads the UserREgistration data and decides that the registration
  can go on, this depends on the factor, the first, that the user is not registered yet
  and the registration time limit has not passed yet.
  If the registration is not permitted the system renders the error page, otherwise
  a page where the user can enter the desired password. The password field is validated
  by JavaScript.
- On POST request the desired password is validated by server side too, if the validation
  is passed than the user registration happens. If any error occurs during the registration
  an error page is shown, otherwise the page is redirected to "/"
-}
finalizeRegistration :: Handler App App ()
finalizeRegistration = method GET renderForm <|> method POST createStudent where

  readRegParameters = do
    username <- readParameter regUsernamePrm
    key      <- readParameter regUserRegKeyPrm
    token    <- readParameter regTokenPrm
    case (key, token, username) of
      (Just k, Just t, Just u) -> return $ Just (k,t,u)
      _                        -> return $ Nothing

  renderForm = do
    values <- readRegParameters

    case values of
      Nothing -> errorPageWithTitle registrationTitle "Nincsenek regisztrációs paraméterek!"
      Just (key, token, username) -> do
        result <- registrationStory $ do
                    userReg   <- S.loadUserReg key
                    existence <- S.doesUserExist username
                    return (userReg, existence)
        case result of
          Left e -> errorPageWithTitle registrationTitle $ printf "Valami hiba történt: %s" (show e)
          Right (userRegData,exist) -> do
            -- TODO: Check username and token values
            now <- liftIO $ getCurrentTime
            case (reg_timeout userRegData < now, exist) of
              (True , _) -> errorPageWithTitle
                registrationTitle
                "A regisztrációs token lejárt, regisztrálj újra!"
              (False, True) -> errorPageWithTitle
                registrationTitle
                "Ez a felhasználó már létezik!"
              (False, False) -> blaze $ dynamicTitleAndHead registrationTitle $ do
                postForm "reg_final" ! (A.id . formId $ regFinalForm) $ do
                  table (fieldName registrationTable) (fieldName registrationTable) $ do
                    tableLine "Jelszó:" $ passwordInput (name regPasswordPrm) 20 Nothing ! A.required ""
                    tableLine "Jelszó (ismét):" $ passwordInput (name regPasswordAgainPrm) 20 Nothing ! A.required ""
                  hiddenParam regUserRegKeyPrm key
                  hiddenParam regTokenPrm      token
                  hiddenParam regUsernamePrm   username
                  submitButton (fieldName regSubmitBtn) "Regisztráció"
                linkToRoute backToMain

  hiddenParam parameter value = hiddenInput (name parameter) (encode parameter value)

  createStudent = do
    values <- readRegParameters
    pwd    <- readParameter regPasswordPrm
    case (values, pwd) of
      (Nothing,_) -> errorPageWithTitle registrationTitle "Nincsenek regisztrációs paraméterek!" -- TODO
      (Just (key, token, username), Just password) -> do
        result <- registrationStory (S.loadUserReg key)
        case result of
          Left e -> errorPageWithTitle registrationTitle "Valamilyen belső hiba történt!" -- TODO
          Right userRegData -> do
            now <- liftIO getCurrentTime
            -- TODO: Check username and token values (are the same as in the persistence)
            case (reg_timeout userRegData < now) of
              True -> errorPageWithTitle registrationTitle "A regisztrációs token már lejárt, regisztrálj újra!"
              False -> do
                result <- withTop auth $ createNewUser userRegData password
                redirect "/"

  log lvl msg = withTop serviceContext $ logMessage lvl msg



createNewUser :: UserRegistration -> String -> Handler App (AuthManager App) (Either RegError ())
createNewUser reg password = runErrorT $ do
  -- Check if the user is exist already
  userExistence <- checkFailure =<< lift (registrationStory (S.doesUserExist username))
  when userExistence . throwError $ (RegErrorUserExist username)

  -- Registers the user in the Snap authentication module
  lift $ registerUser (B.pack $ name regUsernamePrm) (B.pack $ name regPasswordPrm)
  let user = User {
      u_role = Student
    , u_username = username
    , u_email = email
    , u_name = fullname
    , u_timezone = UTC
    }

  -- Check if the Snap Auth registration went fine
  createdUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameCata T.pack username)
  when (isNothing createdUser) $ throwError (RegError ERROR "User was not created in the Snap Auth module")
  let snapAuthUser = fromJust createdUser
  when (isNothing . passwordFromAuthUser $ snapAuthUser) $ throwError (RegError ERROR "Snap Auth: no password is created")
  let snapAuthPwd = fromJust . passwordFromAuthUser $ snapAuthUser

  -- Creates the user in the persistence layer
  checkFailure =<< lift (registrationStory (S.createUser user))
  return ()

  where
    username = Username . reg_username $ reg
    email    = Email . reg_email $ reg
    fullname = reg_name reg

    -- Checks if the result of a story is failure, in the case of failure
    -- it throws an exception, otherwise lift's the result into the monadic
    -- calculation
    checkFailure (Left _)  = throwError (RegError ERROR "User story failed")
    checkFailure (Right x) = return x

pageContent :: Handler App a ()
pageContent = blaze $ dynamicTitleAndHead registrationTitle $ do
  H.p $ "A regisztrációs tokent elküldtük levélben, nézd meg a leveleidet!"
  H.br
  linkToRoute backToMain

#endif
