{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.PageHandlers (
    routes
  ) where

-- Bead imports

import Bead.Domain.Types
import Bead.Domain.Entities as E
import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.UserActions
import Bead.View.Snap.Application
import Bead.View.Snap.Blaze hiding (index)
import qualified Bead.View.Snap.Blaze as B (index)

-- Haskell imports

import Data.String
import Data.ByteString.Char8 hiding (index)
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List as L
import Control.Monad (join)
import Control.Arrow ((&&&))
import Control.Monad.Error (Error(..))
import qualified Control.Monad.CatchIO as CMC
import qualified Control.Exception as CE

-- Snap and Blaze imports

import Text.Blaze.Html (Html)
import Snap
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveDirectory)


-- * Route table

routes :: [(ByteString, Handler App App ())]
routes = join
  [ -- Add login handlers
    [ ("/",         index)
    , ("/logout",   handleLogout)
    , ("/new_user", with auth $ handleNewUser)
    ]
    -- Add all pages with template names and handlers
  , (L.map (routeOf &&& handlePage) P.allPages)
    -- Add static handlers
  , [ ("",          serveDirectory "static") ]
  ]

-- * Handlers

index :: Handler App App ()
index =
  ifTop $ requireUser auth (with auth $ handleLogin Nothing) loggedIn
  where
    loggedIn = do
      acc <- with auth $ fmap (maybe "" userLogin) currentUser
      blaze . B.index . Just . fromString . T.unpack $ acc

{- Logged In user combinator. It tries to authenticate the user with three methods.
   The first method authenticate it using Snap auth, the second method authenticates
   it using the session encoded user information. The third method authenticates it
   using the service context. There is an extra criteria for the user, it's session
   must be the same as the actual version, otherwise the authentication fails
-}
hLoggedInUser :: Handler App b () -> Handler App b () -> Handler App b ()
hLoggedInUser inside outside = do
  -- Login authentication
  um <- withTop auth $ currentUser
  sv <- withTop sessionManager $ getSessionVersion
  case (um,sv == (Just sessionVersion)) of
    (Nothing,_) -> do
      logMessage ERROR "Unknown user from session"
      outside
    (_, False) -> do
      logMessage ERROR "Invalid session version found"
      outside
    -- The user must be authenticated by the auth manager and also has to be the right session version
    (Just authUser, True) -> do
      -- Session cookie authentication
      let unameFromAuth = usernameFromAuthUser authUser
      unameFromSession <- withTop sessionManager $ usernameFromSession
      pageFromSession  <- withTop sessionManager $ actPageFromSession
      case (Just unameFromAuth) == unameFromSession of
        False -> do
          logMessage ERROR $ join [
              "hLoggedIn: invalid username from session ", show unameFromAuth
            , ", ", show unameFromSession
            ]
          outside
        True -> do
          context <- withTop serviceContext $ getServiceContext
          let users = userContainer context
          -- Service context authentication
          isLoggedIn <- liftIO $ users `isUserLoggedIn` unameFromAuth
          case isLoggedIn of
            False -> do
              logMessage ERROR "hLoggedIn: user is not logged in persistence"
              outside
            True -> do
              inside
              mUserData <- liftIO $ users `userData` unameFromAuth
              case mUserData of
                Nothing -> do
                  logMessage ERROR $ "No user data was found for the user " ++ show unameFromAuth
                  outside
                Just ud ->
                  CMC.catch
                    (withTop sessionManager . setActPageInSession . page $ ud)
                    someExceptionHandler
  where
    someExceptionHandler :: CE.SomeException -> Handler App b ()
    someExceptionHandler e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      errorPageHandler $ T.append "Exception occured; " (T.pack . show $ e)

-- | Runs a user story for authenticated user
loggedInUserStory :: S.UserStory a -> Handler App SnapletServiceContext (Either S.UserError a)
loggedInUserStory story = do
  result <- serviceContextAndUserData $ \context users authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      ustate <- liftIO $ userData users unameFromAuth
      case ustate of
        Nothing -> return . Left . strMsg $ "The user was not authenticated: " ++ show unameFromAuth
        Just state -> do
          eResult <- liftIO $ S.runUserStory context state story
          case eResult of
            Left e -> return . Left $ e
            Right (a,state') -> do
              liftIO $ modifyUserData users unameFromAuth (const state')
              saveActPage state'
              return $ Right a
  case result of
    Left msg -> return . Left . strMsg . show $ msg
    Right x -> return x

  where
    saveActPage state = withTop sessionManager $ setActPageInSession $ page state

    serviceContextAndUserData
      :: (ServiceContext -> UserContainer UserState -> AuthUser -> Handler App SnapletServiceContext a)
      -> Handler App SnapletServiceContext (Either String a)
    serviceContextAndUserData f = do
      context <- getServiceContext
      let users = userContainer context
      um <- withTop auth $ currentUser
      case um of
        Nothing -> return . Left $ "Unauthenticated user"
        Just authUser -> liftM Right $ f context users authUser


-- * Login and logout handlers

handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = blaze $ login

-- TODO: Handle multiple login attempts correctly
-- One user should just log in at once.
handleLoginSubmit :: Handler App b ()
handleLoginSubmit = do
  withTop auth $ loginUser
    (fieldName loginUsername)
    (fieldName loginPassword)
    Nothing (\_ -> handleLogin err) $ do
      um <- currentUser
      case um of
        Nothing -> do
          logMessage ERROR $ "User is not logged during login submittion process"
          return ()
        Just authUser -> do
          context <- withTop serviceContext getServiceContext
          let unameFromAuth = usernameFromAuthUser authUser
              passwFromAuth = passwordFromAuthUser authUser
          result <- liftIO $ S.runUserStory context UserNotLoggedIn (S.login unameFromAuth passwFromAuth)
          case result of
            Right (val,userState) -> initSessionValues (page userState) unameFromAuth
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ show err
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` unameFromAuth
              logout

  withTop sessionManager $ commitSession
  redirect "/"

  where
    err = Just . T.pack $ "Unknown user or password"

    initSessionValues :: P.Page -> Username -> Handler App b ()
    initSessionValues page username = do
      withTop sessionManager $ do
        setSessionVersion
        setUsernameInSession username
        setActPageInSession  page

      withTop serviceContext $ do
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page

handleLogout :: Handler App b ()
handleLogout = do
  um <- withTop auth $ currentUser
  case um of

    Nothing -> do
      logMessage ERROR "There is no user logged in to log out."
      redirect "/"

    Just authUser -> do
      let unameFromAuth = usernameFromAuthUser authUser
      context <- withTop serviceContext $ getServiceContext
      let users = userContainer context
      -- Service context authentication
      liftIO $ users `userLogsOut` unameFromAuth
      withTop auth logout
      redirect "/"

-- * User registration handler

-- TODO: Create the appropiate form
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = blaze $ newUser

    -- Registers a user as a Student, the administrator can grant
    -- better access to the user
    handleFormSubmit = do
      -- Register the user in the Snap auth module
      registerUser
        (fieldName loginUsername)
        (fieldName loginPassword)
      -- Register the user in the service context module
      uname      <- getParam (fieldName loginUsername)
      passwBS    <- getParam (fieldName loginPassword)
      email      <- getParam (fieldName registrationEmailAddress)
      familyname <- getParam (fieldName registrationFamilyName)
      case (uname,email,familyname) of
        (Just u,Just e, Just f) -> do
          -- Create a user info for the service context and
          -- read the created user info from the Snap auth service context
          -- and run the registration user story in the service context
          -- with a freshly created (and encrypted) user password
          -- in the user context
          let usr = User {
                u_role = Student
              , u_username = asUsername u
              , u_email = Email . unpack $ e
              , u_name = unpack f
              }
          context <- withTop serviceContext $ getServiceContext
          createdUser <- withBackend $ \r -> liftIO $ lookupByLogin r (T.pack $ unpack u)
          case createdUser of
            Nothing -> withTop serviceContext . logMessage ERROR $
                         "User was not created at the first stage"
            Just u' -> do
              result  <- liftIO $ S.runUserStory context UserNotLoggedIn
                           (S.createUser usr (passwordFromAuthUser u'))
              case result of
                Left err -> withTop serviceContext . logMessage ERROR . show $ err
                _        -> withTop serviceContext . logMessage INFO . show $
                              "Everything went fine. The user is created."

        _ -> withTop serviceContext . logMessage ERROR $
               "Username, email, or family name was not provided by the form"
      -- It does not matter what happens we redirects to the "/" page
      redirect "/"

-- | The 'logMessage' logs a message at a given level using the service context logger
logMessage :: LogLevel -> String -> Handler App b ()
logMessage lvl msg = do
  context <- withTop serviceContext $ getServiceContext
  liftIO $ L.log (logger context) lvl msg

userState :: Handler App b UserState
userState = do
  context   <- withTop serviceContext $ getServiceContext
  mUsername <- withTop sessionManager $ usernameFromSession
  case mUsername of
    Nothing -> do
      logMessage ERROR "User is not logged in the session"
      error "User is not logged in the session"
    Just user -> do
      let users = userContainer context
      userData <- liftIO $ users `userData` user
      case userData of
        Nothing -> do
          logMessage ERROR "No data found for the user"
          error "No data found for the user"
        Just ud -> return ud

-- | The 'redirectToActPage' redirects to the page if the user's state stored in the cookie
--   and the service state are the same, otherwise it's raises an exception
redirectToActPage :: Handler App b ()
redirectToActPage = do
  pageInSession <- withTop sessionManager $ actPageFromSession
  uState <- userState
  case pageInSession == Just (page uState) of
    False -> do
      logMessage ERROR $ "Actual page data stored in session and in the server differ"
      error $ "Actual page data stored in session and in the server differ"
    True ->  redirect . routeOf . page $ uState

{- When a user logs in the home page is shown for her. An universal handler
   is used. E.g "/home" -> handlePage P.Home.
   * If the user can navigate to the
   intented page from its state, it's state is going to change in his session
   and in the server side as well.
   * When a user submits information with a POST request, from the submitted information
   we calculate the appropiate user action and runs it
-}
handlePage :: P.Page -> Handler App App ()
handlePage P.Login = handleLoginSubmit
handlePage p = method GET (handleRenderPage p) <|> method POST (handleSubmitPage p)
  where
    handleRenderPage page = hLoggedInUser
      -- Logged in user GET data
      (maybe
         (do logMessage DEBUG $ "No GET handler found for " ++ show page
             handleLogout)
         id
         (handlerGET page))
      -- Not logged in user tries to get some data
      (do withTop sessionManager $ resetSession
          handleLogout)

    handleSubmitPage page = hLoggedInUser
      -- Logged in user POSTs data
      (case handlerPOST page of
         Nothing -> do
           logMessage DEBUG $ "No POST handler found for " ++ show page
           handleLogout
         Just handlerUserAction -> do
           userAction <- handlerUserAction
           -- let userAction = Profile -- .. TODO: calculate the user action
           ustate <- userState
           let story = userStoryFor ustate userAction
           with serviceContext $ loggedInUserStory story
           with sessionManager $ (commitSession >> touchSession)
           redirectToActPage)
      -- Not logged in user tires to post some data
      (do withTop sessionManager $ resetSession
          handleLogout)

renderPage :: P.Page -> Handler App b ()
renderPage p = blaze $ template p

renderOpenExam :: Handler App b ()
renderOpenExam = do
  let t = "This is your exercise"
  blaze $ base (exerciseTextArea t "area" "/openexam") Nothing

submitSolution :: Handler App b UserAction
submitSolution = do
  -- TODO: Get the necessary keys from the session
  return $ SubmitSolution undefined "This is the solution"

-- * Templating

class (BlazeTemplate h) => AppHandler h where
  handlerGET  :: h -> Maybe (Handler App App ())
  handlerPOST :: h -> Maybe (Handler App App UserAction)

instance AppHandler P.Page where
  handlerGET = h where
    j = Just
    h P.Login      = Nothing
    h P.OpenExam   = j $ renderOpenExam
    h p            = j $ renderPage p
  handlerPOST = h where
    j = Just
    h P.OpenExam  = j $ submitSolution
    h _           = Nothing

-- * Routing

routeOf :: P.Page -> ByteString
routeOf = r where
  r P.Login      = "/login"
  r P.Home       = "/home"
  r P.Profile    = "/profile"
  r P.Course     = "/course"
  r P.Group      = "/group"
  r P.OpenExam   = "/openexam"
  r P.ClosedExam = "/closedexam"
  r P.Error      = "/error"
  r P.SubmitExam = "/submitexam"
  r P.Evaulation = "/evaulation"
  r P.Training   = "/training"
  r P.Admin      = "/admin"
  r _            = error "routeOf"

-- TODO: Show some error
errorPageHandler :: T.Text -> Handler App b ()
errorPageHandler msg = blaze errorPage

-- * Session Management

class SessionStore s where
  sessionStore :: s -> [(T.Text, T.Text)]

class SessionRestore s where
  restoreFromSession :: [(T.Text, T.Text)] -> Maybe s

-- * Session Key and Values for Page

pageSessionKey :: T.Text
pageSessionKey = "Page"

instance SessionStore P.Page where
  sessionStore p = [(pageSessionKey, T.pack $ s p)] where
    s P.Login      = "Login"
    s P.Home       = "Home"
    s P.Profile    = "Profile"
    s P.Course     = "Course"
    s P.Group      = "Group"
    s P.OpenExam   = "OpenExam"
    s P.ClosedExam = "ClosedExam"
    s P.Error      = "Error"
    s P.SubmitExam = "SubmitExam"
    s P.Evaulation = "Evaulation"
    s P.Training   = "Training"
    s P.Admin      = "Admin"
    s p = error $ "Undefined SessionStore value for the page: " ++ show p

instance SessionRestore P.Page where
  restoreFromSession kv = case L.lookup pageSessionKey kv of
    Nothing           -> Nothing
    Just "Login"      -> Just P.Login
    Just "Home"       -> Just P.Home
    Just "Profile"    -> Just P.Profile
    Just "Course"     -> Just P.Course
    Just "Group"      -> Just P.Group
    Just "OpenExam"   -> Just P.OpenExam
    Just "ClosedExam" -> Just P.ClosedExam
    Just "Error"      -> Just P.Error
    Just "SubmitExam" -> Just P.SubmitExam
    Just "Evaulation" -> Just P.Evaulation
    Just "Training"   -> Just P.Training
    Just "Admin"      -> Just P.Admin

-- * Session Key Values for Username

usernameSessionKey :: T.Text
usernameSessionKey = "Username"

instance SessionStore E.Username where
  sessionStore (E.Username n) = [(usernameSessionKey, T.pack n)]

instance SessionRestore E.Username where
  restoreFromSession kv = case L.lookup usernameSessionKey kv of
    Nothing -> Nothing
    Just v -> Just $ E.Username $ T.unpack v

-- * Session handlers

sessionVersionKey :: T.Text
sessionVersionKey = "Version"

sessionVersionValue :: T.Text
sessionVersionValue = "1"

newtype SessionVersion = SessionVersion T.Text
  deriving (Eq)

sessionVersion = SessionVersion sessionVersionValue

instance SessionRestore SessionVersion where
  restoreFromSession kv = case L.lookup sessionVersionKey kv of
    Nothing -> Nothing
    Just v -> Just . SessionVersion $ v

setInSessionKeyValues :: [(T.Text, T.Text)] -> Handler App SessionManager ()
setInSessionKeyValues = mapM_ (\(key,value) -> setInSession key value)

fromSession :: (SessionRestore r) => T.Text -> Handler App SessionManager (Maybe r)
fromSession key = do
  v <- getFromSession key
  return $ join $ fmap (restoreFromSession . (\v' -> [(key,v')])) v

getSessionVersion :: Handler App SessionManager (Maybe SessionVersion)
getSessionVersion = fromSession sessionVersionKey

setSessionVersion :: Handler App SessionManager ()
setSessionVersion = setInSessionKeyValues [(sessionVersionKey, sessionVersionValue)]

usernameFromSession :: Handler App SessionManager (Maybe E.Username)
usernameFromSession = fromSession usernameSessionKey

setUsernameInSession :: Username -> Handler App SessionManager ()
setUsernameInSession = setInSessionKeyValues . sessionStore

actPageFromSession :: Handler App SessionManager (Maybe P.Page)
actPageFromSession = fromSession pageSessionKey

setActPageInSession :: P.Page -> Handler App SessionManager ()
setActPageInSession = setInSessionKeyValues . sessionStore

-- * Username and UserState correspondence

usernameFromAuthUser :: AuthUser -> Username
usernameFromAuthUser = E.Username . (T.unpack) . A.userLogin

passwordFromAuthUser :: AuthUser -> E.Password
passwordFromAuthUser a = case userPassword a of
  Just p  -> asPassword p
  Nothing -> error "passwordFromAuthUser: No password was given"

instance AsUsername ByteString where
  asUsername = E.Username . unpack

instance AsPassword ByteString where
  asPassword = unpack

instance AsPassword A.Password where
  asPassword (A.ClearText t) = unpack t
  asPassword (A.Encrypted e) = unpack e
