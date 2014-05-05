{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
module Bead.View.Snap.Routing (
    routes
  , pages
#ifdef TEST
  , routingInvariants
#endif
  ) where

import           Control.Arrow
import qualified Control.Exception as CE
import qualified Control.Monad.Error as CME
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Prelude hiding (id)
import qualified Prelude as P

import qualified Data.ByteString.Char8 as BS
import           Snap.Snaplet.Auth as A
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Session
import           Snap.Util.FileServe (serveDirectory)
import           Text.Printf (printf)

import           Bead.Configuration (Config(..))
import           Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as SC hiding (serviceContext)
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities as E
import           Bead.View.Snap.Application
import           Bead.View.Snap.Content hiding (BlazeTemplate, template)
import           Bead.View.Snap.Content.All
import           Bead.View.Snap.HandlerUtils as HU
import           Bead.View.Snap.ErrorPage
import           Bead.View.Snap.Login as L
import           Bead.View.Snap.Registration
import           Bead.View.Snap.ResetPassword
import           Bead.View.Snap.RouteOf
import           Bead.View.Snap.RequestParams
import           Bead.View.Snap.Session

#ifdef TEST
import           Bead.Invariants (Invariants(..))
#endif

{-
Routing of a given request based on the path of the request. A page handler
is selected of the path is known otherwise an error page is rendered.
-}

-- * Route table

routes :: Config -> [(ByteString, Handler App App ())]
routes config = join
  [ -- Add login handlers
    [ ("/",         index)
    , ("/logout",   logoutAndResetRoute)
    , ("/reset_pwd",resetPasswordPage)
    , ("/reg_request", registrationRequest config)
    , ("/reg_final", finalizeRegistration)
    , ("/change-language", changeLanguage)
    , ("/fay", with fayContext fayServe)
    , ("/upload", fileUpload)
    ]
    -- Add static handlers
  , [ ("",          serveDirectory "static") ]
  ]

pages :: Handler App App ()
pages = do
  path <- getRequest >>= return . proc req -> do
            ctx <- rqContextPath -< req
            pth <- rqPathInfo    -< req
            returnA -< (BS.append ctx pth)
  page <- requestToPageHandler path
  case page of
    -- No Page value is calculated from the request
    Nothing -> logoutAndErrorPage "Invalid route in request"
    Just pd
      | P.isLogin pd -> loginSubmit
      | otherwise ->  handlePage (pageContent pd)

-- * Handlers

index :: Handler App App ()
index =
  ifTop $ requireUser auth
            (with auth $ login Nothing)
            (redirect (routeOf $ P.home ()))

-- TODO: I18N
{- Logged In user combinator. It tries to authenticate the user with three methods.
   The first method authenticate it using Snap auth, the second method authenticates
   it using the session encoded user information. The third method authenticates it
   using the service context. There is an extra criteria for the user, it's session
   must be the same as the actual version, otherwise the authentication fails
-}
userIsLoggedInFilter
  :: Handler App b HandlerResult
  -> Handler App b ()
  -> (String -> Handler App b ())
  -> Handler App b ()
userIsLoggedInFilter inside outside onError = do
  sessionVer <- withTop sessionManager $ getSessionVersion
  case sessionVer of
    -- Session timed out
    Nothing -> onError "Lejárt a munkamenet!"
    -- Active session
    Just _ -> do
      e <- CME.runErrorT loggedInFilter
      case e of
        Right () -> return ()
        Left e' -> errorHappened . show $ e'

  where
    errorHappened e = do
      logMessage ERROR e
      outside

    someExceptionHandler :: (String -> Handler App b ()) -> CE.SomeException -> Handler App b ()
    someExceptionHandler onError e = do
      logMessage ERROR $ "Exception occured, redirecting to error page. " ++ show e
      onError $ show e

    loggedInFilter = do
      -- Authenticated user information
      serverSideUser <- lift . withTop auth           $ currentUser
      sessionVer     <- lift . withTop sessionManager $ getSessionVersion

      -- Get username form session
      tkn <- lift sessionToken
      usernameFromSession <- lift . withTop sessionManager $ usernameFromSession
      let throwError msg = CME.throwError . strMsg $ concat [show usernameFromSession, " ", show tkn, ": ", msg]

      -- Guards: invalid session version or invalid user
      when (sessionVer /= (Just sessionVersion)) $ throwError "Nem megfelelő a munkamenet verziója!"
      when (isNothing serverSideUser)            $ throwError "Ismeretlen felhasználó!"

      -- Username and page from session
      let unameFromAuth = usernameFromAuthUser . fromJust $ serverSideUser

      -- Guard: invalid user in session
      when (usernameFromSession /= (Just unameFromAuth)) . throwError $
        printf "Hibás felhasználó a munkamenetben: %s, %s." (show unameFromAuth) (show usernameFromSession)

      -- Guard: Is user logged in?
      context <- lift . withTop serviceContext $ getServiceContext
      let users = userContainer context
          usrToken = userToken (unameFromAuth, tkn)
      isLoggedIn <- lift (liftIO $ users `isUserLoggedIn` usrToken)
      unless (isLoggedIn) $ throwError "A felhasználó a szerveren nincs bejelentkezve!"

      -- Correct user is logged in, run the handler and save the data
      result <- lift inside
      case result of
        HFailure -> lift $ HU.logout
        HSuccess -> do
          mUserData <- lift (liftIO $ users `userData` usrToken)

          when (isNothing mUserData) . CME.throwError . contentHandlerError $
            printf "Nem található adat a felhasználóhoz: %s %s" (show unameFromAuth) (show tkn)

-- Redirects to the parent page of the given page
redirectToParentPage :: P.PageDesc -> HandlerError App b ()
redirectToParentPage = maybe (return ()) (redirect . routeOf) . P.parentPage

-- | Represents the result of a GET or POST handler
-- HandSuccess when no exception occured during the execution
-- HandFailure when some exception occured
data HandlerResult
  = HSuccess
  | HFailure
  deriving (Eq)

hsuccess :: Handler App a b -> Handler App a HandlerResult
hsuccess h = h >> (return HSuccess)

hfailure :: Handler App a b -> Handler App a HandlerResult
hfailure h = h >> (return HFailure)

evalHandlerError
  :: (ContentHandlerError -> Handler App b a)
  -> (c -> Handler App b a)
  -> HandlerError App b c
  -> Handler App b a
evalHandlerError onError onSuccess h = do
  x <- runErrorT h
  case x of
    Left e  -> onError e
    Right s -> onSuccess s

-- Runs the handler and clears the status message, is any error occurs
-- the onError handler is run, in both cases returns information about
-- the successfullness.
runGETHandler
  :: (ContentHandlerError -> Handler App App ())
  -> HandlerError App App ()
  -> Handler App App HandlerResult
runGETHandler onError handler
  = evalHandlerError
      (hfailure . onError)
      (\_ -> return HSuccess)
      (do handler
          userStory S.clearStatusMessage
          lift . with sessionManager $ do
            touchSession
            commitSession)

-- Runs the 'h' handler if no error occurs during the run of the handler
-- calculates the parent page for the given 'p', and runs the attached userstory
-- from the calculated user action
-- and redirects at the end to the parent page, if the page is not
-- a temporary view page, otherwise runs the onError handler
-- in both ways returns information about the successfulness.
runPOSTHandler
  :: (ContentHandlerError -> Handler App App ())
  -> P.PageDesc
  -> HandlerError App App UserAction
  -> Handler App App HandlerResult
runPOSTHandler onError p h
  = evalHandlerError
      (hfailure . onError)
      (\_ -> return HSuccess)
      (do userAction <- h
          let userView = P.isUserViewPage p
          userStory $ do
            userStoryFor userAction
            unless userView $ changeToParentPage p
          lift . with sessionManager $ do
            touchSession
            commitSession
          unless userView $ redirectToParentPage p)
  where
    changeToParentPage = maybe (return ()) S.changePage . P.parentPage

runUserViewPOSTHandler
  :: (ContentHandlerError -> Handler App App ())
  -> HandlerError App App ()
  -> Handler App App HandlerResult
runUserViewPOSTHandler onError userViewHandler
  = evalHandlerError
      (hfailure . onError)
      (\_ -> return HSuccess)
      (do userViewHandler
          lift . with sessionManager $ do
            touchSession
            commitSession)

logoutAndResetRoute :: Handler App App ()
logoutAndResetRoute = do
  HU.logout
  redirect "/"

logoutAndErrorPage :: String -> Handler App App ()
logoutAndErrorPage msg = do
  HU.logout
  msgErrorPage msg

{- When a user logs in the home page is shown for her. An universal handler
   is used. E.g "/home" -> handlePage P.Home.
   * If the user can navigate to the
   intented page from its state, it's state is going to change in his session
   and in the server side as well.
   * When a user submits information with a POST request, from the submitted information
   we calculate the appropiate user action and runs it
-}
handlePage :: PageHandler -> Handler App App ()
handlePage page = P.pageKindCata view userView viewModify modify page where
  pageDesc = P.pageToPageDesc page

  loggedInFilter m = userIsLoggedInFilter
    m
    -- Not logged in user tries to get some data
    logoutAndResetRoute
    -- Some internal error happened
    logoutAndErrorPage

  invalidPOSTMethodCall = do
     logMessage DEBUG $ "Invalid POST handler " ++ show pageDesc
     logoutAndResetRoute

  invalidGETMethodCall = do
     logMessage DEBUG $ "Invalid GET handler" ++ show pageDesc
     logoutAndResetRoute

  changePage handler =
    allowedPageByTransition pageDesc
      ((lift $ runStory $ S.changePage pageDesc) >> handler)
      notAllowedPage

  notAllowedPage = withUserState $ \s -> do
    lift $ logMessage ERROR . join $ [
        usernameCata show (user s)
      , ": Page transition is not allowed "
      , show (SC.page s), " -> ", show pageDesc
      ]
    lift $ logoutAndResetRoute

  get  h = method GET h <|> method POST invalidPOSTMethodCall
  post h = method GET invalidGETMethodCall <|> method POST h
  getPost g p = method GET g <|> method POST p

  runGetOrError h = runGETHandler defErrorPage (changePage h)

  runPostOrError h = do
    runStory $ S.changePage pageDesc
    runPOSTHandler defErrorPage pageDesc h

  runUserViewPostOrError h = do
    runStory $ S.changePage pageDesc
    runUserViewPOSTHandler defErrorPage h

  view = viewHandlerCata (get . loggedInFilter . runGetOrError) . P.viewPageValue

  userView = userViewHandlerCata (post . loggedInFilter . runUserViewPostOrError) . P.userViewPageValue

  viewModify = viewModifyHandlerCata
    (\get post -> getPost (loggedInFilter $ runGetOrError  get)
                          (loggedInFilter $ runPostOrError post))
    . P.viewModifyPageValue

  modify = modifyHandlerCata (post . loggedInFilter . runPostOrError) . P.modifyPageValue


allowedPageByTransition
  :: P.Page a b c d -> HandlerError App App a -> HandlerError App App a -> HandlerError App App a
allowedPageByTransition p allowed restricted = withUserState $ \state ->
  let allow = P.allowedPage (role state) p
  in case allow of
    False -> restricted
    True  -> allowed

-- Creates a handler, that tries to calculate a Page value
-- from the requested route and the parameters of the request uri
requestToPageHandler :: RoutePath -> Handler App App (Maybe P.PageDesc)
requestToPageHandler path = requestToPage path <$> getParams

-- Calculates a Just page if the route is a valid route path
-- and all the parameters were given is the params for the
-- routePath necesary for the Page value, otherwise Nothing
requestToPage :: RoutePath -> Params -> Maybe P.PageDesc
requestToPage path params = do
  page <- Map.lookup path routeToPageMap
  page params

routeToPageMap :: Map RoutePath (Params -> Maybe P.PageDesc)
routeToPageMap = Map.fromList [
    (loginPath       , j $ P.login ())
  , (logoutPath      , j $ P.logout ())
  , (homePath        , j $ P.home ())
  , (profilePath     , j $ P.profile ())
  , (courseAdminPath , j $ P.courseAdmin ())
  , (courseOverviewPath , \ps -> P.courseOverview <$> courseKey ps <*> unit)
  , (modifyEvaluationPath , \ps -> P.modifyEvaluation <$> submissionKey ps <*> evaluationKey ps <*> unit)
  , (evaluationTablePath  , j $ P.evaluationTable ())
  , (evaluationPath , \ps -> P.evaluation <$> submissionKey ps <*> unit)
  , (submissionPath , j $ P.submission ())
  , (submissionListPath   , j $ P.submissionList ())
  , (userSubmissionsPath  , j $ P.userSubmissions ())
  , (newTestScriptPath    , j $ P.newTestScript ())
  , (modifyTestScriptPath , \ps -> P.modifyTestScript <$> testScriptKey ps <*> unit)
  , (uploadFilePath , j $ P.uploadFile ())
  , (submissionDetailsPath , \ps -> P.submissionDetails <$> assignmentKey ps <*> submissionKey ps <*> unit)
  , (administrationPath    , j $ P.administration ())
  , (groupRegistrationPath , j $ P.groupRegistration ())
  , (createCoursePath      , j $ P.createCourse ())
  , (userDetailsPath       , j $ P.userDetails ())
  , (assignCourseAdminPath , j $ P.assignCourseAdmin ())
  , (createGroupPath       , j $ P.createGroup ())
  , (assignGroupAdminPath  , j $ P.assignGroupAdmin ())
  , (newGroupAssignmentPath , \ps -> P.newGroupAssignment <$> groupKey ps <*> unit)
  , (newCourseAssignmentPath , \ps -> P.newCourseAssignment <$> courseKey ps <*> unit)
  , (modifyAssignmentPath , \ps -> P.modifyAssignment <$> assignmentKey ps <*> unit)
  , (viewAssignmentPath , \ps -> P.viewAssignment <$> assignmentKey ps <*> unit)
  , (newGroupAssignmentPreviewPath , \ps -> P.newGroupAssignmentPreview <$> groupKey ps <*> unit)
  , (newCourseAssignmentPreviewPath , \ps -> P.newCourseAssignmentPreview <$> courseKey ps <*> unit)
  , (modifyAssignmentPreviewPath , \ps -> P.modifyAssignmentPreview <$> assignmentKey ps <*> unit)
  , (changePasswordPath      , j $ P.changePassword ())
  , (setUserPasswordPath     , j $ P.setUserPassword ())
  , (commentFromEvaluationPath , \ps -> P.commentFromEvaluation <$> submissionKey ps <*> unit)
  , (commentFromModifyEvaluationPath , \ps -> P.commentFromModifyEvaluation <$> submissionKey ps <*> evaluationKey ps <*> unit)
  , (deleteUsersFromCoursePath , \ps -> P.deleteUsersFromCourse <$> courseKey ps <*> unit)
  , (deleteUsersFromGroupPath , \ps -> P.deleteUsersFromGroup <$> groupKey ps <*> unit)
  , (unsubscribeFromCoursePath , \ps -> P.unsubscribeFromCourse <$> groupKey ps <*> unit)
  ] where
      j = const . Just
      unit = return ()

      courseKey     = fmap (CourseKey     . unpack) . value courseKeyParamName
      groupKey      = fmap (GroupKey      . unpack) . value groupKeyParamName
      assignmentKey = fmap (AssignmentKey . unpack) . value assignmentKeyParamName
      submissionKey = fmap (SubmissionKey . unpack) . value submissionKeyParamName
      evaluationKey = fmap (EvaluationKey . unpack) . value evaluationKeyParamName
      testScriptKey = fmap (TestScriptKey . unpack) . value testScriptKeyParamName

      -- Returns Just x if only one x corresponds to the key in the request params
      -- otherwise Nothing
      value key params = Map.lookup key params >>= oneValue
        where
          oneValue []  = Nothing
          oneValue [l] = Just l
          oneValue _   = Nothing


#ifdef TEST

requestToParams :: [ReqParam] -> Params
requestToParams = foldl insert Map.empty
  where
    insert m (ReqParam (name, value)) =
      Map.insert (fromString name) [(fromString value)] m

routingInvariants = Invariants [
    ("For each page must be requestToPage must be defined",
     \p -> let rp = P.pageValue $ pageRoutePath p
               ps = requestToParams . P.pageValue $ pageRequestParams p
           in requestToPage rp ps == Just p)
  ]

#endif
