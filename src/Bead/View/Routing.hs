{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Routing (
    routes
  , pages
#ifdef TEST
  , routingTest
#endif
  ) where

import           Control.Arrow
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Prelude hiding (id)
import qualified Prelude

import qualified Data.ByteString.Char8 as BS
import           Snap.Snaplet.Auth as A
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Session
import           Snap.Util.FileServe (serveDirectory)

import           Bead.Config (Config(..))
import           Bead.Controller.Logging as L
import           Bead.Controller.ServiceContext as SC hiding (serviceContext)
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities as E
import           Bead.View.BeadContext
import qualified Bead.View.Command.Fayax as Command
import           Bead.View.Content hiding (BlazeTemplate, template, void)
import           Bead.View.ContentHandler as ContentHandler hiding (void)
import           Bead.View.Content.All
import           Bead.View.ErrorPage
import           Bead.View.Login as L
import           Bead.View.LoggedInFilter
import           Bead.View.Markdown
#ifndef LDAPEnabled
import           Bead.View.Registration
import           Bead.View.ResetPassword
#endif
import           Bead.View.RouteOf
import           Bead.View.RequestParams

import           Bead.Shared.Command
import           Bead.View.Command.All
import           Bead.View.Command.Fayax

#ifdef TEST
import           Test.Tasty.TestSet
#endif

{-
Routing of a given request based on the path of the request. A page handler
is selected of the path is known otherwise an error page is rendered.
-}

-- * Route table

routes :: Config -> [(ByteString, BeadHandler ())]
routes config = join
  [ -- Add login handlers
    [ ("/",         index)
    , ("/logout",   logoutAndResetRoute)
    , ("/change-language", changeLanguage)
    ]
  , registrationRoutes config
  , [ ("/fay", with fayContext fayServe)
    , fayaxRouteHandler ping
    , fayaxRouteHandler refresh
    , ("/upload", fileUpload)
    ]
  , [ (markdownPath, serveMarkdown) ]
    -- Add static handlers
  , [ ("",          serveDirectory "static") ]
  ]

registrationRoutes :: Config -> [(ByteString, BeadHandler ())]
#ifdef LDAPEnabled
registrationRoutes _ = []
#else
registrationRoutes config = [
    ("/reset_pwd",resetPasswordPage)
  , ("/reg_request", registrationRequest config)
  , ("/reg_final", finalizeRegistration)
  ]
#endif

pages :: BeadHandler ()
pages = do
  path <- getRequest >>= return . proc req -> do
            ctx <- rqContextPath -< req
            pth <- rqPathInfo    -< req
            returnA -< (BS.append ctx pth)
  page <- requestToPageHandler path
  case page of
    -- No Page value is calculated from the request, pass to other handler
    Nothing -> pass
    Just pd
      | P.isLogin pd -> loginSubmit
      | otherwise ->  handlePage $ renderResponse $ pageContent pd

-- * Handlers

index :: BeadHandler ()
index =
  ifTop $ requireUser auth
            (with auth $ login Nothing)
            (redirect (routeOf $ P.home ()))

-- Redirects to the parent page of the given page
redirectToParentPage :: P.PageDesc -> ContentHandler ()
redirectToParentPage = maybe (return ()) (redirect . routeOf) . P.parentPage

hfailure :: BeadHandler' a b -> BeadHandler' a (HandlerResult b)
hfailure h = h >> (return HFailure)

evalHandlerError
  :: (ContentError -> BeadHandler a)
  -> (c -> BeadHandler a)
  -> ContentHandler c
  -> BeadHandler a
evalHandlerError onError onSuccess h = do
  x <- runErrorT h
  case x of
    Left e  -> onError e
    Right s -> onSuccess s

-- Runs the handler and clears the status message, is any error occurs
-- the onError handler is run, in both cases returns information about
-- the successfullness.
runGETHandler
  :: (ContentError -> BeadHandler a)
  -> ContentHandler a
  -> BeadHandler (HandlerResult a)
runGETHandler onError handler
  = evalHandlerError
      (hfailure . onError)
      (return . HSuccess)
      (do x <- handler
          userStory S.clearStatusMessage
          lift . with sessionManager $ do
            touchSession
            commitSession
          return x)

-- Runs the 'h' handler if no error occurs during the run of the handler
-- calculates the parent page for the given 'p', and runs the attached userstory
-- from the calculated user action
-- and redirects at the end to the parent page, if the page is not
-- a temporary view page, otherwise runs the onError handler
-- in both ways returns information about the successfulness.
runPOSTHandler
  :: (ContentError -> BeadHandler ())
  -> P.PageDesc
  -> ContentHandler UserAction
  -> BeadHandler (HandlerResult ())
runPOSTHandler onError p h
  = evalHandlerError
      (hfailure . onError)
      (return . HSuccess)
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
  :: (ContentError -> BeadHandler a)
  -> ContentHandler a
  -> BeadHandler (HandlerResult a)
runUserViewPOSTHandler onError userViewHandler
  = evalHandlerError
      (hfailure . onError)
      (return . HSuccess)
      (do x <- userViewHandler
          lift . with sessionManager $ do
            touchSession
            commitSession
          return x)

logoutAndResetRoute :: BeadHandler ()
logoutAndResetRoute = do
  ContentHandler.logout
  redirect "/"

logoutAndErrorPage :: String -> BeadHandler ()
logoutAndErrorPage msg = do
  ContentHandler.logout
  msgErrorPage msg

-- Helper type synonyms
type CH   = ContentHandler ()
type CHUA = ContentHandler UserAction

type PageRenderer = P.Page CH CH (CH,CHUA) CHUA CH

renderResponse :: PageHandler -> PageRenderer
renderResponse = P.pfmap
  (viewHandlerCata (>>= renderBootstrapPage))
  (userViewHandlerCata (>>= renderBootstrapPage))
  (viewModifyHandlerCata (\get post -> (get >>= renderBootstrapPage, post)))
  (modifyHandlerCata Prelude.id)
  (dataHandlerCata Prelude.id)

{- When a user logs in the home page is shown for her. An universal handler
   is used. E.g "/home" -> handlePage P.Home.
   * If the user can navigate to the
   intented page from its state, it's state is going to change in his session
   and in the server side as well.
   * When a user submits information with a POST request, from the submitted information
   we calculate the appropiate user action and runs it
-}
handlePage :: PageRenderer -> BeadHandler ()
handlePage page = P.pageKindCata view userView viewModify modify data_ page where
  pageDesc = P.pageToPageDesc page

  loggedInFilter m = userIsLoggedInFilter
    m
    -- Not logged in user tries to get some data
    (logoutAndResetRoute' ("Routing.loggedInFilter1" :: String))
    -- Some internal error happened
    (logoutAndErrorPage' ("Routing.loggedInFilter2" :: String))

  invalidPOSTMethodCall = do
     logMessage DEBUG $ "Invalid POST handler " ++ show pageDesc
     (logoutAndResetRoute' "Rounting.invalidPOSTMethodCall")

  invalidGETMethodCall = do
     logMessage DEBUG $ "Invalid GET handler" ++ show pageDesc
     (logoutAndResetRoute' "Routing.invalidGETMethodCall")

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
    lift $ (logoutAndResetRoute' "Routing.notAllowedPage")

  logoutAndResetRoute' _name = do
    logoutAndResetRoute

  logoutAndErrorPage' _name msg = do
    logoutAndErrorPage msg


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

  view       = get . void . loggedInFilter . runGetOrError . P.viewPageValue
  data_      = get . void . loggedInFilter . runGetOrError . P.dataPageValue
  userView   = post . void . loggedInFilter . runUserViewPostOrError . P.userViewPageValue
  viewModify = void . uncurry (\get post -> getPost (loggedInFilter $ runGetOrError get)
                                                    (loggedInFilter $ runPostOrError post))
                    . P.viewModifyPageValue
  modify     = post . void . loggedInFilter . runPostOrError . P.modifyPageValue

allowedPageByTransition
  :: P.Page a b c d e -> ContentHandler a -> ContentHandler a -> ContentHandler a
allowedPageByTransition p allowed restricted = withUserState $ \state ->
  let allow = P.allowedPage (role state) p
  in case allow of
    False -> restricted
    True  -> allowed

-- Creates a handler, that tries to calculate a Page value
-- from the requested route and the parameters of the request uri
requestToPageHandler :: RoutePath -> BeadHandler (Maybe P.PageDesc)
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
#ifndef LDAPEnabled
  , (setUserPasswordPath     , j $ P.setUserPassword ())
#endif
  , (deleteUsersFromCoursePath , \ps -> P.deleteUsersFromCourse <$> courseKey ps <*> unit)
  , (deleteUsersFromGroupPath , \ps -> P.deleteUsersFromGroup <$> groupKey ps <*> unit)
  , (unsubscribeFromCoursePath , \ps -> P.unsubscribeFromCourse <$> groupKey ps <*> unit)
  , (getSubmissionPath, \ps -> P.getSubmission <$> submissionKey ps <*> unit)
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

void :: Monad m => m a -> m ()
void m = m >> return ()

-- * Fayax

fayaxRouteHandler :: FayaxCommand a -> (ByteString, BeadHandler ())
fayaxRouteHandler f = (fayaxCmdValue $ fayaxRoute f, fayaxCmdValue $ fayaxHandler f)

-- Abstract request handler, for the type instanciation
fayaxRequest :: (Command c) => c -> BeadHandler (Answer c)
fayaxRequest = fayaxLoginFilter fayaxInteract

-- Associates a fayax command with a handler
fayaxHandler
  :: FayaxCommand a
  -> FayaxCommand (BeadHandler ())
fayaxHandler = fayaxCmdConsts
  (fayax pingc)
  (fayax refreshc)
  where
    pingc :: Ping -> BeadHandler Pong
    pingc = fayaxRequest

    refreshc :: Refresh -> BeadHandler RefreshReply
    refreshc = fayaxRequest

-- * Tests

#ifdef TEST

requestToParams :: [ReqParam] -> Params
requestToParams = foldl insert Map.empty
  where
    insert m (ReqParam (name, value)) =
      Map.insert (fromString name) [(fromString value)] m

routingTest =
  assertProperty
    "requestToPage is totally defined"
    (\p -> let rp = P.pageValue $ pageRoutePath p
               ps = requestToParams . P.pageValue $ pageRequestParams p
           in requestToPage rp ps == Just p)
    P.pageGen
    "For each page must be requestToPage must be defined"


#endif
