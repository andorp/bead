{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Snap.Content.Home (
    home
  , deleteUsersFromCourse
  , deleteUsersFromGroup
#ifdef TEST
  , sumBinaryResultTests
  , sumPercentageResultTests
  , calculateSubmissionResultTests
#endif
  ) where

import           Control.Monad.Identity
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String (fromString)
import           Data.Time

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (style, id)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities as E (Role(..))
import           Bead.Domain.Evaluation
import           Bead.View.Snap.Content hiding (userState)
import qualified Bead.View.UserActions as UA
import           Bead.View.Snap.Content.SubmissionTable as ST

#ifdef TEST
import           Bead.Invariants
#endif

home :: Content
home = getContentHandler homePage

deleteUsersFromCourse :: Content
deleteUsersFromCourse = postContentHandler deleteUsersFromCourseHandler

deleteUsersFromGroup :: Content
deleteUsersFromGroup = postContentHandler deleteUsersFromGroupHandler

data HomePageData = HomePageData {
    userState   :: UserState
  , hasCourses  :: Bool -- True if the user has administrated courses
  , hasGroups   :: Bool -- True if the user has administrated groups
  , assignments :: Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)] -- Nothing means that the user is not registrated in any courses
  , sTables     :: [SubmissionTableInfo]
    -- ^ The convertes function that convert a given utc time into the users local timezone
  , timeConverter :: UserTimeConverter
  , submissionTableCtx :: SubmissionTableContext
  }

administratedCourseMap = stcAdminCourses . submissionTableCtx
administratedGroupMap  = stcAdminGroups  . submissionTableCtx
courseTestScripts      = stcCourseTestScriptInfos . submissionTableCtx

homePage :: GETContentHandler
homePage = withUserState $ \s -> do
  converter <- usersTimeZoneConverter
  now <- liftIO getCurrentTime
  (renderPagelet . withUserFrame s . homeContent now) =<< do
    (userStory $ do
       ua <- S.userAssignments
       sbmTables <- (map sortUserLines <$> S.submissionTables)
       stc <- ST.submissionTableContext
       return $
         HomePageData
           s
           (not . Map.null $ stcAdminCourses stc)
           (not . Map.null $ stcAdminGroups stc)
           ua
           sbmTables
           converter
           stc)

deleteUsersFromCourseHandler :: POSTContentHandler
deleteUsersFromCourseHandler =
  UA.DeleteUsersFromCourse
    <$> (getParameter delUserFromCourseKeyPrm)
    <*> (getParameterValues delUserFromCoursePrm)

deleteUsersFromGroupHandler :: POSTContentHandler
deleteUsersFromGroupHandler =
  UA.DeleteUsersFromGroup
    <$> (getParameter delUserFromGroupKeyPrm)
    <*> (getParameterValues delUserFromGroupPrm)

navigation :: [Pages.Page a] -> IHtml
navigation links = do
  msg <- getI18N
  return $ H.div ! A.id "menu" $ H.ul $ mapM_ (i18n msg . linkToPage) links

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _c _us as _uls _ans _ck = as
  group _n _c _us cgas _uls _ans _ck _gk = map (cgInfoCata id id) cgas

homeContent :: UTCTime -> HomePageData -> IHtml
homeContent now d = do
  let s = userState d
      r = role s
      hasCourse = hasCourses d
      hasGroup  = hasGroups d
      testScripts = courseTestScripts d
  msg <- getI18N
  return $ H.div # textAlign "left" $ do
    when (isAdmin s) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_AdminTasks "Administrator Menu"
      i18n msg $ navigation [administration]
      H.hr
    when (courseAdminUser r) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_CourseAdminTasks "Course Administrator Menu"
      when (not hasCourse) $ do
        H.p $ fromString . msg $ Msg_Home_NoCoursesYet
          "There are no courses.  Contact the administrator to have courses assigned."
    when (groupAdminUser r) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_GroupAdminTasks "Teacher Menu"
      when (not hasGroup) $ (fromString $ msg $ Msg_Home_NoGroupsYet "There are no groups.")
    when ((courseAdminUser r) || (groupAdminUser r)) $ do
      when hasGroup $ H.p $ do
        when (not . null $ concatMap submissionTableInfoAssignments $ sTables d) $ do
          H.p $ fromString . msg $ Msg_Home_SubmissionTable_Info $ concat
            [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
            , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
            , "then clicking on the button."
            ]
        i18n msg $ htmlSubmissionTables d now
      when hasCourse $ H.p $ do
        H.h3 $ fromString . msg $ Msg_Home_CourseAdministration "Course Administration"
        H.p $ fromString . msg $ Msg_Home_CourseSubmissionTableList_Info $ concat
          [ "Submission table for courses can be found on separate pages, please click on the "
          , "name of a course."
          ]
        H.ul ! A.style "list-style-type: none" $ do
          let courseList = sortBy (compareHun `on` (courseName . snd)) $ Map.toList $ administratedCourseMap d
          forM_ courseList $ \(ck, c) ->
            H.li $ linkWithText (routeOf (courseOverview ck)) (courseName c)
    when (courseAdminUser r && hasCourse) $ H.p $ do
      H.p $ fromString . msg $ Msg_Home_CourseAdministration_Info $ concat
        [ "New groups for courses may be created in the Course Settings menu.  Teachers may be also assigned to "
        , "each of the groups there as well."
        ]
      H.p $ do
        i18n msg $ navigation $ [
            courseAdmin, newTestScript, evaluationTable
          , setUserPassword, uploadFile ]
      H.hr
    when (groupAdminUser r && hasGroup) $ H.p $ do
      i18n msg $ navigation [evaluationTable, setUserPassword, uploadFile ]
      H.hr
    H.h3 . fromString . msg $ Msg_Home_StudentTasks "Student Menu"
    H.p $ do
      i18n msg $ availableAssignments (timeConverter d) (assignments d)
      i18n msg $ navigation [groupRegistration]
    where
      administration    = Pages.administration ()
      courseAdmin       = Pages.courseAdmin ()
      courseOverview ck = Pages.courseOverview ck ()
      evaluationTable   = Pages.evaluationTable ()
      groupRegistration = Pages.groupRegistration ()
      newTestScript     = Pages.newTestScript ()
      setUserPassword   = Pages.setUserPassword ()
      submission     = Pages.submission ()
      submissionList = Pages.submissionList ()
      uploadFile     = Pages.uploadFile ()

      courseAdminUser = (==E.CourseAdmin)
      groupAdminUser  = (==E.GroupAdmin)


availableAssignments :: UserTimeConverter -> Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)] -> IHtml
availableAssignments _ Nothing = do
  msg <- getI18N
  return $ fromString $ msg $ Msg_Home_HasNoRegisteredCourses "There are no registered courses, register to some."
availableAssignments _ (Just []) = do
  msg <- getI18N
  return $ fromString $ msg $ Msg_Home_HasNoAssignments "There are no available assignments yet."
availableAssignments timeconverter (Just as) = do
  msg <- getI18N
  return $ do
    H.p $ fromString . msg $ Msg_Home_Assignments_Info $ concat
      [ "Submissions and their evaluations may be accessed by clicking on each assignment's link. "
      , "The table shows only the last evaluation per assignment."
      ]
    table (fieldName availableAssignmentsTable) (className assignmentTable) # informationalTable $ do
    headerLine msg
    mapM_ (assignmentLine msg) as
  where
    dataCell = H.td # informationalCell
    dataCell' r = H.td # (informationalCell <> r)
    headerCell t = H.th # (informationalCell <> grayBackground) $ t
    headerLine msg = H.tr $ do
      headerCell ""
      headerCell (fromString $ msg $ Msg_Home_Course "Course")
      headerCell (fromString $ msg $ Msg_Home_CourseAdmin "Teacher")
      headerCell (fromString $ msg $ Msg_Home_Assignment "Assignment")
      headerCell (fromString $ msg $ Msg_Home_Deadline "Deadline")
      headerCell (fromString $ msg $ Msg_Home_Evaluation "Evaluation")
    assignmentLine msg (k,a,s) = H.tr $ do
      case aActive a of
        True -> dataCell $ link (routeWithParams (Pages.submission ()) [requestParam k]) (msg $ Msg_Home_NewSolution "New submission")
        False -> dataCell (fromString . msg $ Msg_Home_ClosedSubmission "Closed")
      dataCell (fromString . aGroup $ a)
      dataCell (fromString . join . intersperse ", " . aTeachers $ a)
      dataCell $ linkWithText (routeWithParams (Pages.submissionList ()) [requestParam k]) (fromString (aTitle a))
      dataCell (fromString . showDate . timeconverter $ aEndDate a)
      (coloredSubmissionCell dataCell' (H.td) fromString
        (msg $ Msg_Home_SubmissionCell_NoSubmission "No submission")
        (msg $ Msg_Home_SubmissionCell_NonEvaluated "Non-evaluated")
        (msg $ Msg_Home_SubmissionCell_Tested "Tested")
        (msg $ Msg_Home_SubmissionCell_Accepted "Accepted")
        (msg $ Msg_Home_SubmissionCell_Rejected "Rejected")
        s)

htmlSubmissionTables :: HomePageData -> UTCTime -> IHtml
htmlSubmissionTables pd now = do
  tables <- mapM (htmlSubmissionTable pd now) $ zip [1..] (sTables pd)
  return $ sequence_ tables
  where
    htmlSubmissionTable pd now (i,s) = do
      submissionTable (concat ["st", show i]) now (submissionTableCtx pd) s

-- * Evaluation

-- Produces the result of the submissions. The selected evaluation method depends
-- on the given configuration.
calculateSubmissionResult :: I18N -> [SubmissionInfo] -> EvaluationConfig -> Either String Result
calculateSubmissionResult msg si e =
  case results of
    [] -> (Left (msg $ Msg_Home_HasNoSummary "N/A"))
    rs -> evaluationDataMap
            (const (sumBinaryResult msg rs))
            (flip (sumPercentageResult msg) rs)
            e
  where
    results = filter evaluated si

    evaluated = submissionInfoCata
                  False -- not found
                  False -- unevaulated
                  False -- tested
                  (\_ _ -> True) -- result

-- Produces the result of a user's submission list for a binary evaluation.
-- Returns (Right result) when there is no error in the submission set, otherwise (Left "Reason")
sumBinaryResult :: I18N -> [SubmissionInfo] -> Either String Result
sumBinaryResult msg = calcEvaluationResult binary calcBinaryResult
  where
    -- Checks if the result is a binary result
    -- Produces (Left "error") if the result is not a binary result
    -- otherwise (Right result)
    binary :: EvaluationResult -> Either String Binary
    binary = evaluationDataMap Right (const . Left $ (msg $ Msg_Home_NonBinaryEvaluation "Not a binary evaluation"))

    calcBinaryResult :: [Binary] -> Result
    calcBinaryResult bs = calculateEvaluation bs ()

-- Produces the result of a user's submission list for a percentage evaluation using
-- the given config.
-- Returns (Right result) when there is no error in the submission set, otherwise (Left "Reason")
sumPercentageResult :: I18N -> PctConfig -> [SubmissionInfo] -> Either String Result
sumPercentageResult msg config = calcEvaluationResult percentage calcPercentageResult
  where
    percentage :: EvaluationResult -> Either String Percentage
    percentage = evaluationDataMap
                   (const . Left $ (msg $ Msg_Home_NonPercentageEvaluation "Not a percentage evaluation"))
                   Right

    calcPercentageResult :: [Percentage] -> Result
    calcPercentageResult ps = calculateEvaluation ps config

-- Produces the result of a user's submission list using the selectResult
-- projection and the calculateResult function
-- Returns (Right result) if the calculation is correct, otherwise (Left "reason")
calcEvaluationResult
  :: (EvaluationResult -> Either String result) -- Selects the correct result or produces an error msg
  -> ([result] -> Result) -- Aggregates the results calculating into the final result
  -> [SubmissionInfo]
  -> Either String Result
calcEvaluationResult selectResult calculateResult
  = right calculateResult . checkErrors . map selectResult . filterEvaluation
  where
    result = const Just

    right :: (a -> b) -> Either c a -> Either c b
    right f (Right x) = Right (f x)
    right _ (Left x)  = (Left x)

    -- Filters only the evaluation results
    filterEvaluation :: [SubmissionInfo] -> [EvaluationResult]
    filterEvaluation = catMaybes . map (submissionInfoCata Nothing Nothing Nothing result)

    -- Checks if no error is found.
    -- Produces (Left "error") when at least one element has an error,
    -- otherwise the list
    checkErrors :: [Either String a] -> Either String [a]
    checkErrors [] = Right []
    checkErrors ((Left msg):_) = Left msg
    checkErrors ((Right b):bs) = fmap (b:) (checkErrors bs)

-- * Tests

#ifdef TEST
binPassed = Submission_Result undefined (BinEval (Binary Passed))
binFailed = Submission_Result undefined (BinEval (Binary Failed))
pctResult = Submission_Result undefined (PctEval (Percentage (Scores [0.1])))

sumBinaryResultTests = [
    Assertion "Empty list" (sumBinaryResult trans []) (Right Failed)
  , Assertion "Homogenous passed list" (sumBinaryResult trans [binPassed, binPassed]) (Right Passed)
  , Assertion "Homogenous failed list" (sumBinaryResult trans [binPassed, binFailed]) (Right Failed)
  , Assertion "Inhomogenous list" (sumBinaryResult trans [binPassed, binFailed, pctResult, binPassed])
              (Left "Not a binary evaluation")
  ]

cfg30 = PctConfig 0.3 -- At least 30% is needed to pass
cfg40 = PctConfig 0.4 -- At least 40% is needed to pass
pct x = Submission_Result undefined (PctEval (Percentage (Scores [x])))

sumPercentageResultTests = [
    Assertion "Empty list"     (sumPercentageResult trans cfg30 []) (Right Failed)
  , Assertion "30% and passed" (sumPercentageResult trans cfg30 [pct 0.3]) (Right Passed)
  , Assertion "40% and failed" (sumPercentageResult trans cfg40 [pct 0.3]) (Right Failed)
  , Assertion "60/200 and passed" (sumPercentageResult trans cfg30 [pct 0.1, pct 0.5]) (Right Passed)
  , Assertion "50/200 and failed" (sumPercentageResult trans cfg30 [pct 0, pct 0.5]) (Right Failed)
  , Assertion "Inhomogenous list" (sumPercentageResult trans cfg30 [pct 0, binPassed])
                                  (Left "Not a percentage evaluation")
  ]

binConfig = BinEval ()
pctConfig = PctEval cfg30

calculateSubmissionResultTests = [
    Assertion "Binary config, failed"
              (calculateSubmissionResult trans [binPassed, binFailed] binConfig) (Right Failed)
  , Assertion "Percentage config, failed"
              (calculateSubmissionResult trans [pct 0.3, pct 0.1] pctConfig) (Right Failed)
  , Assertion "Binary config, wrong list"
              (calculateSubmissionResult trans [binPassed, binFailed] pctConfig)
              (Left "Not a percentage evaluation")
  , Assertion "Percentage config, wrong list"
              (calculateSubmissionResult trans [pct 0.3, pct 0.1] binConfig)
              (Left "Not a binary evaluation")
  ]
#endif
