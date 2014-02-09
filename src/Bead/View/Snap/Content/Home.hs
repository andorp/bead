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

import Numeric (showHex)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map (lookup)
import Data.List (intersperse, sortBy)
import Data.String (fromString)
import Data.Time
import Control.Monad (join, when, liftM)
import Control.Monad.Identity
import Control.Monad.Trans.Error

import Bead.Domain.Entities as E (Role(..))
import Bead.Domain.Evaluation
import Bead.Domain.Relationships (AssignmentDesc(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content hiding (userState)
import Bead.View.Snap.DataBridge as Param (Parameter(name))
import qualified Bead.View.UserActions as UA
import Bead.Controller.UserStories (
    userAssignments
  , submissionTables
  , administratedCourses
  , administratedGroups
  )

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A (class_, style, id, colspan)

import Bead.View.Snap.I18N (IHtml)
import qualified Text.Blaze.Html5 as H

#ifdef TEST
import Bead.Invariants
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
    -- Nothing means that the user is not registrated in any
    -- courses
  , assignments :: Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)]
  , sTables     :: [SubmissionTableInfo]
    -- The convertes function that convert a given utc time into the users local
    -- timezone
  , timeConverter :: UserTimeConverter
  }

homePage :: GETContentHandler
homePage = withUserState $ \s -> do
  converter <- usersTimeZoneConverter
  (renderPagelet . withUserFrame s . homeContent) =<<
    (userStory
       (HomePageData s
          <$> ((not . null) <$> administratedCourses)
          <*> ((not . null) <$> administratedGroups)
          <*> userAssignments
          <*> (map sortUserLines <$> submissionTables)
          <*> (return converter)))

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

navigation :: [P.Page] -> IHtml
navigation links = do
  msg <- getI18N
  return $ H.div ! A.id "menu" $ H.ul $ mapM_ (i18n msg . linkToPage) links

homeContent :: HomePageData -> IHtml
homeContent d = do
  let s = userState d
      r = role s
      hasCourse = hasCourses d
      hasGroup  = hasGroups d
  msg <- getI18N
  return $ H.div # textAlign "left" $ do
    when (isAdmin s) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_AdminTasks "Rendszergazdai feladatok"
      i18n msg $ navigation [P.Administration]
      H.hr
    when (courseAdminUser r) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_CourseAdminTasks "Tárgyfelelősi feladatok"
      when (not hasCourse) $ do
        H.p $ fromString . msg $ Msg_Home_NoCoursesYet
          "Még nincsenek tárgyak.  Meg kell kérni az adminisztrátort, hogy rendeljen hozzánk tárgyakat!"
    when (groupAdminUser r) $ H.p $ do
      H.h3 . fromString . msg $ Msg_Home_GroupAdminTasks "Oktatói feladatok"
      when (not hasGroup) $ (fromString $ msg $ Msg_Home_NoGroupsYet "Még nincsenek csoportok.")
    when ((courseAdminUser r) || (groupAdminUser r)) $ do
      when (hasCourse || hasGroup) $ H.p $ do
        i18n msg $ htmlSubmissionTables (sTables d)
    when (courseAdminUser r && hasCourse) $ do
      H.p $ fromString . msg $ Msg_Home_CourseAdministration_Info $ concat
        [ "A tárgyhoz új csoportokat a Tárgyi beállítások almenüben lehet létrehozni.  Ugyanitt lehet "
        , "egyúttal az egyes csoportokhoz oktatókat rendelni."
        ]
      H.p $ do
        i18n msg $ navigation $ [ P.CourseAdmin, NewCourseAssignment] ++
                                (if hasGroup then [P.NewGroupAssignment] else []) ++
                                [ P.EvaluationTable, P.SetUserPassword ]
      H.hr
    when (groupAdminUser r && hasGroup) $ H.p $ do
      i18n msg $ navigation [P.NewGroupAssignment, P.EvaluationTable, P.SetUserPassword]
      H.hr
    H.h3 . fromString . msg $ Msg_Home_StudentTasks "Hallgatói feladatok"
    H.p $ do
      i18n msg $ availableAssignments (timeConverter d) (assignments d)
      i18n msg $ navigation [P.GroupRegistration]
    where
      courseAdminUser = (==E.CourseAdmin)
      groupAdminUser  = (==E.GroupAdmin)

availableAssignments :: UserTimeConverter -> Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)] -> IHtml
availableAssignments _ Nothing = do
  msg <- getI18N
  return $ fromString $ msg $ Msg_Home_HasNoRegisteredCourses "Még nincsenek felvett tárgyaid, vegyél fel tárgyakat!"
availableAssignments _ (Just []) = do
  msg <- getI18N
  return $ fromString $ msg $ Msg_Home_HasNoAssignments "Még nincsenek kiírva feladatok."
availableAssignments timeconverter (Just as) = do
  msg <- getI18N
  return $ table (fieldName availableAssignmentsTable) (className assignmentTable) # informationalTable $ do
    headerLine msg
    mapM_ (assignmentLine msg) as
  where
    dataCell = H.td # informationalCell
    dataCell' r = H.td # (informationalCell <> r)
    headerCell t = H.th # (informationalCell <> grayBackground) $ t
    headerLine msg = H.tr $ do
      headerCell ""
      headerCell (fromString $ msg $ Msg_Home_Course "Tárgy")
      headerCell (fromString $ msg $ Msg_Home_CourseAdmin "Oktató")
      headerCell (fromString $ msg $ Msg_Home_Assignment "Feladat")
      headerCell (fromString $ msg $ Msg_Home_Deadline "Határidő")
      headerCell (fromString $ msg $ Msg_Home_Evaluation "Értékelés")
    assignmentLine msg (k,a,s) = H.tr $ do
      case aActive a of
        True -> dataCell $ link (routeWithParams P.Submission [requestParam k]) (msg $ Msg_Home_NewSolution "Új megoldás")
        False -> dataCell (fromString . msg $ Msg_Home_ClosedSubmission "Lezárva")
      dataCell (fromString . aGroup $ a)
      dataCell (fromString . join . intersperse ", " . aTeachers $ a)
      dataCell $ linkWithText (routeWithParams P.SubmissionList [requestParam k]) (fromString (aTitle a))
      dataCell (fromString . showDate . timeconverter $ aEndDate a)
      (coloredSubmissionCell dataCell' (H.td) fromString
        (msg $ Msg_Home_SubmissionCell_NoSubmission "Nincs megoldás")
        (msg $ Msg_Home_SubmissionCell_NonEvaluated "Nem értékelt")
        (msg $ Msg_Home_SubmissionCell_Accepted "Elfogadott")
        (msg $ Msg_Home_SubmissionCell_Rejected "Elutasított")
        s)

htmlSubmissionTables :: [SubmissionTableInfo] -> IHtml
htmlSubmissionTables xs = do
  tables <- mapM htmlSubmissionTable . zip [1..] $ xs
  return $ sequence_ tables

-- Produces the HTML table from the submission table information,
-- if there is no users registered and submission posted to the
-- group or course students, an informational text is shown.
htmlSubmissionTable :: (Int,SubmissionTableInfo) -> IHtml

-- Empty table
htmlSubmissionTable (i,s)
  | and [null . stAssignments $ s, null . stUsers $ s] = do
    msg <- getI18N
    return $ H.p $ do
      (fromString $ msg $ Msg_Home_SubmissionTable_NoCoursesOrStudents "Nincsenek feladatok vagy hallgatók a csoporthoz:")
      H.br
      fromString . stCourse $ s
      H.br

-- Non empty table
htmlSubmissionTable (i,s) = do
  msg <- getI18N
  return $ courseForm $ table tableId (className groupSubmissionTable) # informationalTable $ do
    headLine (stCourse s)
    assignmentLine msg (stAssignments s)
    mapM_ (userLine msg) (stUserLines s)
  where
    courseForm = infoSourceCata createForm createForm id infoSrc where
      createForm = either
        (postForm . routeOf . P.DeleteUsersFromGroup)
        (postForm . routeOf . P.DeleteUsersFromCourse)
        key

    tableId = join ["st", show i]
    headLine = H.tr . (H.th # textAlign "left" ! A.colspan "4") . fromString
    headerCell = H.th # (informationalCell <> grayBackground)
    dataCell r = H.td # (informationalCell <> r)
    assignmentLine msg as = H.tr $ do
      headerCell $ fromString $ msg $ Msg_Home_SubmissionTable_StudentName "Név"
      headerCell $ fromString $ msg $ Msg_Home_SubmissionTable_Username "NEPTUN"
      mapM_ (headerCell . modifyAssignmentLink) . zip [1..] $ as
      headerCell $ fromString $ msg $ Msg_Home_SubmissionTable_Summary "Összesítés"
      deleteHeaderCell msg

    modifyAssignmentLink (i,ak) =
      linkWithTitle
        (routeWithParams P.ModifyAssignment [requestParam ak])
        (maybe "" id . Map.lookup ak $ stAssignmentNames s)
        (show i)

    userLine msg (u, p, as) = H.tr $ do
      let username = ud_username u
          submissionInfos = map snd as
      dataCell noStyle . fromString . ud_fullname $ u
      dataCell noStyle . fromString . show $ username
      mapM_ (submissionCell username) $ as
      case calculateSubmissionResult msg submissionInfos (stEvalConfig s) of
        Left  e      -> dataCell summaryErrorStyle  $ fromString e
        Right Passed -> dataCell summaryPassedStyle $ fromString $ msg $ Msg_Home_SubmissionTable_Accepted "Elfogadott"
        Right Failed -> dataCell summaryFailedStyle $ fromString $ msg $ Msg_Home_SubmissionTable_Rejected "Elutasított"
      deleteUserCheckbox u

    submissionCell u (ak,s) =
      coloredSubmissionCell
        dataCell
        (H.td)
        (linkWithText (routeWithParams P.UserSubmissions [requestParam u, requestParam ak]))
        " "
        "."
        "1"
        "0"
        s

    deleteHeaderCell msg =
      either
        (infoSourceCata -- GroupKey
          (const emptyHtml)
          deleteForGroupButton
          (const emptyHtml)
          infoSrc)
        (infoSourceCata -- CourseKey
           deleteForCourseButton
           (const emptyHtml)
           (const emptyHtml)
           infoSrc)
        key
      where
        deleteForCourseButton ck =
          headerCell $ submitButton
            (fieldName delUsersFromCourseBtn)
            (msg $ Msg_Home_DeleteUsersFromCourse "Törlés")

        deleteForGroupButton gk =
          headerCell $ submitButton
            (fieldName delUsersFromGroupBtn)
            (msg $ Msg_Home_DeleteUsersFromGroup "Törlés")

    deleteUserCheckbox u =
      infoSourceCata
        deleteCourseCheckbox
        deleteGroupCheckbox
        emptyHtml
        infoSrc
      where
        deleteCourseCheckbox =
          dataCell noStyle $ checkBox
            (Param.name delUserFromCoursePrm)
            (encode delUserFromCoursePrm $ ud_username u)
            False

        deleteGroupCheckbox =
          dataCell noStyle $ checkBox
            (Param.name delUserFromGroupPrm)
            (encode delUserFromGroupPrm $ ud_username u)
            False

    emptyHtml = return ()

    infoSrc = stOrigin s

    key = stKey s

-- Create a table cell for the evaulation value, where
-- simpleCell is the combinator for the non RGB colored cells
-- rgbCell is a cell combinator where the rgb value will be set
-- content how the computed text value is wrapped
-- notFound text for the non evaulated submission
-- unevaluated text for the unevaluated submission
-- passed message for the passed binary evaulation
-- failed message for the failed binary evaulation
-- s the submission information itself
coloredSubmissionCell simpleCell rgbCell content notFound unevaluated passed failed s =
  coloredCell $ content (sc s)
  where
    sc Submission_Not_Found   = notFound
    sc Submission_Unevaluated = unevaluated
    sc (Submission_Result _ r) = val r

    val (BinEval (Binary Passed)) = passed
    val (BinEval (Binary Failed)) = failed
    val (PctEval (Percentage (Scores [p]))) = percent p

    coloredCell = color s

    color =
      submissionInfoCata
        (simpleCell noStyle)        -- Not Found
        (simpleCell unevaluatedStyle) -- Unevulated
        (const resultCell)        -- Result

    resultCell (BinEval (Binary Passed)) = simpleCell binaryPassedStyle
    resultCell (BinEval (Binary Failed)) = simpleCell binaryFailedStyle
    resultCell p@(PctEval {}) = withRGBClass (EvResult p) rgbCell

    percent x = join [show . round $ (100 * x), "%"]

    withRGBClass r = maybe id (\pct html -> html ! (A.style . fromString . colorStyle . pctCellColor $ pct)) (percentValue r)

-- * Evaluation

-- Produces the result of the submissions. The selected evaluation method depends
-- on the given configuration.
calculateSubmissionResult :: I18N -> [SubmissionInfo] -> EvaluationConfig -> Either String Result
calculateSubmissionResult msg si e =
  case results of
    [] -> (Left (msg $ Msg_Home_HasNoSummary "Nem összesíthető"))
    rs -> evaluationDataMap
            (const (sumBinaryResult msg rs))
            (flip (sumPercentageResult msg) rs)
            e
  where
    results = filter evaluated si

    evaluated = submissionInfoCata
                  False -- not found
                  False -- unevaulated
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
    binary = evaluationDataMap Right (const . Left $ (msg $ Msg_Home_NonBinaryEvaluation "Nem kétértékű értékelés"))

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
                   (const . Left $ (msg $ Msg_Home_NonPercentageEvaluation "Nem százalékos értékelés"))
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
    filterEvaluation = catMaybes . map (submissionInfoCata Nothing Nothing result)

    -- Checks if no error is found.
    -- Produces (Left "error") when at least one element has an error,
    -- otherwise the list
    checkErrors :: [Either String a] -> Either String [a]
    checkErrors [] = Right []
    checkErrors ((Left msg):_) = Left msg
    checkErrors ((Right b):bs) = fmap (b:) (checkErrors bs)

-- * CSS Section

binaryPassedStyle = backgroundColor "lightgreen"
binaryFailedStyle = backgroundColor "red"
unevaluatedStyle  = backgroundColor "gray"
summaryPassedStyle = backgroundColor "lightgreen"
summaryFailedStyle = backgroundColor "red"
summaryErrorStyle  = backgroundColor "yellow"

-- * Colors

newtype RGB = RGB (Int, Int, Int)

pctCellColor :: Double -> RGB
pctCellColor x = RGB (round ((1 - x) * 255), round (x * 255), 0)

colorStyle :: RGB -> String
colorStyle (RGB (r,g,b)) = join ["background-color:#", hex r, hex g, hex b]
  where
    twoDigits [d] = ['0',d]
    twoDigits ds  = ds

    hex x = twoDigits (showHex x "")

-- * Tools

-- Sorts the userlines alphabetically ordered in submissionTableInfo
sortUserLines = submissionTableInfoCata
  id -- course
  id -- information source
  id -- number
  id -- config
  id -- assignment
  id -- assignments
  id -- user
  id -- users
  id -- userline
  sort -- userlines
  id -- assignment names
  id -- key
  SubmissionTableInfo
  where
   sort = sortBy (compareHun `on` fst3)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

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
