{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Snap.Content.Home (
    home
#ifdef TEST
  , sumBinaryResultTests
  , sumPercentageResultTests
  , calculateSubmissionResultTests
#endif
  ) where

import Numeric (showHex)
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Data.String (fromString)
import Control.Monad (join, when, liftM)
import Control.Monad.Identity
import Control.Monad.Trans.Error

import Bead.Domain.Evaulation
import Bead.Domain.Relationships (AssignmentDesc(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.Controller.UserStories (
    userAssignments
  , submissionTables
  , administratedCourses
  , administratedGroups
  )

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (class_, style)

#ifdef TEST
import Bead.Invariants
#endif

home :: Content
home = getContentHandler homePage

data HomePageData = HomePageData {
    userState   :: UserState
  , hasCourses  :: Bool -- True if the user has administrated courses
  , hasGroups   :: Bool -- True if the user has administrated groups
  , assignments :: [(AssignmentKey, AssignmentDesc)]
  , sTables     :: [SubmissionTableInfo]
  }

homePage :: GETContentHandler
homePage = withUserState $ \s ->
  (renderPagelet . withUserFrame s . homeContent) =<<
    (runStoryE
       (HomePageData s
          <$> ((not . null) <$> administratedCourses)
          <*> ((not . null) <$> administratedGroups)
          <*> userAssignments
          <*> submissionTables))

homeContent :: HomePageData -> Pagelet
homeContent d = onlyHtml $ mkI18NHtml $ \i18n -> do
  let s = userState d
  when (isAdmin s) $ H.p $ do
    H.h3 $ (translate i18n "Admin's menu")
  when (hasCourses d) $ H.p $ do
    H.h3 $ (translate i18n "Course Admin's menu")
    linkToPage P.NewCourseAssignment
  when (hasGroups d) $ H.p $ do
    H.h3 $ (translate i18n "Teacher's menu")
    linkToPage P.NewGroupAssignment
  when (or [hasCourses d, hasGroups d]) $ H.p $ do
    H.h3 $ (translate i18n "Submission table")
    htmlSubmissionTables i18n (sTables d)
  when (isStudent s) $ H.p $ do
    H.h3 $ (translate i18n "Student's menu")
    availableAssignments i18n (assignments d)

availableAssignments :: I18N -> [(AssignmentKey,AssignmentDesc)] -> Html
availableAssignments i18n as = do
  table' (fieldName availableAssignmentsTable) # informationalTable $ do
    mapM_ assignmentLine as
  where
    dataCell = H.td # informationalCell
    assignmentLine (k,a) = H.tr $ do
      case aActive a of
        True -> dataCell $ link (routeWithParams P.Submission [requestParam k]) (i18n "New submission")
        False -> dataCell "Inactive"
      dataCell (fromString . aGroup $ a)
      dataCell (fromString . join . intersperse ", " . aTeachers $ a)
      dataCell $ link (routeWithParams P.SubmissionList [requestParam k]) (fromString (aTitle a))
      dataCell (fromString . show . aOk  $ a)
      dataCell (fromString . show . aNew $ a)
      dataCell (fromString . show . aBad $ a)

htmlSubmissionTables :: I18N -> [SubmissionTableInfo] -> Html
htmlSubmissionTables i18n xs = mapM_ (htmlSubmissionTable i18n) . zip [1..] $ xs

-- Produces the HTML table from the submission table information,
-- if there is no users registered and submission posted to the
-- group or course students, an informational text is shown.
htmlSubmissionTable :: I18N -> (Int,SubmissionTableInfo) -> Html

-- Empty table
htmlSubmissionTable i18n (i,s)
  | and [null . stAssignments $ s, null . stUsers $ s] = H.p $ do
      translate i18n "Assignments and users are not registered for the group:"
      H.br
      fromString . stCourse $ s
      H.br

-- Non empty table
htmlSubmissionTable i18n (i,s) = table' tableId # informationalTable $ do
  headLine (stCourse s)
  assignmentLine (stAssignments s)
  mapM_ userLine (stUserLines s)
  where
    tableId = join ["st", show i]
    headLine = H.tr . H.th . fromString
    headerCell = H.th # (informationalCell <> grayBackground)
    dataCell r = H.td # (informationalCell <> r)
    assignmentLine as = H.tr $ do
      headerCell $ (translate i18n "Name")
      headerCell $ (translate i18n "Username")
      mapM_ (headerCell . modifyAssignmentLink) . zip [1..] $ as
      headerCell $ (translate i18n "Passed")

    modifyAssignmentLink (i,ak) =
      link (routeWithParams P.ModifyAssignment [requestParam ak])
           (show i)

    userLine (u, p, as) = H.tr $ do
      let username = ud_username u
          submissionInfos = map snd as
      dataCell noStyle . fromString . ud_fullname $ u
      dataCell noStyle . fromString . show $ username
      mapM_ (submissionCell username) $ as
      case calculateSubmissionResult submissionInfos (stEvalConfig s) of
        Left  e      -> dataCell summaryErrorStyle  $ fromString e
        Right Passed -> dataCell summaryPassedStyle $ fromString (i18n "Passed")
        Right Failed -> dataCell summaryFailedStyle $ fromString (i18n "Failed")

    submissionCell u (ak,s) =
      coloredCell $ link (routeWithParams P.UserSubmissions [requestParam u, requestParam ak]) (sc s)
      where
        sc Submission_Not_Found   = " "
        sc Submission_Unevaulated = "."
        sc (Submission_Result _ r) = val r

        val (BinEval (Binary Passed)) = "1"
        val (BinEval (Binary Failed)) = "0"
        val (PctEval (Percentage (Scores [p]))) = percent p

        coloredCell = color s

        color =
          submissionInfoMap
            (dataCell noStyle)        -- Not Found
            (dataCell unevaulatedStyle) -- Unevulated
            (const resultCell)        -- Result

        resultCell (BinEval (Binary Passed)) = dataCell binaryPassedStyle
        resultCell (BinEval (Binary Failed)) = dataCell binaryFailedStyle
        resultCell p@(PctEval {}) = withRGBClass (EvResult p) H.td

        percent x = join [show . round $ (100 * x), "%"]

        withRGBClass r = maybe id (\pct html -> html ! (A.style . fromString . colorStyle . pctCellColor $ pct)) (percentValue r)

-- * Evaluation

-- Produces the result of the submissions. The selected evaulation method depends
-- on the given configuration.
calculateSubmissionResult :: [SubmissionInfo] -> EvaulationConfig -> Either String Result
calculateSubmissionResult si =
  evaluationDataMap
    (const (sumBinaryResult si))
    (flip sumPercentageResult si)

-- Produces the result of a user's submission list for a binary evaulation.
-- Returns (Right result) when there is no error in the submission set, otherwise (Left "Reason")
sumBinaryResult :: [SubmissionInfo] -> Either String Result
sumBinaryResult = calcEvaluationResult binary calcBinaryResult
  where
    -- Checks if the result is a binary result
    -- Produces (Left "error") if the result is not a binary result
    -- otherwise (Right result)
    binary :: EvaulationResult -> Either String Binary
    binary = evaluationDataMap Right (const . Left $ "Not a binary evaluation")

    calcBinaryResult :: [Binary] -> Result
    calcBinaryResult bs = calculateEvaulation bs ()

-- Produces the result of a user's submission list for a percentage evaulation using
-- the given config.
-- Returns (Right result) when there is no error in the submission set, otherwise (Left "Reason")
sumPercentageResult :: PctConfig -> [SubmissionInfo] -> Either String Result
sumPercentageResult config = calcEvaluationResult percentage calcPercentageResult
  where
    percentage :: EvaulationResult -> Either String Percentage
    percentage = evaluationDataMap
                   (const . Left $ "Not a percentage evaluation")
                   Right

    calcPercentageResult :: [Percentage] -> Result
    calcPercentageResult ps = calculateEvaulation ps config

-- Produces the result of a user's submission list using the selectResult
-- projection and the calculateResult function
-- Returns (Right result) if the calculation is correct, otherwise (Left "reason")
calcEvaluationResult
  :: (EvaulationResult -> Either String result) -- Selects the correct result or produces an error msg
  -> ([result] -> Result) -- Aggregates the results calculating into the final result
  -> [SubmissionInfo]
  -> Either String Result
calcEvaluationResult selectResult calculateResult
  = right calculateResult . checkErrors . map selectResult . filterEvaulation
  where
    result = const Just

    right :: (a -> b) -> Either c a -> Either c b
    right f (Right x) = Right (f x)
    right _ (Left x)  = (Left x)

    -- Filters only the evaulation results
    filterEvaulation :: [SubmissionInfo] -> [EvaulationResult]
    filterEvaulation = catMaybes . map (submissionInfoMap Nothing Nothing result)

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
unevaulatedStyle  = backgroundColor "gray"
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

-- * Tests

#ifdef TEST
binPassed = Submission_Result undefined (BinEval (Binary Passed))
binFailed = Submission_Result undefined (BinEval (Binary Failed))
pctResult = Submission_Result undefined (PctEval (Percentage (Scores [0.1])))

sumBinaryResultTests = [
    Assertion "Empty list" (sumBinaryResult []) (Right Failed)
  , Assertion "Homogenous passed list" (sumBinaryResult [binPassed, binPassed]) (Right Passed)
  , Assertion "Homogenous failed list" (sumBinaryResult [binPassed, binFailed]) (Right Failed)
  , Assertion "Inhomogenous list" (sumBinaryResult [binPassed, binFailed, pctResult, binPassed])
              (Left "Not a binary evaluation")
  ]

cfg30 = PctConfig 0.3 -- At least 30% is needed to pass
cfg40 = PctConfig 0.4 -- At least 40% is needed to pass
pct x = Submission_Result undefined (PctEval (Percentage (Scores [x])))

sumPercentageResultTests = [
    Assertion "Empty list"     (sumPercentageResult cfg30 []) (Right Failed)
  , Assertion "30% and passed" (sumPercentageResult cfg30 [pct 0.3]) (Right Passed)
  , Assertion "40% and failed" (sumPercentageResult cfg40 [pct 0.3]) (Right Failed)
  , Assertion "60/200 and passed" (sumPercentageResult cfg30 [pct 0.1, pct 0.5]) (Right Passed)
  , Assertion "50/200 and failed" (sumPercentageResult cfg30 [pct 0, pct 0.5]) (Right Failed)
  , Assertion "Inhomogenous list" (sumPercentageResult cfg30 [pct 0, binPassed])
                                  (Left "Not a percentage evaluation")
  ]

binConfig = BinEval ()
pctConfig = PctEval cfg30

calculateSubmissionResultTests = [
    Assertion "Binary config, failed"
              (calculateSubmissionResult [binPassed, binFailed] binConfig) (Right Failed)
  , Assertion "Percentage config, failed"
              (calculateSubmissionResult [pct 0.3, pct 0.1] pctConfig) (Right Failed)
  , Assertion "Binary config, wrong list"
              (calculateSubmissionResult [binPassed, binFailed] pctConfig)
              (Left "Not a percentage evaluation")
  , Assertion "Percentage config, wrong list"
              (calculateSubmissionResult [pct 0.3, pct 0.1] binConfig)
              (Left "Not a binary evaluation")
  ]
#endif
