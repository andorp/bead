{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Numeric (showHex)
import Data.List (intersperse)
import Data.String (fromString)
import Control.Monad (join, when, liftM)

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
homePage = withUserStateE $ \s ->
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
  when (isAdmin s) $ H.p $ (translate i18n "Admin's menu")
  when (hasCourses d) $ H.p $ do
    (translate i18n "Course Admin's menu")
    linkToPage P.NewCourseAssignment
  when (hasGroups d) $ H.p $ do
    (translate i18n "Teacher's menu")
    linkToPage P.NewGroupAssignment
  when (or [hasCourses d, hasGroups d]) $ H.p $ do
    (translate i18n "Submission table")
    htmlSubmissionTables i18n (sTables d)
  when (isStudent s) $ H.p $ do
    (translate i18n "Student's menu")
    availableAssignments i18n (assignments d)

availableAssignments :: I18N -> [(AssignmentKey,AssignmentDesc)] -> Html
availableAssignments i18n as = do
  table (fieldName availableAssignmentsTable) (className assignmentTable) $ do
    mapM_ assignmentLine as
  where
    assignmentLine (k,a) = H.tr $ do
      case aActive a of
        True -> H.td $ link (routeWithParams P.Submission [requestParam k]) (i18n "New submission")
        False -> H.td "Inactive"
      H.td (fromString . aGroup $ a)
      H.td (fromString . join . intersperse ", " . aTeachers $ a)
      H.td $ link (routeWithParams P.SubmissionList [requestParam k]) (fromString (aTitle a))
      H.td (fromString . show . aOk  $ a)
      H.td (fromString . show . aNew $ a)
      H.td (fromString . show . aBad $ a)

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
htmlSubmissionTable i18n (i,s) = table tableId (className groupSubmissionTable) $ do
  headLine (stCourse s)
  assignmentLine (stAssignments s)
  mapM_ userLine (stUserLines s)
  where
    tableId = join ["st", show i]
    headLine = H.tr . H.th . fromString
    assignmentLine as = H.tr $ do
      H.th (translate i18n "Name")
      H.th (translate i18n "Username")
      mapM_ (H.th . modifyAssignmentLink) . zip [1..] $ as
      H.th (translate i18n "Passed")

    modifyAssignmentLink (i,ak) =
      link (routeWithParams P.ModifyAssignment [requestParam ak])
           (show i)

    userLine (u, p, as) = H.tr $ do
      let username = ud_username u
      H.td . fromString . ud_fullname $ u
      H.td . fromString . show $ username
      mapM_ (submissionCell username) $ as
      H.td . fromString . show $ p

    submissionCell u (ak,s) =
      coloredCell $ link (routeWithParams P.UserSubmissions [requestParam u, requestParam ak]) (sc s)
      where
        sc Submission_Not_Found   = " "
        sc Submission_Unevaulated = "."
        sc (Submission_Result _ r) = val r

        val (BinEval (Binary Passed)) = "1"
        val (BinEval (Binary Failed)) = "0"
        val (PctEval (Percentage (Scores [p]))) = percent p

        coloredCell = color s H.td

        color (Submission_Not_Found)   x = x
        color (Submission_Unevaulated) x = x ! A.class_ (className submissionUnevaulated)
        color (Submission_Result _ r)  x = resultCell r x

        resultCell (BinEval (Binary Passed)) x = x ! A.class_ (className submissionBinaryPassed)
        resultCell (BinEval (Binary Failed)) x = x ! A.class_ (className submissionBinaryFailed)
        resultCell p@(PctEval {}) x = withRGBClass (EvResult p) x

        percent x = join [show . round $ (100 * x), "%"]

        withRGBClass r = maybe id (\pct html -> html ! (A.style . fromString . colorStyle . pctCellColor $ pct)) (percentValue r)


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

