{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Data.List (intersperse)
import Data.String (fromString)
import Control.Monad (join, when, liftM)

import Bead.Domain.Shared
import Bead.Domain.Evaulation
import Bead.Domain.Relationships (AssignmentDesc(..))
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Controller.Pages as P (Page(..))
import Bead.Controller.UserStories (userAssignments, submissionTables)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

home :: Content
home = getContentHandler homePage

data HomePageData = HomePageData {
    userState   :: UserState
  , assignments :: [(AssignmentKey, AssignmentDesc)]
  , sTables     :: [SubmissionTableInfo]
  }

homePage :: GETContentHandler
homePage = withUserStateE $ \s ->
  (renderPagelet . withUserFrame s . homeContent) =<<
    (runStoryE $ do
       a <- userAssignments
       t <- submissionTables
       return HomePageData {
           userState   = s
         , assignments = a
         , sTables     = t
         })


homeContent :: HomePageData -> Pagelet
homeContent d = onlyHtml $ mkI18NHtml $ \i18n -> do
  let s = userState d
  when (isAdmin s) $ H.p $ (translate i18n "Admin's menu")
  when (isCourseAdmin s) $ H.p $ do
    (translate i18n "Course Admin's menu")
    linkToPage P.NewCourseAssignment
  when (isProfessor s) $ H.p $ do
    (translate i18n "Teacher's menu")
    linkToPage P.NewGroupAssignment
  when (isCourseAdmin s || isProfessor s) $ H.p $ do
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
      H.td $ link (routeWithParams P.Submission [requestParam k]) (i18n "New submission")
      H.td (fromString . aGroup $ a)
      H.td (fromString . join . intersperse ", " . aTeachers $ a)
      H.td $ link (routeWithParams P.SubmissionList [requestParam k]) (fromString (aTitle a))
      H.td (fromString . show . aOk  $ a)
      H.td (fromString . show . aNew $ a)
      H.td (fromString . show . aBad $ a)

htmlSubmissionTables :: I18N -> [SubmissionTableInfo] -> Html
htmlSubmissionTables i18n xs = mapM_ (htmlSubmissionTable i18n) . zip [1..] $ xs

htmlSubmissionTable :: I18N -> (Int,SubmissionTableInfo) -> Html
htmlSubmissionTable i18n (i,s) = table (join ["st", show i]) (className groupSubmissionTable) $ do
  headLine (stCourse s)
  assignmentLine (stAssignments s)
  mapM_ userLine (stUserLines s)
  where
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
      H.td $ link (routeWithParams P.UserSubmissions [requestParam u, requestParam ak]) (sc s)
      where
        sc Submission_Not_Found   = " "
        sc Submission_Unevaulated = "."
        sc (Submission_Result _ r) = val r

        val (BinEval (Binary Passed)) = "1"
        val (BinEval (Binary Failed)) = "0"
        val (PctEval (Percentage (Scores [p]))) = percent p

        percent x = join [show . round $ (100 * x), "%"]
