{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Home (
    home
  ) where

import Data.List (intersperse)
import Data.String (fromString)
import Control.Monad (join, when, liftM)

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
  (blaze . withUserFrame s . homeContent) =<<
    (runStoryE $ do
       a <- userAssignments
       t <- submissionTables
       return HomePageData {
           userState   = s
         , assignments = a
         , sTables     = t
         })


homeContent :: HomePageData -> Html
homeContent d = do
  let s = userState d
  when (isAdmin s) $ H.p $ "Admin's menu"
  when (isCourseAdmin s) $ H.p $ do
    "Course Admin's menu"
    linkToPage P.NewCourseAssignment
  when (isProfessor s) $ H.p $ do
    "Teacher's menu"
    linkToPage P.NewGroupAssignment
  when (isCourseAdmin s || isProfessor s) $ H.p $ do
    "Submission table"
    htmlSubmissionTables (sTables d)
  when (isStudent s) $ H.p $ do
    "Student's menu"
    availableAssignments (assignments d)

availableAssignments :: [(AssignmentKey,AssignmentDesc)] -> Html
availableAssignments as = do
  table "available-assignments" $ do
    mapM_ assignmentLine as
  where
    assignmentLine (k,a) = H.tr $ do
      H.td $ link (routeWithParams P.Submission [requestParam k]) "New submission"
      H.td (fromString . aGroup $ a)
      H.td (fromString . join . intersperse ", " . aTeachers $ a)
      H.td $ link (routeWithParams P.SubmissionList [requestParam k]) (fromString (aTitle a))
      H.td (fromString . show . aOk  $ a)
      H.td (fromString . show . aNew $ a)
      H.td (fromString . show . aBad $ a)

htmlSubmissionTables :: [SubmissionTableInfo] -> Html
htmlSubmissionTables xs = mapM_ htmlSubmissionTable . zip [1..] $ xs

htmlSubmissionTable :: (Int,SubmissionTableInfo) -> Html
htmlSubmissionTable (i,s) = table (join ["st", show i]) $ do
  headLine (stCourse s)
  assignmentLine (stAssignments s)
  mapM_ userLine (stUserLines s)
  where
    headLine = H.tr . H.th . fromString
    assignmentLine as = H.tr $ do
      H.td "Name"
      H.td "Username"
      mapM_ (H.td . modifyAssignmentLink) . zip [1..] $ as
      H.td "Passed"

    modifyAssignmentLink (i,ak) =
      link (routeWithParams P.ModifyAssignment [requestParam ak])
           (show i)

    userLine (u, p, as) = H.tr $ do
      let username = ud_username u
      H.td . fromString . ud_fullname $ u
      H.td . fromString . show $ username
      mapM_ (H.td . submissionCell username) $ as
      H.td . fromString . show $ p

    submissionCell u (ak,s) =
      link (routeWithParams P.UserSubmissions [requestParam u, requestParam ak]) (sc s)
      where
        sc Submission_Not_Found   = " "
        sc Submission_Unevaulated = "."
        sc (Submission_Failed _)  = "X"
        sc (Submission_Passed _)  = "O"
