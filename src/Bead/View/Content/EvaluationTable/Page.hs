{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.EvaluationTable.Page (
    evaluationTable
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Data.Time (UTCTime)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (openSubmissions)
import           Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Pagelets
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

import           Text.Blaze.Html5 as H hiding (link, map)

evaluationTable :: ViewHandler
evaluationTable = ViewHandler evaluationTablePage

evaluationTablePage :: GETContentHandler
evaluationTablePage =
  evaluationTableContent
  <$> userTimeZoneToLocalTimeConverter
  <*> userStory openSubmissions

evaluationTableContent :: UserTimeConverter -> OpenedSubmissions -> IHtml
evaluationTableContent tc = openedSubmissionsCata $ \admincourse admingroup related -> do
  msg <- getI18N
  return $ do
    noUnevaluatedSubmission msg admincourse admingroup related
    when (not $ null admingroup) $ Bootstrap.rowColMd12 $ do
      H.h4 $ fromString . msg $ Msg_EvaluationTable_GroupAssignment "Group assignments"
      fromString . msg $ Msg_EvaluationTable_GroupAssignmentInfo $ concat
        [ "Submissions for group assignments sent by the users who correspond "
        , "the groups administrated by you."
        ]
      evaluationTable msg (sortSubmissions admingroup) isGroup
    when (not $ null admincourse) $ Bootstrap.rowColMd12 $ do
      H.h4 $ fromString . msg $ Msg_EvaluationTable_CourseAssignment "Course assignments"
      fromString . msg $ Msg_EvaluationTable_CourseAssignmentInfo $ concat
        [ "Submissions for course assignments sent by the users who correspond "
        , "the courses of groups administrated by you."
        ]
      evaluationTable msg (sortSubmissions admincourse) isCourse
    when (not $ null related) $ Bootstrap.rowColMd12 $ do
      H.h4 $ fromString . msg $ Msg_EvaluationTable_MiscCourseAssignment "Miscellaneous Course assignments"
      fromString . msg $ Msg_EvaluationTable_MiscCourseAssignmentInfo $ concat
        [ "Submissions for course assignments sent by the users who correspond "
        , "the courses of groups administrated by you, but not your students."
        ]
      evaluationTable msg (sortSubmissions related) isCourse
  where
    isGroup  = True
    isCourse = False

    noUnevaluatedSubmission msg ac ag rl = Bootstrap.rowColMd12 $ if (and [null ac, null ag, null rl])
      then (H.p $ fromString $ msg $ Msg_EvaluationTable_EmptyUnevaluatedSolutions "There are no unevaluated submissions.")
      else
        H.p $ fromString . msg $ Msg_EvaluationTable_Info $ concat
          [ "Only the last unevaluated submission is shown per student. The "
          , "other submissions may be accessed through the submission table "
          , "on the home page."
          ]

    evaluationTable msg ks isGroup =
      when (not $ null ks) $ Bootstrap.rowColMd12 $ do
        Bootstrap.table $ do
          thead $ H.tr $ do
            H.th (fromString . msg $ Msg_EvaluationTable_Link "Link")
            H.th (fromString . msg $ Msg_EvaluationTable_Assignment "Assignment")
            H.th (fromString . msg $ Msg_EvaluationTable_Username "Username")
            H.th (fromString . msg $ Msg_EvaluationTable_Student "Student")
            H.th (fromString . msg $ Msg_EvaluationTable_Course "Course")
            when isGroup $ H.th (fromString . msg $ Msg_EvaluationTable_Group "Group")
            H.th (fromString . msg $ Msg_EvaluationTable_DateOfSubmission "Date")
          tbody $ forM_ ks (submissionInfo tc msg isGroup)

submissionInfo tc msg isGroup (key, desc) = H.tr $ do
  H.td $ link (routeOf (evaluation key)) (msg $ Msg_EvaluationTable_Solution "Submission")
  H.td . fromString . Assignment.name . eAssignment $ desc
  usernameCata (H.td . fromString) $ eUsername desc
  H.td . fromString . eStudent $ desc
  H.td . fromString . eCourse $ desc
  when isGroup $ H.td . fromString . fromMaybe "" . eGroup $ desc
  H.td . fromString . showDate . tc $ eSubmissionDate desc
  where
    evaluation k = Pages.evaluation k ()

-- * Sorting submissions

-- Create an ordered submission list based on the course group and assignment time ordering,
-- the submission list elements are ordered by the submission time of the solution

-- The key for the ordering consists of in order a course name, a group name (possible empty), and
-- the time of the assignment
type SMKey = (String, Maybe String, UTCTime)

type SMVal = (SubmissionKey, SubmissionDesc)

-- SubmissionMap an ordered map which hold several
type SMap = Map SMKey [SMVal]

descToKey :: SMVal -> SMKey
descToKey (_k,d) = (eCourse d, eGroup d, eAssignmentDate d)

insertSMap :: SMap -> SMVal -> SMap
insertSMap m x =
  let key = descToKey x
  in maybe (Map.insert key [x] m) (\xs -> Map.insert key (x:xs) m) (Map.lookup key m)

sortSubmissions :: [SMVal] -> [SMVal]
sortSubmissions [] = []
sortSubmissions [s] = [s]
sortSubmissions sm = concat . map (sortBy cmp) . map snd . Map.toList . foldl insertSMap Map.empty $ sm
  where
    cmp = compare `on` (eSubmissionDate . snd)
