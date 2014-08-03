{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Snap.Content.Home.View where

import           Control.Monad.Identity
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import qualified Data.Map as Map
import           Data.String (fromString)

import           Text.Blaze.Html5 hiding (map, id)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (id)

import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Entities as E (Role(..))
import           Bead.View.Snap.Content as Content hiding (userState, table)
import           Bead.View.Snap.Content.SubmissionTableBS as ST

import           Bead.View.Snap.Content.Home.Data

homeContent :: HomePageData -> IHtml
homeContent d = do
  let s = userState d
      r = role s
      hasCourse = hasCourses d
      hasGroup  = hasGroups d
      testScripts = courseTestScripts d
  msg <- getI18N
  return $ do
            -- Header
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ H.div ! class_ "page-header" $ do
                hr
                h1 . fromString . msg $ Msg_LinkText_Home "Home"

            when (isAdmin s) $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ do
                h3 . fromString . msg $ Msg_Home_AdminTasks "Administrator Menu"
                i18n msg $ navigation [administration]

            -- Course Administration Menu
            when (courseAdminUser r) $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ do
                h3 . fromString . msg $ Msg_Home_CourseAdminTasks "Course Administrator Menu"
                when (not hasCourse) $ do
                  H.p $ fromString . msg $ Msg_Home_NoCoursesYet
                    "There are no courses.  Contact the administrator to have courses assigned."

            -- Submission tables for course or group assignments
            when ((courseAdminUser r) || (groupAdminUser r)) $ do
              when hasGroup $ do
                when (not . null $ concatMap submissionTableInfoAssignments $ sTables d) $ do
                  H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ p $ fromString . msg $ Msg_Home_SubmissionTable_Info $ concat
                    [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
                    , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
                    , "then clicking on the button."
                    ]
                i18n msg $ htmlSubmissionTables d

              -- HR
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr

            -- Course Administration links
            when hasCourse $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ h3 $ fromString . msg $ Msg_Home_CourseAdministration "Course Administration"
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ fromString . msg $ Msg_Home_CourseSubmissionTableList_Info $ concat
                [ "Submission table for courses can be found on separate pages, please click on the "
                , "name of a course."
                ]
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ ul ! class_ "list-group" $ do
                let courseList = sortBy (compareHun `on` (courseName . snd)) $ Map.toList $ administratedCourseMap d
                forM_ courseList $ \(ck, c) ->
                  li ! class_ "list-group-item"
                     $ a ! href (fromString $ routeOf (courseOverview ck))
                     $ (fromString (courseName c))

            -- Course Administration Button Group
            when (courseAdminUser r && hasCourse) $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ p $ fromString . msg $ Msg_Home_CourseAdministration_Info $ concat
                [ "New groups for courses may be created in the Course Settings menu.  Teachers may be also assigned to "
                , "each of the groups there as well."
                ]
              i18n msg $ navigation $ [
                  courseAdmin, newTestScript, evaluationTable
                , setUserPassword, uploadFile ]

            -- Group Administration Button Group
            when (groupAdminUser r && hasGroup) $ do
              i18n msg $ navigation [evaluationTable, setUserPassword, uploadFile ]

            -- HR
            when (or [groupAdminUser r && hasGroup, courseAdminUser r && hasCourse]) $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr

            -- Student Menu
            when (not $ isAdmin r) $ do
              H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ h3 $ fromString $ msg $ Msg_Home_StudentTasks "Student Menu"
              i18n msg $ availableAssignments (timeConverter d) (assignments d)

            -- End
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr

  where
      administration    = Pages.administration ()
      courseAdmin       = Pages.courseAdmin ()
      courseOverview ck = Pages.courseOverview ck ()
      evaluationTable   = Pages.evaluationTable ()
      newTestScript     = Pages.newTestScript ()
      setUserPassword   = Pages.setUserPassword ()
      submission     = Pages.submission ()
      submissionList = Pages.submissionList ()
      uploadFile     = Pages.uploadFile ()

      courseAdminUser = (==E.CourseAdmin)
      groupAdminUser  = (==E.GroupAdmin)

-- * Helpers

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _us as _uls _ans _ck = as
  group _n _us cgas _uls _ans _ck _gk = map (cgInfoCata id id) cgas

htmlSubmissionTables :: HomePageData -> IHtml
htmlSubmissionTables pd = do
  tables <- mapM (htmlSubmissionTable pd) $ zip [1..] (sTables pd)
  return $ sequence_ tables
  where
    htmlSubmissionTable pd (i,s) = do
      submissionTable (concat ["st", show i]) (now pd) (submissionTableCtx pd) s

table' m
  = H.div ! class_ "row"
  $ H.div ! class_ "col-md-12"
  $ H.table ! class_ "table table-bordered table-condensed table-striped table-hover" $ m

navigation :: [Pages.Page a b c d] -> IHtml
navigation links = do
  msg <- getI18N
  return
    $ H.div ! class_ "row"
    $ H.div ! class_ "col-md-12"
    $ H.div ! class_ "btn-group" -- $ a ! href "#" ! class_ "btn btn-default" $ "Group Registration"
    $ mapM_ (i18n msg . linkButtonToPageBS) links

--  return $ H.div ! A.id "menu" $ H.ul $ mapM_ (i18n msg . (linkToPage ! class_ "btn btn-default") links

availableAssignments :: UserTimeConverter -> Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)] -> IHtml
availableAssignments _ Nothing = do
  msg <- getI18N
  return
    $ H.div ! class_ "row"
    $ H.div ! class_ "col-md-12"
    $ p
    $ fromString
    $ msg $ Msg_Home_HasNoRegisteredCourses "There are no registered courses, register to some."

availableAssignments _ (Just []) = do
  msg <- getI18N
  return
    $ H.div ! class_ "row"
    $ H.div ! class_ "col-md-12"
    $ p
    $ fromString
    $ msg $ Msg_Home_HasNoAssignments "There are no available assignments yet."

availableAssignments timeconverter (Just as) = do
  msg <- getI18N
  return $ do
    H.div ! class_ "row"
      $ H.div ! class_ "col-md-12"
      $ p
      $ fromString . msg $ Msg_Home_Assignments_Info $ concat
        [ "Submissions and their evaluations may be accessed by clicking on each assignment's link. "
        , "The table shows only the last evaluation per assignment."
        ]
    i18n msg $ navigation [groupRegistration]
    table' $ do
      thead $ headerLine msg
      tbody $ mapM_ (assignmentLine msg) as
  where
    groupRegistration = Pages.groupRegistration ()

    headerLine msg = tr $ do
      th ""
      th (fromString $ msg $ Msg_Home_Course "Course")
      th (fromString $ msg $ Msg_Home_CourseAdmin "Teacher")
      th (fromString $ msg $ Msg_Home_Assignment "Assignment")
      th (fromString $ msg $ Msg_Home_Deadline "Deadline")
      th (fromString $ msg $ Msg_Home_Evaluation "Evaluation")

    assignmentLine msg (k,a,s) = H.tr $ do
      case aActive a of
        True -> td $ Content.link (routeWithParams (Pages.submission ()) [requestParam k]) (msg $ Msg_Home_NewSolution "New submission")
        False -> td (fromString . msg $ Msg_Home_ClosedSubmission "Closed")
      td (fromString . aGroup $ a)
      td (fromString . join . intersperse ", " . aTeachers $ a)
      td $ linkWithText (routeWithParams (Pages.submissionList ()) [requestParam k]) (fromString (aTitle a))
      td (fromString . showDate . timeconverter $ aEndDate a)
      (resultCell fromString {-coloredSubmissionCell (const H.td) (H.td) fromString -}
        (msg $ Msg_Home_SubmissionCell_NoSubmission "No submission")
        (msg $ Msg_Home_SubmissionCell_NonEvaluated "Non-evaluated")
        (msg $ Msg_Home_SubmissionCell_Tested "Tested")
        (msg $ Msg_Home_SubmissionCell_Accepted "Accepted")
        (msg $ Msg_Home_SubmissionCell_Rejected "Rejected")
        s)
