{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Content.Home.View where

import           Control.Arrow ((***))
import           Control.Monad.Identity
import           Data.Function (on)
import           Data.List (find, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (isNothing, isJust)
import qualified Data.Set as Set
import           Data.String (fromString)

import           Text.Blaze.Html5 hiding (map, id)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (id)

import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Entities as E (Role(..))
import           Bead.Domain.Evaluation
import           Bead.View.Content as Content hiding (userState, table)
import           Bead.View.Content.SubmissionTable as ST

import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Home.Data

homeContent :: HomePageData -> IHtml
homeContent d = do
  let s = userState d
      r = role s
      hasCourse = hasCourses d
      hasGroup  = hasGroups d
      testScripts = courseTestScripts d
  msg <- getI18N
  return $ do
            when (isAdmin s) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ do
                h3 . fromString . msg $ Msg_Home_AdminTasks "Administrator Menu"
                i18n msg $ navigation [administration]

            -- Course Administration Menu
            when (courseAdminUser r) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ do
                h3 . fromString . msg $ Msg_Home_CourseAdminTasks "Course Administrator Menu"
                when (not hasCourse) $ do
                  H.p $ fromString . msg $ Msg_Home_NoCoursesYet
                    "There are no courses.  Contact the administrator to have courses assigned."

            -- Submission tables for course or group assignments
            when ((courseAdminUser r) || (groupAdminUser r)) $ do
              when hasGroup $ do
                when (not . null $ concatMap submissionTableInfoAssignments $ sTables d) $ do
                  Bootstrap.row $ Bootstrap.colMd12 $ p $ fromString . msg $ Msg_Home_SubmissionTable_Info $ concat
                    [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
                    , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
                    , "then clicking on the button."
                    ]
                i18n msg $ htmlSubmissionTables d

              -- HR
              Bootstrap.row $ Bootstrap.colMd12 $ hr

              -- Course Administration links
              when hasCourse $ do
                Bootstrap.row $ Bootstrap.colMd12 $ h3 $ fromString . msg $ Msg_Home_CourseAdministration "Course Administration"
                Bootstrap.row $ Bootstrap.colMd12 $ fromString . msg $ Msg_Home_CourseSubmissionTableList_Info $ concat
                  [ "Submission table for courses can be found on separate pages, please click on the "
                  , "name of a course."
                  ]
                Bootstrap.row $ Bootstrap.colMd12 $ ul ! class_ "list-group" $ do
                  let courseList = sortBy (compareHun `on` (courseName . snd)) $ Map.toList $ administratedCourseMap d
                  forM_ courseList $ \(ck, c) ->
                    li ! class_ "list-group-item"
                       $ a ! href (fromString $ routeOf (courseOverview ck))
                       $ (fromString (courseName c))

            -- Course Administration Button Group
            when (courseAdminUser r && hasCourse) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ p $ fromString . msg $ Msg_Home_CourseAdministration_Info $ concat
                [ "New groups for courses may be created in the Course Settings menu.  Teachers may be also assigned to "
                , "each of the groups there as well."
                ]
              i18n msg $ navigation $ courseAdminButtons
            -- Group Administration Button Group
            when (groupAdminUser r && hasGroup) $ do
              i18n msg $ navigation groupAdminButtons

            -- HR
            when (or [groupAdminUser r && hasGroup, courseAdminUser r && hasCourse]) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ hr

            -- Student Menu
            when (not $ isAdmin r) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ h3 $ fromString $ msg $ Msg_Home_StudentTasks "Student Menu"
              i18n msg $ availableAssignments (timeConverter d) (assignments d)
              let noCourseRegistered = Map.null (assignments d)
              when noCourseRegistered $ i18n msg $ navigation [groupRegistration]
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

      -- With LDAP authentication there passwords can not be set.
#ifdef LDAPEnabled
      courseAdminButtons = [courseAdmin, newTestScript, evaluationTable, uploadFile ]
      groupAdminButtons = [evaluationTable, uploadFile ]
#else
      courseAdminButtons = [courseAdmin, newTestScript, evaluationTable, setUserPassword, uploadFile ]
      groupAdminButtons = [evaluationTable, setUserPassword, uploadFile ]
#endif

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

navigation :: [Pages.Page a b c d e] -> IHtml
navigation links = do
  msg <- getI18N
  return
    $ Bootstrap.row
    $ Bootstrap.colMd12
    $ H.div ! class_ "btn-group"
    $ mapM_ (i18n msg . linkButtonToPageBS) links

availableAssignments :: UserTimeConverter -> StudentAssignments -> IHtml
availableAssignments timeconverter studentAssignments
  | isNotRegistered studentAssignments = do
      msg <- getI18N
      return
        $ Bootstrap.row
        $ Bootstrap.colMd12
        $ p
        $ fromString
        $ msg $ Msg_Home_HasNoRegisteredCourses "There are no registered courses, register to some."

  | null (toAllActiveAssignmentList studentAssignments) = do
      msg <- getI18N
      return
        $ Bootstrap.row
        $ Bootstrap.colMd12
        $ p
        $ fromString
        $ msg $ Msg_Home_HasNoAssignments "There are no available assignments yet."

  | otherwise = do
      -- Sort course or groups by their name.
      let asl = sortBy (compare `on` fst)
                  $ map (courseName *** id)
                  $ toActiveAssignmentList studentAssignments
      msg <- getI18N
      return $ do
        Bootstrap.row
          $ Bootstrap.colMd12
          $ p
          $ fromString . msg $ Msg_Home_Assignments_Info $ concat
            [ "Submissions and their evaluations may be accessed by clicking on each assignment's link. "
            , "The table shows only the last evaluation per assignment."
            ]
        i18n msg $ navigation [groupRegistration]
        forM_ asl $ \(key, as) -> when (not $ null as) $ Bootstrap.rowColMd12 $ do
          h4 $ fromString key
          let areIsolateds = areOpenAndIsolatedAssignments as
          let assignments = if areIsolateds then (isolatedAssignments as) else as
          let isLimited = isLimitedAssignments assignments
          when areIsolateds $ p $ fromString . msg $ Msg_Home_ThereIsIsolatedAssignment $ concat
            [ "ISOLATED MODE: There is at least one assignment which hides the normal assignments for "
            , "this course."
            ]
          Bootstrap.table $ do
            thead $ headerLine msg isLimited
            -- Sort assignments by their end date time in reverse
            tbody $ mapM_ (assignmentLine msg isLimited)
                  $ reverse $ sortBy (compare `on` (aEndDate . activeAsgDesc))
                  $ assignments
  where
    isLimitedAssignments = isJust . find limited

    limited = submissionLimit (const False) (\_ _ -> True) (const True) . (\(a,ad,si) -> aLimit ad)

    isOpenAndIsolated a = and [aIsolated a, aActive a]

    areOpenAndIsolatedAssignments = isJust . find (isOpenAndIsolated . activeAsgDesc)

    isolatedAssignments = filter (isOpenAndIsolated . activeAsgDesc)

    groupRegistration = Pages.groupRegistration ()

    headerLine msg isLimited = tr $ do
      th ""
      th (fromString $ msg $ Msg_Home_Course "Course")
      th (fromString $ msg $ Msg_Home_CourseAdmin "Teacher")
      th (fromString $ msg $ Msg_Home_Assignment "Assignment")
      when isLimited $ th (fromString $ msg $ Msg_Home_Limit "Limit")
      th (fromString $ msg $ Msg_Home_Deadline "Deadline")
      th (fromString $ msg $ Msg_Home_Evaluation "Evaluation")

    assignmentLine msg isLimited (k,a,s) = H.tr $ do
      case and [aActive a, noLimitIsReached a] of
        True -> td $ Content.link (routeWithParams (Pages.submission ()) [requestParam k]) (msg $ Msg_Home_NewSolution "New submission")
        False -> td (fromString . msg $ Msg_Home_ClosedSubmission "Closed")
      td (fromString . aGroup $ a)
      td (fromString . join . intersperse ", " . aTeachers $ a)
      td $ linkWithText (routeWithParams (Pages.submissionList ()) [requestParam k]) (fromString (aTitle a))
      when isLimited $ td (fromString . limit $ aLimit a)
      td (fromString . showDate . timeconverter $ aEndDate a)
      let grayLabel  tag = H.span ! class_ "label label-default" $ tag
      let greenLabel tag = H.span ! class_ "label label-success" $ tag
      let redLabel   tag = H.span ! class_ "label label-danger"  $ tag
      let blueLabel  tag = H.span ! class_ "label label-primary" $ tag
      let label = submissionInfoCata grayLabel grayLabel (const grayLabel)
             (\_evKey evResult -> undefined)
      H.td $ withSubmissionInfo s
               (grayLabel $ fromString $ msg $ Msg_Home_SubmissionCell_NoSubmission "No submission")
               (grayLabel $ fromString $ msg $ Msg_Home_SubmissionCell_NonEvaluated "Non-evaluated")
               (bool (grayLabel $ fromString $ msg $ Msg_Home_SubmissionCell_Tests_Passed "Tests are passed")
                     (grayLabel $ fromString $ msg $ Msg_Home_SubmissionCell_Tests_Failed "Tests are failed"))
               (\_key result -> evResult
                                  (greenLabel $ fromString $ msg $ Msg_Home_SubmissionCell_Accepted "Accepted")
                                  (redLabel   $ fromString $ msg $ Msg_Home_SubmissionCell_Rejected "Rejected")
                                  (blueLabel . fromString)
                                  result)
      where
        noLimitIsReached = submissionLimit (const True) (\n _ -> n > 0) (const False) . aLimit
        limit = fromString . submissionLimit
          (const "") (\n _ -> (msg $ Msg_Home_Remains "Remains: ") ++ show n) (const $ msg $ Msg_Home_Reached "Reached")
        evResult passed failed percentage r
          = case r of
             (EvResult (BinEval (Binary Passed))) -> passed
             (EvResult (BinEval (Binary Failed))) -> failed
             (EvResult (PctEval (Percentage (Scores [p])))) -> percentage $ percent p
             (EvResult (PctEval (Percentage _))) -> error "SubmissionTable.coloredSubmissionCell percentage is not defined"

            where percent x = join [show . round $ (100 * x), "%"]
