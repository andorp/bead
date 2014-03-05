{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseOverview (
    courseOverview
  ) where

import           Data.String (fromString)
import           Data.Time (UTCTime, getCurrentTime)

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as P (Page(..))
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Snap.Content
import           Bead.View.Snap.RequestParams
import           Bead.View.Snap.Content.SubmissionTable

data PageData
  = CourseSubmissions UTCTime SubmissionTableContext SubmissionTableInfo

pageDataCata
  courseSubmissions
  p = case p of
    CourseSubmissions now stc sti -> courseSubmissions now stc sti

courseOverview :: Content
courseOverview = getContentHandler courseSubmissionsPage

courseSubmissionsPage :: GETContentHandler
courseSubmissionsPage = withUserState $ \s -> do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  now <- liftIO $ getCurrentTime
  (stc,sti) <- userStory $ do
    stc <- submissionTableContext
    sti <- Story.courseSubmissionTable ck
    return (stc, sti)
  renderPagelet . withUserFrame s . content $ CourseSubmissions now stc sti

content :: PageData -> IHtml
content = pageDataCata courseSubmissionsContent

courseSubmissionsContent :: UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
courseSubmissionsContent now c s = do
  msg <- getI18N
  return $ do
    H.p $ fromString . msg $ Msg_Home_SubmissionTable_Info $ concat
      [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
      , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
      , "then clicking on the button."
      ]
    H.p . i18n msg $ submissionTable "course-table" now c s
    H.p $ fromString . msg $ Msg_Home_CourseAdministration_Info $ concat
      [ "New groups for courses may be created in the Course Settings menu.  Teachers may be also assigned to "
      , "each of the groups there as well."
      ]
    H.p $ i18n msg $ navigation $ [
        P.CourseAdmin, P.NewTestScript, P.EvaluationTable
      , P.SetUserPassword, P.UploadFile ]

navigation :: [P.Page] -> IHtml
navigation links = do
  msg <- getI18N
  return $ H.div ! A.id "menu" $ H.ul $ mapM_ (i18n msg . linkToPage) links
