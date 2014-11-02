{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.CourseOverview.Page (
    courseOverview
  ) where

import           Data.String (fromString)
import           Data.Time (UTCTime, getCurrentTime)

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (content)

import qualified Bead.Controller.UserStories as Story
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import           Bead.View.Snap.RequestParams
import           Bead.View.Snap.Content.SubmissionTableBS

data PageData
  = CourseSubmissions UTCTime SubmissionTableContext SubmissionTableInfo

pageDataCata
  courseSubmissions
  p = case p of
    CourseSubmissions now stc sti -> courseSubmissions now stc sti

courseOverview :: ViewHandler
courseOverview = ViewHandler courseSubmissionsPage

courseSubmissionsPage :: GETContentHandler
courseSubmissionsPage = withUserState $ \s -> do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  now <- liftIO $ getCurrentTime
  (stc,sti) <- userStory $ do
    stc <- submissionTableContext
    sti <- sortUserLines <$> Story.courseSubmissionTable ck
    return (stc, sti)
  renderBootstrapPage . bootstrapUserFrame s . content $ CourseSubmissions now stc sti

content :: PageData -> IHtml
content = pageDataCata courseSubmissionsContent

courseSubmissionsContent :: UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
courseSubmissionsContent now c s = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $
      fromString  . msg $ Msg_Home_SubmissionTable_Info $ concat
        [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
        , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
        , "then clicking on the button."
        ]
    i18n msg $ submissionTable "course-table" now c s
    Bootstrap.rowColMd12 $ hr
