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
  renderBootstrapPage . bootStrapUserFrame s . content $ CourseSubmissions now stc sti

content :: PageData -> IHtml
content = pageDataCata courseSubmissionsContent

courseSubmissionsContent :: UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
courseSubmissionsContent now c s = do
  msg <- getI18N
  return $ do
            -- Header
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ H.div ! class_ "page-header" $ do
                hr
                h1 . fromString . msg $ Msg_LinkText_CourseOverview "Course Overview"

            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ p $ fromString  . msg $ Msg_Home_SubmissionTable_Info $ concat
              [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
              , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
              , "then clicking on the button."
              ]
            i18n msg $ submissionTable "course-table" now c s

            -- End
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr
