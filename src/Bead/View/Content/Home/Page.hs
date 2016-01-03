{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Home.Page (
    home
  , deleteUsersFromCourse
  , deleteUsersFromGroup
  ) where

import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Time

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (id)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Evaluation
import           Bead.View.Content hiding (userState)
import qualified Bead.View.UserActions as UA
import           Bead.View.Content.SubmissionTable as ST

#ifdef TEST
import           Test.Tasty.TestSet
#endif

import           Bead.View.Content.Home.Data
import           Bead.View.Content.Home.View

home :: ViewHandler
home = ViewHandler homePage

deleteUsersFromCourse :: ModifyHandler
deleteUsersFromCourse = ModifyHandler deleteUsersFromCourseHandler

deleteUsersFromGroup :: ModifyHandler
deleteUsersFromGroup = ModifyHandler deleteUsersFromGroupHandler

administratedCourseMap = stcAdminCourses . submissionTableCtx
administratedGroupMap  = stcAdminGroups  . submissionTableCtx
courseTestScripts      = stcCourseTestScriptInfos . submissionTableCtx

homePage :: GETContentHandler
homePage = withUserState $ \s -> do
  converter <- userTimeZoneToLocalTimeConverter
  now <- liftIO getCurrentTime
  fmap homeContent $ do
    (userStory $ do
       ua <- S.userAssignments
       sbmTables <- (map sortUserLines <$> S.submissionTables)
       scoreBoards <- S.scoreBoards
       stc <- ST.submissionTableContext
       return $
         HomePageData
           s
           (not . Map.null $ stcAdminCourses stc)
           (not . Map.null $ stcAdminGroups stc)
           ua
           sbmTables
           scoreBoards
           converter
           stc
           now)

deleteUsersFromCourseHandler :: POSTContentHandler
deleteUsersFromCourseHandler =
  UA.DeleteUsersFromCourse
    <$> (getParameter delUserFromCourseKeyPrm)
    <*> (getParameterValues delUserFromCoursePrm)

deleteUsersFromGroupHandler :: POSTContentHandler
deleteUsersFromGroupHandler =
  UA.DeleteUsersFromGroup
    <$> (getParameter delUserFromGroupKeyPrm)
    <*> (getParameterValues delUserFromGroupPrm)

navigation :: [Pages.Page a b c d e] -> IHtml
navigation links = do
  msg <- getI18N
  return $ H.div ! A.id "menu" $ H.ul $ mapM_ (i18n msg . linkToPage) links

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _us as _uls _ans _ck = as
  group _n _us cgas _uls _ans _ck _gk = map (cgInfoCata id id) cgas
