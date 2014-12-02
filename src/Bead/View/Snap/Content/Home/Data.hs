{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Snap.Content.Home.Data where

import           Data.Time

import           Bead.View.Snap.Content hiding (userState)
import           Bead.View.Snap.Content.SubmissionTable

data HomePageData = HomePageData {
    userState   :: UserState
  , hasCourses  :: Bool -- True if the user has administrated courses
  , hasGroups   :: Bool -- True if the user has administrated groups
  , assignments :: Maybe [(AssignmentKey, AssignmentDesc, SubmissionInfo)] -- Nothing means that the user is not registrated in any courses
  , sTables     :: [SubmissionTableInfo]
    -- ^ The convertes function that convert a given utc time into the users local timezone
  , timeConverter :: UserTimeConverter
  , submissionTableCtx :: SubmissionTableContext
  , now :: UTCTime
  }

administratedCourseMap = stcAdminCourses . submissionTableCtx
administratedGroupMap  = stcAdminGroups  . submissionTableCtx
courseTestScripts      = stcCourseTestScriptInfos . submissionTableCtx
