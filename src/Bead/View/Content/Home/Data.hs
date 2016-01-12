{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Content.Home.Data where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time

import           Bead.View.Content hiding (userState)
import           Bead.View.Content.SubmissionTable

type ActiveAssignment = (AssignmentKey, AssignmentDesc, SubmissionInfo)

activeAsgKey  (key,_desc,_info) = key
activeAsgDesc (_key,desc,_info) = desc
activeAsgInfo (_key,_desc,info) = info

type StudentAssignments = Map Course [ActiveAssignment]

-- Returns True if the student is not registered in any courses otherwise False
isNotRegistered :: StudentAssignments -> Bool
isNotRegistered = Map.null

-- Returns all the AcitveAssignment list grouped with its courses or groups
toActiveAssignmentList :: StudentAssignments -> [ (Course, [ActiveAssignment]) ]
toActiveAssignmentList = Map.toList

-- Returns a list of all the ActiveAssignments
toAllActiveAssignmentList :: StudentAssignments -> [ActiveAssignment]
toAllActiveAssignmentList = foldl (++) [] . map snd . toActiveAssignmentList

data HomePageData = HomePageData {
    userState   :: UserState
  , hasCourses  :: Bool -- True if the user has administrated courses
  , hasGroups   :: Bool -- True if the user has administrated groups
  , assignments :: StudentAssignments -- Empty map means that the user is not registrated in any courses
  , sTables     :: [SubmissionTableInfo]
  , assessmentTables :: [ScoreBoard]
  , assessments :: Map.Map Course [(AssessmentKey, ScoreInfo)]
    -- ^ The convertes function that convert a given utc time into the users local timezone
  , timeConverter :: UserTimeConverter
  , submissionTableCtx :: SubmissionTableContext
  , now :: UTCTime
  }

administratedCourseMap = stcAdminCourses . submissionTableCtx
administratedGroupMap  = stcAdminGroups  . submissionTableCtx
courseTestScripts      = stcCourseTestScriptInfos . submissionTableCtx
