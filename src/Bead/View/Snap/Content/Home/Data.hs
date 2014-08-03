{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Snap.Content.Home.Data where

import           Control.Monad.Identity
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String (fromString)
import           Data.Time

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (style, id)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities as E (Role(..))
import           Bead.Domain.Evaluation
import           Bead.View.Snap.Content hiding (userState)
import qualified Bead.View.UserActions as UA
import           Bead.View.Snap.Content.SubmissionTableBS as ST

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
