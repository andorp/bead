{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  ) where

import Control.Monad (liftM)
import Data.Either (either)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time as Time
import Data.String (fromString)

import Bead.Domain.Entities (dataTimeZone)
import Bead.Controller.Pages (Page)
import qualified Bead.Controller.Pages as P (Page(..))
import Bead.Controller.ServiceContext (UserState(..))
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.UserActions (UserAction(CreateGroupAssignment, CreateCourseAssignment))

import Text.Printf (printf)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A (id, style, href)

-- * Content Handlers

newCourseAssignment :: Content
newCourseAssignment = getPostContentHandler newCourseAssignmentPage postCourseAssignment

newGroupAssignment :: Content
newGroupAssignment = getPostContentHandler newGroupAssignmentPage postGroupAssignment

modifyAssignment :: Content
modifyAssignment = getPostContentHandler modifyAssignmentPage postModifyAssignment

data PageData
  = PD_Course     (Time.TimeZone, UTCTime, [(CourseKey, Course)])
  | PD_Group      (Time.TimeZone, UTCTime, [(GroupKey, Group, String)])
  | PD_Assignment (Time.TimeZone, AssignmentKey, Assignment)
  -- TODO: Calculate the time differences and shows the values in
  -- the actual time zone

pageDataCata course group assignment p = case p of
  PD_Course x -> course x
  PD_Group  x -> group x
  PD_Assignment x -> assignment x

isEmptyData = pageDataCata (null . trd) (null . trd) (const False)

-- * Course Assignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = withUserState $ \s -> do
  cs <- runStoryE S.administratedCourses
  now <- liftIO $ getCurrentTime
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Course (tz,now,cs)))

postCourseAssignment :: POSTContentHandler
postCourseAssignment = CreateCourseAssignment
  <$> getParameter (customCourseKeyPrm (fieldName selectedCourse))
  <*> getValue -- assignment

-- * Group Assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserState $ \s -> do
  now <- liftIO $ getCurrentTime
  gs <- runStoryE S.administratedGroups
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Group (tz,now,gs)))

postGroupAssignment :: POSTContentHandler
postGroupAssignment = CreateGroupAssignment
  <$> getParameter (customGroupKeyPrm (fieldName selectedGroup))
  <*> getValue -- assignment

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  as <- runStoryE (S.loadAssignment ak)
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Assignment (tz,ak,as)))

postModifyAssignment :: POSTContentHandler
postModifyAssignment = ModifyAssignment <$> getValue <*> getValue

newAssignmentContent :: PageData -> Pagelet
newAssignmentContent pd
  | isEmptyData pd = onlyHtml $ mkI18NHtml $ \i -> do
      H.p $ pageDataCata (const . translate i $ "Nem vagy felelőse egyik tárgynak sem!")
                         (const . translate i $ "Nem vagy oktatója egyetlen csoportnak sem!")
                         (const . translate i $ "Ezt a feladatot más hozta létre!")
                         pd
newAssignmentContent pd = onlyHtml $ mkI18NHtml $ \i -> postForm (routeOf . page $ pd) `withId` (hookId assignmentForm) $ H.div ! formDiv $ do
  H.div ! slimRightCell $ do
    H.b $ (translate i "Cím")
    textInput (fieldName assignmentNameField) 10 (amap assignmentName pd) ! fillDiv
    H.br
  H.div ! leftCell $ do
    H.b $ (translate i "Beadás ideje")
    H.div ! A.id (fieldName startDateDivId) $ do
       translate i "Kezdés"
       fromString $ concat [" (", Time.timeZoneName timezone, ")"]
       H.br
       hiddenInput (fieldName assignmentStartDefaultDate) (fromString startDefDate)
       hiddenInput (fieldName assignmentStartDefaultHour) (fromString startDefHour)
       hiddenInput (fieldName assignmentStartDefaultMin)  (fromString startDefMin)
       hiddenInput (fieldName assignmentStartField) (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"])
    H.div ! A.id (fieldName endDateDivId) $ do
       translate i "Befejezés"
       fromString $ concat [" (", Time.timeZoneName timezone, ")"]
       H.br
       hiddenInput (fieldName assignmentEndDefaultDate) (fromString endDefDate)
       hiddenInput (fieldName assignmentEndDefaultHour) (fromString endDefHour)
       hiddenInput (fieldName assignmentEndDefaultMin)  (fromString endDefMin)
       hiddenInput (fieldName assignmentEndField) (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"])
  H.div ! rightCell $ do
    H.br
    H.b $ (translate i "Szöveges leírás")
    textAreaInput (fieldName assignmentDescField) (amap assignmentDesc pd) ! fillDiv
    H.a ! A.href linkToPandocMarkdown $ do translate i "Markdown formázás"
    translate i " használható."
  H.div ! leftCell $ do
    H.b $ (translate i "Típus")
    H.br
    enumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType $ pd)
    H.br
    H.p $ do
      H.b $ pageDataCata (const (translate i "Tárgy")) (const (translate i "Csoport")) (const (translate i "")) pd
      H.br
      pageDataCata
        (valueTextSelection (fieldName selectedCourse) . trd)
        (valueTextSelection (fieldName selectedGroup)  . trd)
        (hiddenInput (fieldName assignmentKeyField) . paramValue  . snd3)
        pd
    H.p $ submitButton (fieldName saveSubmitBtn) (i "Mentés")

    where
      linkToPandocMarkdown = "http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html"

      page :: PageData -> Page
      page = pageDataCata
                   (const P.NewCourseAssignment)
                   (const P.NewGroupAssignment)
                   (const P.ModifyAssignment)

      amap :: (Assignment -> a) -> PageData -> Maybe a
      amap f (PD_Assignment (_,_,a)) = Just . f $ a
      amap _ _                     = Nothing

      timezone = pageDataCata
        fst3
        fst3
        fst3
        pd

      date t =
        let localTime = Time.utcToLocalTime timezone t
            timeOfDay = Time.localTimeOfDay localTime
        in ( show $ Time.localDay         localTime
           , printf "%02d" $ Time.todHour timeOfDay
           , printf "%02d" $ Time.todMin  timeOfDay
           )

      (startDefDate, startDefHour, startDefMin) = date $ pageDataCata
        snd3
        snd3
        (assignmentStart . trd)
        pd

      (endDefDate, endDefHour, endDefMin) = date $ pageDataCata
        snd3
        snd3
        (assignmentEnd . trd)
        pd

-- CSS Section

slimLeftCell  = A.style "float: left;  width:30%; height: 5%"
slimRightCell = A.style "float: right; width:68%; height: 5%"
leftCell      = A.style "float: left;  width:30%; height: 30%"
rightCell     = A.style "float: right; width:68%; height: 44%"
fillDiv       = A.style "width: 98%; height: 90%"
formDiv       = A.style "width: 100%; height: 600px"

-- Helper

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd  (a,b,c) = c
