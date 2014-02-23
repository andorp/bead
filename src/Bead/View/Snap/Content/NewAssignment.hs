{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  ) where

import Control.Monad.Error
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time as Time
import Data.String (fromString)
import qualified Data.ByteString.Char8 as BS

import Bead.Controller.Pages (Page)
import qualified Bead.Controller.Pages as P (Page(..))
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.RequestParams

import Text.Printf (printf)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A (id, style, href, target)
import qualified Text.Blaze.Html5 as H


-- * Content Handlers

newCourseAssignment :: Content
newCourseAssignment = getPostContentHandler newCourseAssignmentPage postCourseAssignment

newGroupAssignment :: Content
newGroupAssignment = getPostContentHandler newGroupAssignmentPage postGroupAssignment

modifyAssignment :: Content
modifyAssignment = getPostContentHandler modifyAssignmentPage postModifyAssignment

data PageData
  = PD_Course {
      pdTimeZone    :: Time.TimeZone
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    }
  | PD_Group {
      pdTimeZone    :: Time.TimeZone
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    }
  | PD_Assignment {
      pdTimeZone      :: Time.TimeZone
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    }
  -- TODO: Calculate the time differences and shows the values in
  -- the actual time zone

pageDataCata course group assignment p = case p of

  PD_Course timezone time courses tsType files ->
     course timezone time courses tsType files

  PD_Group  timezone time groups tsType files ->
     group  timezone time groups tsType files

  PD_Assignment timezone key asg tsType files testcase ->
     assignment timezone key asg tsType files testcase

const2 = const  . const
const3 = const2 . const
const4 = const3 . const
const5 = const4 . const
const6 = const5 . const

isEmptyData _ = False

-- * Course Assignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = withUserState $ \s -> do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  (c,tss, ufs) <- userStory $ do
    (course, _groupKeys) <- S.loadCourse ck
    tss' <- S.testScriptInfosOfCourse ck
    ufs  <- map fst <$> S.listUsersFiles
    return ((ck, course), nonEmptyList tss', ufs)
  now <- liftIO $ getCurrentTime
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Course tz now c tss ufs))

postCourseAssignment :: POSTContentHandler
postCourseAssignment = do
  CreateCourseAssignment
    <$> getParameter (customCourseKeyPrm (fieldName selectedCourse))
    <*> getValue -- assignment
    <*> readTCCreation

-- Tries to create a TCCreation descriptive value. If the test script, usersfile and testcase
-- parameters are included returns Just tccreation otherwise Nothing
readTCCreation :: HandlerError App b TCCreation
readTCCreation = do
  mTestScript         <- getOptionalParameter (readablePrm (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (readablePrm (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  case tcCreation mTestScript mZippedTestCaseName mPlainTestCase of
    Nothing -> throwError $ strMsg "Some error in test case parameters"
    Just tc -> return tc

tcCreation :: Maybe (Maybe TestScriptKey) -> Maybe UsersFile -> Maybe String -> Maybe TCCreation
tcCreation testScriptKey usersFile text
  | join testScriptKey == Nothing        = Just NoCreation
tcCreation (Just (Just tsk)) (Just uf) _ = Just $ FileCreation tsk uf
tcCreation (Just (Just tsk)) _ (Just t)  = Just $ TextCreation tsk t
tcCreation _ _ _                         = Nothing

readTCModification :: HandlerError App b TCModification
readTCModification = do
  mTestScript         <- getOptionalParameter (readablePrm (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (readablePrm (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  case tcModification mTestScript mZippedTestCaseName mPlainTestCase of
    Nothing -> throwError $ strMsg "Some error in test case parameters"
    Just tm -> return tm

tcModification :: Maybe (Maybe TestScriptKey) -> Maybe (Either () UsersFile) -> Maybe String -> Maybe TCModification
tcModification Nothing        _ _                    = Just NoModification
tcModification (Just Nothing) _ _                    = Just TCDelete
tcModification (Just (Just tsk)) (Just (Left ())) _  = Just NoModification
tcModification (Just (Just tsk)) (Just (Right uf)) _ = Just $ FileOverwrite tsk uf
tcModification (Just (Just tsk)) _ (Just t)          = Just $ TextOverwrite tsk t
tcModification _ _ _                                 = Nothing

-- * Group Assignment

newGroupAssignmentPage :: GETContentHandler
newGroupAssignmentPage = withUserState $ \s -> do
  now <- liftIO $ getCurrentTime
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  (g,tss,ufs) <- userStory $ do
    group <- S.loadGroup gk
    tss' <- S.testScriptInfosOfGroup gk
    ufs  <- map fst <$> S.listUsersFiles
    return ((gk, group), nonEmptyList tss', ufs)
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Group tz now g tss ufs))

postGroupAssignment :: POSTContentHandler
postGroupAssignment = do
  CreateGroupAssignment
  <$> getParameter (customGroupKeyPrm (fieldName selectedGroup))
  <*> getValue -- assignment
  <*> readTCCreation

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,ufs,tc) <- userStory $ do
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    return (as, nonEmptyList tss', ufs, tc)
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent (PD_Assignment tz ak as tss ufs tc))

postModifyAssignment :: POSTContentHandler
postModifyAssignment = do
  ModifyAssignment <$> getValue <*> getValue <*> readTCModification

-- * Helpers

-- | Returns Nothing if the given list was empty, otherwise Just list
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

-- * Page rendering

newAssignmentContent :: PageData -> IHtml
newAssignmentContent pd
  | isEmptyData pd = do
      msg <- getI18N
      return . H.p . fromString . msg $ pageDataCata
        (const5 $ Msg_NewAssignment_IsNoCourseAdmin "No courses are assigned to this user.")
        (const5 $ Msg_NewAssignment_IsNoGroupAdmin "No groups are assigned to this user.")
        (const6 $ Msg_NewAssignment_IsNoCreator "This assignment was created by somebody else.")
        pd
newAssignmentContent pd = do
  msg <- getI18N
  return $ do
    postForm (routeOf . page $ pd) `withId` (hookId assignmentForm) $ H.div ! formDiv $ do
    H.div ! slimRightCell $ do
      H.b $ (fromString . msg $ Msg_NewAssignment_Title "Title")
      textInput (fieldName assignmentNameField) 10 (amap assignmentName pd) ! fillDiv
      H.br
    H.div ! leftCell $ do
      H.b $ (fromString . msg $ Msg_NewAssignment_SubmissionDeadline "Visibility")
      H.div ! A.id (fieldName startDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_StartDate "Opens")
         (fromString $ concat [" (", Time.timeZoneName timezone, ")"])
         H.br
         hiddenInput (fieldName assignmentStartDefaultDate) (fromString startDefDate)
         hiddenInput (fieldName assignmentStartDefaultHour) (fromString startDefHour)
         hiddenInput (fieldName assignmentStartDefaultMin)  (fromString startDefMin)
         hiddenInput (fieldName assignmentStartField) (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"])
      H.div ! A.id (fieldName endDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_EndDate "Closes")
         (fromString $ concat [" (", Time.timeZoneName timezone, ")"])
         H.br
         hiddenInput (fieldName assignmentEndDefaultDate) (fromString endDefDate)
         hiddenInput (fieldName assignmentEndDefaultHour) (fromString endDefHour)
         hiddenInput (fieldName assignmentEndDefaultMin)  (fromString endDefMin)
         hiddenInput (fieldName assignmentEndField) (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"])
    H.div ! rightCell $ do
      H.br
      H.b $ (fromString . msg $ Msg_NewAssignment_Description "Description")
      textAreaInput (fieldName assignmentDescField) (amap assignmentDesc pd) ! fillDiv
      H.a ! A.href linkToPandocMarkdown ! A.target "_blank" $ (fromString . msg $ Msg_NewAssignment_Markdown "Markdown syntax")
      (fromString . msg $ Msg_NewAssignment_CanBeUsed " may be used for formatting.")
      H.p $ do
        H.b (fromString . msg $ Msg_NewAssignment_Title_Normal "Normal")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_Normal $ concat
          [ "Solutions may be submitted from the time of opening until the time of closing.  The assignment will not "
          , "be visible until it is opened.  The assignments open and close automatically."
          ]
      H.p $ do
        H.b (fromString . msg $ Msg_NewAssignment_Title_Urn "Urn")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_Urn $ concat
          [ "(Recommended for tests.)  Solutions may be submitted from the time of opening until the time of closing, "
          , "but the submissions and the corresponding evaluations will be visible by the students until it is closed.  "
          , "The assignment will not be visible until it is opened and they open and close automatically."
          ]
      testCaseArea msg pd
    H.div ! leftCell $ do
      H.b (fromString . msg $ Msg_NewAssignment_Type "Type")
      H.br
      defEnumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType $ pd)
      H.br
      H.p $ do
        H.b $ pageDataCata (const5 . fromString . msg $ Msg_NewAssignment_Course "Course")
                           (const5 . fromString . msg $ Msg_NewAssignment_Group "Group")
                           (const6 $ "")
                           pd
        H.br
        courseOrGroupName pd
        hiddenKeyField pd
        testScriptSelection msg pd
      H.p $ submitButton (fieldName saveSubmitBtn) (fromString . msg $ Msg_NewAssignment_SaveButton "Commit")

    where
      linkToPandocMarkdown = "http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html"

      page :: PageData -> Page
      page = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> P.NewCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs -> P.NewGroupAssignment key)
        (const6 P.ModifyAssignment)

      amap :: (Assignment -> a) -> PageData -> Maybe a
      amap f (PD_Assignment _ _ a _ _ _) = Just . f $ a
      amap _ _                           = Nothing

      courseOrGroupName = pageDataCata
        (\_tz _t (_key,course) _tsType _fs -> fromString $ courseName course)
        (\_tz _t (_key,group)  _tsType _fs -> fromString $ groupName group)
        (const6 (return ()))

      hiddenKeyField = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
        (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
        (\_tz key _asg _tsType _fs _tc -> hiddenInput (fieldName assignmentKeyField) $ paramValue key)

      testCaseArea msg = pageDataCata
        (\_tz _t _c tsType fs -> createTestCaseArea fs tsType)
        (\_tz _t _g tsType fs -> createTestCaseArea fs tsType)
        (\_tz _k _a tsType fs tc -> overwriteTestCaseArea fs tsType tc)
        where
          textArea val = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestCase "Test cases"
            textAreaInput (fieldName assignmentTestCaseField) val ! fillDiv

          createTestCaseArea fs ts = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where
              selectionOrTextArea = testScriptTypeCata
                (textArea Nothing)
                usersFileSelection

              usersFileSelection = do
                H.b . fromString . msg $ Msg_NewAssignment_SelectATestCaseFile "Please select a test case file"
                H.br
                valueSelection keyValue (fieldName assignmentUsersFileField) fs
                where
                  keyValue uf = flip usersFileCata uf $ \u -> (show uf, u)

          overwriteTestCaseArea fs ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where
              testCaseText Nothing = Nothing
              testCaseText (Just (_,tc',_)) = Just . BS.unpack $ tcValue tc'

              testCaseFileName Nothing = return ()
              testCaseFileName (Just (_,tc',_)) = fromString $ tcInfo tc'

              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc)) -- simple
                usersFileSelection           -- zipped

              usersFileSelection = do
                H.b . fromString . msg $ Msg_NewAssignment_SelectATestCaseFile "Please select a test case file"
                fromString . msg $ Msg_NewAssignment_ActualTestFileIs "Actually test case file is:"
                testCaseFileName tc
                H.br
                valueSelection keyValue (fieldName assignmentUsersFileField) ((Left ()):map Right fs)
                where
                  keyValue l@(Left ()) = (show l, msg $ Msg_NewAssignment_DoNotOverwrite "Do not overwrite")
                  keyValue r@(Right uf) = flip usersFileCata uf $ \u -> (show r, u)

      -- TODO
      testScriptSelection :: (Translation String -> String) -> PageData -> H.Html
      testScriptSelection msg = pageDataCata
        (\_tz _t _c tsType _fs -> scriptSelection tsType)
        (\_tz _t _g tsType _fs -> scriptSelection tsType)
        (\_tz _k _a tsType _fs mts -> modificationScriptSelection tsType mts)
        where
          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
            H.br
            valueSelection keyValue (fieldName assignmentTestScriptField) (Nothing:map Just ts)

          modificationScriptSelection ts mts = maybe
            (return ())
            (mtsSelection mts)
            ts

          mtsSelection mts ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
            H.br
            valueSelectionWithDefault
              keyValue (fieldName assignmentTestScriptField) (def mts) (Nothing:map Just ts)
            where
              def Nothing Nothing = True
              def Nothing _       = False
              def (Just (_,_,tsk)) (Just (tsk',_)) = tsk == tsk'
              def _                _               = False

          keyValue :: Maybe (TestScriptKey, TestScriptInfo) -> (String, String)
          keyValue Nothing = (show nothing, msg $ Msg_NewAssignment_NoTesting "Assignment without testing")
          keyValue (Just (testScriptKey, tsInfo)) = (show (Just testScriptKey), tsiName tsInfo)

          nothing = Nothing :: Maybe TestScriptKey


      timezone = pageDataCata
        (\tz _t _c _ts _fs -> tz)
        (\tz _t _g _ts _fs -> tz)
        (\tz _k _a _ts _fs _tc -> tz)
        pd

      date t =
        let localTime = Time.utcToLocalTime timezone t
            timeOfDay = Time.localTimeOfDay localTime
        in ( show $ Time.localDay         localTime
           , printf "%02d" $ Time.todHour timeOfDay
           , printf "%02d" $ Time.todMin  timeOfDay
           )

      (startDefDate, startDefHour, startDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc -> assignmentStart a)
        pd

      (endDefDate, endDefHour, endDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc -> assignmentEnd a)
        pd

      testScriptType' Nothing   = Nothing
      testScriptType' (Just []) = Nothing
      testScriptType' (Just ((_tk,tsi):_)) = Just $ tsiType tsi

-- CSS Section

slimLeftCell  = A.style "float: left;  width:30%; height: 5%"
slimRightCell = A.style "float: right; width:68%; height: 5%"
leftCell      = A.style "float: left;  width:30%; height: 30%"
rightCell     = A.style "float: right; width:68%; height: 44%"
fillDiv       = A.style "width: 98%; height: 90%"
formDiv       = A.style "width: 100%; height: 600px"

-- Helper

