{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  , viewAssignment
  ) where

import Control.Monad.Error
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time as Time
import Data.String (fromString)
import qualified Data.ByteString.UTF8 as BsUTF8
import qualified Data.Map as Map

import Bead.Controller.Pages (Page)
import qualified Bead.Controller.Pages as P (Page(..))
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Pagelets
import Bead.View.Snap.Content
import Bead.View.Snap.RequestParams

import Text.Printf (printf)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
  (id, style, href, target, required, readonly)
import qualified Text.Blaze.Html5 as H


-- * Content Handlers

newCourseAssignment :: Content
newCourseAssignment = getPostContentHandler newCourseAssignmentPage postCourseAssignment

newGroupAssignment :: Content
newGroupAssignment = getPostContentHandler newGroupAssignmentPage postGroupAssignment

modifyAssignment :: Content
modifyAssignment = getPostContentHandler modifyAssignmentPage postModifyAssignment

viewAssignment :: Content
viewAssignment = getContentHandler viewAssignmentPage

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
  | PD_ViewAssignment {
      pdTimeZone      :: Time.TimeZone
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScript    :: Maybe TestScriptInfo
    , pdTestCaseInfo  :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    }
  -- TODO: Calculate the time differences and shows the values in
  -- the actual time zone

pageDataCata course group assignment viewAssignment p = case p of

  PD_Course timezone time courses tsType files ->
     course timezone time courses tsType files

  PD_Group  timezone time groups tsType files ->
     group  timezone time groups tsType files

  PD_Assignment timezone key asg tsType files testcase ->
     assignment timezone key asg tsType files testcase

  PD_ViewAssignment timezone key asg tsInfo testcase ->
     viewAssignment timezone key asg tsInfo testcase

const2 = const  . const
const3 = const2 . const
const4 = const3 . const
const5 = const4 . const
const6 = const5 . const

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

viewAssignmentPage :: GETContentHandler
viewAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,tc) <- userStory $ do
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ts   <- S.testCaseOfAssignment ak
    return (as, tss', ts)
  tz <- dataTimeZone <$> userTimeZone
  let ti = do (_tck, _tc, tsk) <- tc
              Map.lookup tsk $ Map.fromList tss
  renderPagelet $ withUserFrame s (newAssignmentContent (PD_ViewAssignment tz ak as ti tc))

-- * Helpers

-- | Returns Nothing if the given list was empty, otherwise Just list
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

-- * Page rendering

newAssignmentContent :: PageData -> IHtml
newAssignmentContent pd = do
  msg <- getI18N
  return $ do
    postForm (routeOf . page $ pd) `withId` (hookId assignmentForm) $ H.div ! formDiv $ do
    H.div ! slimRightCell $ do
      H.b $ (fromString . msg $ Msg_NewAssignment_Title "Title")
      editOrReadonly pd $ textInput (fieldName assignmentNameField) 10 (amap assignmentName
        (Just . fromString . msg $ Msg_NewAssignment_Title_Default "Unnamed Assignment") pd)
        ! fillDiv ! A.required ""
      H.br
    H.div ! leftCell $ do
      H.b $ (fromString . msg $ Msg_NewAssignment_SubmissionDeadline "Visibility")
      H.div ! A.id (fieldName startDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_StartDate "Opens")
         (fromString $ concat [" (", Time.timeZoneName timezone, ")"])
         H.br
         startTimePart pd
      H.div ! A.id (fieldName endDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_EndDate "Closes")
         (fromString $ concat [" (", Time.timeZoneName timezone, ")"])
         H.br
         endTimePart pd
    H.div ! rightCell $ do
      H.br
      H.b $ (fromString . msg $ Msg_NewAssignment_Description "Description")
      editOrReadonly pd $ textAreaInput (fieldName assignmentDescField) (amap assignmentDesc (Just . fromString . msg $
        Msg_NewAssignment_Description_Default $ unlines
          [ concat
             [ "This text shall be in markdown format.  Here are some quick "
             , "examples:"
             ]
          , ""
          , "  - This is a bullet list item with *emphasis* (italic)."
          , "  - And this is another item in the list with "
          , "    **strong** (bold). Note that the rest of the item"
          , "    shall be aligned."
          , ""
          , concat
              [ "Sometimes one may want to write verbatim text, this how it can "
              , "be done.  However, `verbatim` words may be inlined any time by "
              , "using the backtick (`` ` ``) symbol."
              ]
          , ""
          , "~~~~~"
          , "verbatim text"
          , "~~~~~"
          , ""
          , concat
              [ "Note that links may be also [inserted](http://haskell.org/). And "
              , "when everything else fails, <a>pure</a> <b>HTML code</b> "
              , "<i>may be embedded</i>."
              ]
          ]) pd) ! fillDiv ! A.required ""
      H.a ! A.href linkToPandocMarkdown ! A.target "_blank" $ (fromString . msg $ Msg_NewAssignment_Markdown "Markdown syntax")
      (fromString . msg $ Msg_NewAssignment_CanBeUsed " is recommended for study to learn more about formatting.")
      H.p $ do
        H.i (fromString . msg $ Msg_NewAssignment_Title_Normal "Normal")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_Normal $ concat
          [ "Solutions may be submitted from the time of opening until the time of closing.  The assignment will not "
          , "be visible until it is opened.  The assignments open and close automatically."
          ]
      H.p $ do
        H.i (fromString . msg $ Msg_NewAssignment_Title_Urn "Urn")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_Urn $ concat
          [ "(Recommended for tests.)  Solutions may be submitted from the time of opening until the time of closing, "
          , "but the submissions and the corresponding evaluations will be visible by the students until it is closed.  "
          , "The assignment will not be visible until it is opened and they open and close automatically."
          ]
      testCaseArea msg pd
    H.div ! leftCell $ do
      typeSelection msg pd
      H.br
      H.p $ do
        H.b $ pageDataCata (const5 . fromString . msg $ Msg_NewAssignment_Course "Course")
                           (const5 . fromString . msg $ Msg_NewAssignment_Group "Group")
                           (const6 $ "")
                           (const5 $ "")
                           pd
        H.br
        courseOrGroupName pd
        hiddenKeyField pd
        testScriptSelection msg pd
      submit msg pd
    where
      button msg = H.p $ submitButton (fieldName saveSubmitBtn) (fromString . msg $ Msg_NewAssignment_SaveButton "Commit")

      submit msg = pageDataCata
        (const5 (button msg))
        (const5 (button msg))
        (const6 (button msg))
        (const5 (return ()))

      typeSelection msg pd = do
        H.b (fromString . msg $ Msg_NewAssignment_Type "Type")
        H.br
        pageDataCata
          (const5 ts)
          (const5 ts)
          (const6 ts)
          (\_tz _k a _ts _tc -> fromString . show $ assignmentType a)
          pd
        where
          ts = defEnumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType Nothing $ pd)

      editOrReadonly = pageDataCata (const5 id) (const5 id) (const6 id) (const5 (! A.readonly ""))

      linkToPandocMarkdown = "http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html"

      page :: PageData -> Page
      page = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> P.NewCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs -> P.NewGroupAssignment key)
        (const6 P.ModifyAssignment)
        (\_tz k _a _ts _tc -> P.ViewAssignment k)

      amap :: (Assignment -> a) -> Maybe a -> PageData -> Maybe a
      amap f _   (PD_Assignment _ _ a _ _ _) = Just . f $ a
      amap f _ (PD_ViewAssignment _ _ a _ _) = Just . f $ a
      amap _ def _                           = def

      startTimePart = pageDataCata
        (const5 hiddenStartTime)
        (const5 hiddenStartTime)
        (const6 hiddenStartTime)
        (const5 (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"]))

      hiddenStartTime = do
        hiddenInput (fieldName assignmentStartDefaultDate) (fromString startDefDate)
        hiddenInput (fieldName assignmentStartDefaultHour) (fromString startDefHour)
        hiddenInput (fieldName assignmentStartDefaultMin)  (fromString startDefMin)
        hiddenInput (fieldName assignmentStartField) (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"])

      endTimePart = pageDataCata
        (const5 hiddenEndTime)
        (const5 hiddenEndTime)
        (const6 hiddenEndTime)
        (const5 (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"]))

      hiddenEndTime = do
        hiddenInput (fieldName assignmentEndDefaultDate) (fromString endDefDate)
        hiddenInput (fieldName assignmentEndDefaultHour) (fromString endDefHour)
        hiddenInput (fieldName assignmentEndDefaultMin)  (fromString endDefMin)
        hiddenInput (fieldName assignmentEndField) (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"])

      courseOrGroupName = pageDataCata
        (\_tz _t (_key,course) _tsType _fs -> fromString $ courseName course)
        (\_tz _t (_key,group)  _tsType _fs -> fromString $ groupName group)
        (const6 (return ()))
        (const5 (return ()))

      hiddenKeyField = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
        (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
        (\_tz key _asg _tsType _fs _tc -> hiddenInput (fieldName assignmentKeyField) $ paramValue key)
        (const5 (return ()))

      testCaseArea msg = pageDataCata
        (\_tz _t _c tsType fs -> createTestCaseArea fs tsType)
        (\_tz _t _g tsType fs -> createTestCaseArea fs tsType)
        (\_tz _k _a tsType fs tc -> overwriteTestCaseArea fs tsType tc)
        (\_tz _k _a ts tc -> viewTestCaseArea ts tc)
        where
          textArea val = do
            H.b . fromString . msg $ Msg_NewAssignment_TestCase "Test cases"
            editOrReadonly pd $ textAreaInput (fieldName assignmentTestCaseField) val ! fillDiv

          createTestCaseArea fs ts = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where
              selectionOrTextArea = testScriptTypeCata
                (textArea Nothing)
                usersFileSelection

              usersFileSelection = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.br
                valueSelection keyValue (fieldName assignmentUsersFileField) fs
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank P.UploadFile
                where
                  keyValue uf = flip usersFileCata uf $ \u -> (show uf, u)

          testCaseText Nothing = Nothing
          testCaseText (Just (_,tc',_)) = Just . BsUTF8.toString $ tcValue tc'

          testCaseFileName Nothing = return ()
          testCaseFileName (Just (_,tc',_)) = fromString $ tcInfo tc'

          viewTestCaseArea ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType'' ts)
            where
              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc))
                (usersFile)

              usersFile = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc

          overwriteTestCaseArea fs ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where

              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc)) -- simple
                usersFileSelection           -- zipped

              usersFileSelection = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc
                valueSelection keyValue (fieldName assignmentUsersFileField) ((Left ()):map Right fs)
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank P.UploadFile
                where
                  keyValue l@(Left ()) = (show l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = flip usersFileCata uf $ \u -> (show r, u)

      -- TODO
      testScriptSelection :: (Translation String -> String) -> PageData -> H.Html
      testScriptSelection msg = pageDataCata
        (\_tz _t _c tsType _fs -> scriptSelection tsType)
        (\_tz _t _g tsType _fs -> scriptSelection tsType)
        (\_tz _k _a tsType _fs mts -> modificationScriptSelection tsType mts)
        (const5 (return ()))
        where
          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
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
        (\tz _k _a _ts _tc -> tz)
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
        (\_tz _k a _ts _tc -> assignmentStart a)
        pd

      (endDefDate, endDefHour, endDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc -> assignmentEnd a)
        (\_tz _k a _ts _tc -> assignmentEnd a)
        pd

      testScriptType' Nothing   = Nothing
      testScriptType' (Just []) = Nothing
      testScriptType' (Just ((_tk,tsi):_)) = Just $ tsiType tsi

      testScriptType'' = fmap tsiType

-- CSS Section

slimLeftCell  = A.style "float: left;  width:30%; height: 5%"
slimRightCell = A.style "float: right; width:68%; height: 5%"
leftCell      = A.style "float: left;  width:30%; height: 30%"
rightCell     = A.style "float: right; width:68%; height: 44%"
fillDiv       = A.style "width: 98%; height: 90%"
formDiv       = A.style "width: 100%; height: 600px"

-- Helper

