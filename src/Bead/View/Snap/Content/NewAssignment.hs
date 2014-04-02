{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewAssignment (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  , viewAssignment
  , newGroupAssignmentPreview
  , newCourseAssignmentPreview
  , modifyAssignmentPreview
  ) where

import           Control.Monad.Error
import qualified Data.ByteString.UTF8 as BsUTF8
import qualified Data.Map as Map
import           Data.String (fromString)
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time as Time

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import           Text.Printf (printf)

import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.View.Snap.Content
import           Bead.View.Snap.Markdown
import           Bead.View.Snap.RequestParams



-- * Content Handlers

newCourseAssignment = ViewModifyHandler newCourseAssignmentPage postCourseAssignment
newGroupAssignment  = ViewModifyHandler newGroupAssignmentPage postGroupAssignment
modifyAssignment    = ViewModifyHandler modifyAssignmentPage postModifyAssignment
viewAssignment      = ViewHandler viewAssignmentPage
newCourseAssignmentPreview = UserViewHandler newCourseAssignmentPreviewPage
newGroupAssignmentPreview  = UserViewHandler newGroupAssignmentPreviewPage
modifyAssignmentPreview    = UserViewHandler modifyAssignmentPreviewPage

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
  | PD_Course_Preview {
      pdTimeZone    :: Time.TimeZone
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Group_Preview {
      pdTimeZone    :: Time.TimeZone
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Assignment_Preview {
      pdTimeZone      :: Time.TimeZone
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    , pdTCModificationPreview :: TCModificationParameters
    }
  -- TODO: Calculate the time differences and shows the values in
  -- the actual time zone

type TCModificationParameters = (Maybe (Maybe TestScriptKey), Maybe (Either () UsersFile), Maybe String)

tcmpTextTestCase (_,_,t) = t
tcmpFileTestCase (_,t,_) = t
tcmpTestScriptKey (k,_,_) = k

type TCCreationParameters = (Maybe (Maybe TestScriptKey), Maybe UsersFile, Maybe String)

tccTestScriptKey (k,_,_) = k
tccFileTestCase  (_,t,_) = t
tccTextTestCase  (_,_,t) = t

pageDataCata
  course
  group
  assignment
  viewAssignment
  coursePreview
  groupPreview
  assignmentPreview
  p = case p of

  PD_Course timezone time courses tsType files ->
     course timezone time courses tsType files

  PD_Group  timezone time groups tsType files ->
     group  timezone time groups tsType files

  PD_Assignment timezone key asg tsType files testcase ->
     assignment timezone key asg tsType files testcase

  PD_ViewAssignment timezone key asg tsInfo testcase ->
     viewAssignment timezone key asg tsInfo testcase

  PD_Course_Preview timezone time courses tsType files assignment tccreation ->
     coursePreview  timezone time courses tsType files assignment tccreation

  PD_Group_Preview timezone time groups tsType files assignment tccreation ->
     groupPreview  timezone time groups tsType files assignment tccreation

  PD_Assignment_Preview timezone key asg tsType files testcase tcmod ->
     assignmentPreview  timezone key asg tsType files testcase tcmod

const2 = const  . const
const3 = const2 . const
const4 = const3 . const
const5 = const4 . const
const6 = const5 . const
const7 = const6 . const

-- * Course Assignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = withUserState $ \s -> do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  (c, tss, ufs) <- userStory $ do
    S.isAdministratedCourse ck
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

newCourseAssignmentPreviewPage :: ViewPOSTContentHandler
newCourseAssignmentPreviewPage = withUserState $ \s -> do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  assignment <- getValue
  tc <- readTCCreationParameters
  (c, tss, ufs) <- userStory $ do
    S.isAdministratedCourse ck
    (course, _groupKeys) <- S.loadCourse ck
    tss' <- S.testScriptInfosOfCourse ck
    ufs  <- map fst <$> S.listUsersFiles
    return ((ck, course), nonEmptyList tss', ufs)
  now <- liftIO $ getCurrentTime
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet . withUserFrame s . newAssignmentContent $
    PD_Course_Preview tz now c tss ufs assignment tc

-- Tries to create a TCCreation descriptive value. If the test script, usersfile and testcase
-- parameters are included returns Just tccreation otherwise Nothing
readTCCreation :: HandlerError App b TCCreation
readTCCreation = do
  (mTestScript, mZippedTestCaseName, mPlainTestCase) <- readTCCreationParameters
  case tcCreation mTestScript mZippedTestCaseName mPlainTestCase of
    Left  e  -> throwError . strMsg $ "Some error in test case parameters " ++ e
    Right tc -> return tc

readTCCreationParameters :: HandlerError App b TCCreationParameters
readTCCreationParameters = do
  mTestScript         <- getOptionalParameter (readablePrm (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (readablePrm (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  return (mTestScript, mZippedTestCaseName, mPlainTestCase)

tcCreation :: Maybe (Maybe TestScriptKey) -> Maybe UsersFile -> Maybe String -> Either String TCCreation
tcCreation testScriptKey usersFile text
  | join testScriptKey == Nothing        = Right NoCreation
tcCreation (Just (Just tsk)) (Just uf) _ = Right $ FileCreation tsk uf
tcCreation (Just (Just tsk)) _ (Just t)  = Right $ TextCreation tsk t
tcCreation (Just (Just tsk)) Nothing Nothing = Left "#1"

readTCModificationParameters :: HandlerError App b TCModificationParameters
readTCModificationParameters = do
  mTestScript         <- getOptionalParameter (readablePrm (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (readablePrm (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  return (mTestScript,mZippedTestCaseName,mPlainTestCase)

readTCModification :: HandlerError App b TCModification
readTCModification = do
  (mTestScript,mZippedTestCaseName,mPlainTestCase) <- readTCModificationParameters
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
    S.isAdministratedGroup gk
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

newGroupAssignmentPreviewPage :: ViewPOSTContentHandler
newGroupAssignmentPreviewPage = withUserState $ \s -> do
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  assignment <- getValue
  tc <- readTCCreationParameters
  (g,tss,ufs) <- userStory $ do
    S.isAdministratedGroup gk
    group <- S.loadGroup gk
    tss' <- S.testScriptInfosOfGroup gk
    ufs  <- map fst <$> S.listUsersFiles
    return ((gk, group), nonEmptyList tss', ufs)
  tz <- dataTimeZone <$> userTimeZone
  now <- liftIO $ getCurrentTime
  renderDynamicPagelet $ withUserFrame s . newAssignmentContent $
    PD_Group_Preview tz now g tss ufs assignment tc

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,ufs,tc) <- userStory $ do
    S.isAdministratedAssignment ak
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

modifyAssignmentPreviewPage :: ViewPOSTContentHandler
modifyAssignmentPreviewPage = withUserState $ \s -> do
  ak <- getValue
  as <- getValue
  tm <- readTCModificationParameters
  (tss,ufs,tc) <- userStory $ do
    S.isAdministratedAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    return (nonEmptyList tss', ufs, tc)
  tz <- dataTimeZone <$> userTimeZone
  renderDynamicPagelet . withUserFrame s . newAssignmentContent $
    PD_Assignment_Preview tz ak as tss ufs tc tm

viewAssignmentPage :: GETContentHandler
viewAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,tc) <- userStory $ do
    S.isAdministratedAssignment ak
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
  return $ H.div ! formDiv $ do
    H.div ! rightCell $ do
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_Title "Title"
      editOrReadonly pd $ textInput (fieldName assignmentNameField) 10 (amap assignmentName
        (Just . fromString . msg $ Msg_NewAssignment_Title_Default "Unnamed Assignment") pd)
        ! fillLine ! A.required "" ! asgField
      H.br
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_Description "Description"
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
          ]) pd) ! fillDiv ! A.required "" ! asgField
      previewDiv msg pd
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
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_SubmissionDeadline "Visibility"
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
      typeSelection msg pd
      H.br
      H.p $ do
        H.span ! boldText $
              pageDataCata
                (const5 . fromString . msg $ Msg_NewAssignment_Course "Course")
                (const5 . fromString . msg $ Msg_NewAssignment_Group "Group")
                (const6 $ "")
                (const5 $ "")
                (const7 . fromString . msg $ Msg_NewAssignment_Course "Course")
                (const7 . fromString . msg $ Msg_NewAssignment_Group "Group")
                (const7 $ "")
                pd
        H.br
        courseOrGroupName pd
        hiddenKeyField pd
        testScriptSelection msg pd

        multiActionPostForm (hookId assignmentForm)
          [ ((routeOf . pagePreview $ pd), (fromString . msg $ Msg_NewAssignment_PreviewButton "Preview"))
          , ((routeOf . page $ pd),        (fromString . msg $ Msg_NewAssignment_SaveButton "Commit"))
          ] empty

    where
      asgField = A.form (fromString $ hookId assignmentForm)

      typeSelection msg pd = do
        H.b (fromString . msg $ Msg_NewAssignment_Type "Type")
        H.br
        pageDataCata
          (const5 ts)
          (const5 ts)
          (const6 ts)
          (\_tz _k a _ts _tc -> fromString . show $ assignmentType a)
          (const7 ts)
          (const7 ts)
          (const7 ts)
          pd
        where
          ts = defEnumSelection (fieldName assignmentTypeField) (maybe Normal id . amap assignmentType Nothing $ pd) ! asgField

      editOrReadonly = pageDataCata
        (const5 id)
        (const5 id)
        (const6 id)
        (const5 (! A.readonly ""))
        (const7 id)
        (const7 id)
        (const7 id)

      linkToPandocMarkdown = "http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html"

      pagePreview :: PageData -> PageDesc
      pagePreview = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase -> modifyAssignmentPreview key)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase _tc -> modifyAssignmentPreview key)

      page :: PageData -> PageDesc
      page = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignment key)
        (\_tz ak _asg _tsType _files _testcase -> modifyAssignment ak)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignment key)
        (\_tz k _a _fs _ts _tc _tm -> modifyAssignment k)

      newCourseAssignment k = Pages.newCourseAssignment k ()
      newGroupAssignment k  = Pages.newGroupAssignment k ()
      modifyAssignment k    = Pages.modifyAssignment k ()
      newCourseAssignmentPreview k = Pages.newCourseAssignmentPreview k ()
      newGroupAssignmentPreview k  = Pages.newGroupAssignmentPreview k ()
      modifyAssignmentPreview k    = Pages.modifyAssignmentPreview k ()
      viewAssignment k = Pages.viewAssignment k ()
      uploadFile = Pages.uploadFile ()

      amap :: (Assignment -> a) -> Maybe a -> PageData -> Maybe a
      amap f _ (PD_Assignment _ _ a _ _ _)           = Just . f $ a
      amap f _ (PD_Assignment_Preview _ _ a _ _ _ _) = Just . f $ a
      amap f _ (PD_ViewAssignment _ _ a _ _)     = Just . f $ a
      amap f _ (PD_Course_Preview _ _ _ _ _ a _) = Just . f $ a
      amap f _ (PD_Group_Preview  _ _ _ _ _ a _) = Just . f $ a
      amap _ def _ = def

      previewDiv msg = pageDataCata
        (const5 empty)
        (const5 empty)
        (const6 empty)
        (const5 empty)
        (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
        (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
        (\_timezone _key asg _tst _files _testcase _tm -> assignmentPreview asg)
        where
         assignmentPreview a = H.div $ do
           H.h3 $ fromString . msg $ Msg_NewAssignment_AssignmentPreview "Assignment Preview"
           H.div # assignmentTextDiv $ markdownToHtml $ assignmentDesc a

      startTimePart = pageDataCata
        (const5 hiddenStartTime)
        (const5 hiddenStartTime)
        (const6 hiddenStartTime)
        (const5 (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"]))
        (const7 hiddenStartTime)
        (const7 hiddenStartTime)
        (const7 hiddenStartTime)

      hiddenStartTime = do
        hiddenInput (fieldName assignmentStartDefaultDate) (fromString startDefDate) ! asgField
        hiddenInput (fieldName assignmentStartDefaultHour) (fromString startDefHour) ! asgField
        hiddenInput (fieldName assignmentStartDefaultMin)  (fromString startDefMin)  ! asgField
        hiddenInput (fieldName assignmentStartField) (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"]) ! asgField
        hiddenInput (fieldName assignmentStartDefaultDate) (fromString startDefDate) ! asgField
        hiddenInput (fieldName assignmentStartDefaultHour) (fromString startDefHour) ! asgField
        hiddenInput (fieldName assignmentStartDefaultMin)  (fromString startDefMin)  ! asgField

      endTimePart = pageDataCata
        (const5 hiddenEndTime)
        (const5 hiddenEndTime)
        (const6 hiddenEndTime)
        (const5 (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"]))
        (const7 hiddenEndTime)
        (const7 hiddenEndTime)
        (const7 hiddenEndTime)

      hiddenEndTime = do
        hiddenInput (fieldName assignmentEndDefaultDate) (fromString endDefDate) ! asgField
        hiddenInput (fieldName assignmentEndDefaultHour) (fromString endDefHour) ! asgField
        hiddenInput (fieldName assignmentEndDefaultMin)  (fromString endDefMin)  ! asgField
        hiddenInput (fieldName assignmentEndField) (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"]) ! asgField
        hiddenInput (fieldName assignmentEndDefaultDate) (fromString endDefDate) ! asgField
        hiddenInput (fieldName assignmentEndDefaultHour) (fromString endDefHour) ! asgField
        hiddenInput (fieldName assignmentEndDefaultMin)  (fromString endDefMin)  ! asgField

      courseOrGroupName = pageDataCata
        (\_tz _t (_key,course) _tsType _fs -> fromString $ courseName course)
        (\_tz _t (_key,group)  _tsType _fs -> fromString $ groupName group)
        (const6 (return ()))
        (const5 (return ()))
        (\_tz _t (_key,course) _tsType _fs _a _tc -> fromString $ courseName course)
        (\_tz _t (_key,group)  _tsType _fs _a _tc -> fromString $ groupName group)
        (const7 (return ()))

      hiddenKeyField = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key) ! asgField)
        (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key) ! asgField)
        (const6 (return ()))
        (const5 (return ()))
        (\_tz _t (key,_course) _tsType _fs _a _tc -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key) ! asgField)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key) ! asgField)
        (const7 (return ()))

      testCaseArea msg = pageDataCata
        (\_tz _t _c tsType fs -> createTestCaseArea fs tsType)
        (\_tz _t _g tsType fs -> createTestCaseArea fs tsType)
        (\_tz _k _a tsType fs tc -> overwriteTestCaseArea fs tsType tc)
        (\_tz _k _a ts tc -> viewTestCaseArea ts tc)
        (\_tz _t _c tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _t _g tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _k _a tsType fs tc tm -> overwriteTestCaseAreaPreview fs tsType tc tm)
        where
          textArea val = do
            H.b . fromString . msg $ Msg_NewAssignment_TestCase "Test cases"
            editOrReadonly pd $ textAreaInput (fieldName assignmentTestCaseField) val ! fillDiv ! asgField

          createTestCaseAreaPreview fs ts tcp = case tcp of
            (Just Nothing , Nothing, Nothing) -> createTestCaseArea fs ts
            (Just _       , Just uf, Nothing) -> userFileSelection uf
            (Just _       , Nothing,  Just f) -> textAreaPreview f
            _ -> return ()
            where
              userFileSelection uf = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.br
                valueSelectionWithDefault keyValue (fieldName assignmentUsersFileField) (==uf) fs ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue uf = flip usersFileCata uf $ \u -> (show uf, u)

              textAreaPreview f = textArea (Just f)

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
                valueSelection keyValue (fieldName assignmentUsersFileField) fs ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
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

          overwriteTestCaseAreaPreview fs ts tc tm = maybe
            (return ())
            (selectionOrTextAreaPreview)
            (testScriptType' ts)
            where

              selectionOrTextAreaPreview = testScriptTypeCata
                (textArea (tcmpTextTestCase tm)) -- simple
                (maybe (return ()) userFileSelectionPreview (tcmpFileTestCase tm)) -- zipped

              userFileSelectionPreview uf = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc
                valueSelectionWithDefault keyValue (fieldName assignmentUsersFileField) (uf==) ((Left ()):map Right fs) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (show l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = flip usersFileCata uf $ \u -> (show r, u)


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
                valueSelection keyValue (fieldName assignmentUsersFileField) ((Left ()):map Right fs) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
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
        (\_tz _t _c tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _t _g tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _k _a tsType _fs mts tm -> modificationScriptSelectionPreview tsType mts tm)
        where
          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            H.br
            valueSelection keyValue (fieldName assignmentTestScriptField) (Nothing:map Just ts) ! asgField

          scriptSelectionPreview ts tcp = case tcp of
            (Just Nothing    , _, _) -> scriptSelection ts
            (Just (Just tsk) , _, _) -> preview ts tsk
            _ -> return ()
            where
              preview ts tsk = maybe (return ()) (tsSelectionPreview tsk) ts

          tsSelectionPreview tsk ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            H.br
            valueSelectionWithDefault
              keyValue
              (fieldName assignmentTestScriptField)
              (\ts' -> (Just tsk)==(fmap fst ts'))
              (Nothing:map Just ts) ! asgField

          modificationScriptSelection ts mts = maybe
            (return ())
            (mtsSelection mts)
            ts

          mtsSelection mts ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
            H.br
            valueSelectionWithDefault
              keyValue (fieldName assignmentTestScriptField) (def mts) (Nothing:map Just ts) ! asgField
            where
              def Nothing Nothing = True
              def Nothing _       = False
              def (Just (_,_,tsk)) (Just (tsk',_)) = tsk == tsk'
              def _                _               = False

          modificationScriptSelectionPreview ts mts tm =
            case (tcmpTestScriptKey tm, ts) of
              (Just Nothing   , Just ts') -> mtsSelection' Nothing ts'
              (Just (Just tsk), Just ts') -> mtsSelection' (Just tsk) ts'
              _                           -> return ()
            where
              mtsSelection' tsk ts = do
                H.br
                H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
                H.br
                valueSelectionWithDefault
                  keyValue (fieldName assignmentTestScriptField) (def tsk) (Nothing:map Just ts) ! asgField
                where
                  def Nothing Nothing = True
                  def Nothing _       = False
                  def (Just tsk) (Just (tsk',_)) = tsk == tsk'
                  def _                _         = False


          keyValue :: Maybe (TestScriptKey, TestScriptInfo) -> (String, String)
          keyValue Nothing = (show nothing, msg $ Msg_NewAssignment_NoTesting "Assignment without testing")
          keyValue (Just (testScriptKey, tsInfo)) = (show (Just testScriptKey), tsiName tsInfo)

          nothing = Nothing :: Maybe TestScriptKey


      timezone = pageDataCata
        (\tz _t _c _ts _fs -> tz)
        (\tz _t _g _ts _fs -> tz)
        (\tz _k _a _ts _fs _tc -> tz)
        (\tz _k _a _ts _tc -> tz)
        (\tz _t _c _ts _fs _a _tc  -> tz)
        (\tz _t _g _ts _fs _a _tc  -> tz)
        (\tz _k _a _ts _fs _tc _tm -> tz)
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
        (\_tz _t _c _ts _fs a _tc  -> assignmentStart a)
        (\_tz _t _g _ts _fs a _tc  -> assignmentStart a)
        (\_tz _k a _ts _fs _tc _tm -> assignmentStart a)
        pd

      (endDefDate, endDefHour, endDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc -> assignmentEnd a)
        (\_tz _k a _ts _tc -> assignmentEnd a)
        (\_tz _t _c _ts _fs a _tc  -> assignmentEnd a)
        (\_tz _t _g _ts _fs a _tc  -> assignmentEnd a)
        (\_tz _k a _ts _fs _tc _tm -> assignmentEnd a)
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
fillDiv       = A.style "width: 99%; height: 99%"
fillLine      = A.style "width: 99%"
formDiv       = A.style "width: 100%; height: 600px"
boldText      = A.style "font-weight: bold"

-- Helper

