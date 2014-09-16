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

import           Control.Arrow ((&&&))
import           Control.Monad.Error
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BsLazy
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.Time as Time

import           Fay.Convert
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import           Text.Printf (printf)

import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Shared.Evaluation (binaryConfig, evConfigCata)
import           Bead.View.Snap.Fay.HookIds
import           Bead.View.Snap.Fay.Hooks
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
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    }
  | PD_Group {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    }
  | PD_Assignment {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    , pdEvalTypeMod   :: Bool -- True, the evalution type can be modified
    }
  | PD_ViewAssignment {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScript    :: Maybe TestScriptInfo
    , pdTestCaseInfo  :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    }
  | PD_Course_Preview {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Group_Preview {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Assignment_Preview {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    , pdTCModificationPreview :: TCModificationParameters
    , pdEvalTypeMod   :: Bool -- True, the evalution type can be modified
    }

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

  PD_Assignment timezone key asg tsType files testcase ev ->
     assignment timezone key asg tsType files testcase ev

  PD_ViewAssignment timezone key asg tsInfo testcase ->
     viewAssignment timezone key asg tsInfo testcase

  PD_Course_Preview timezone time courses tsType files assignment tccreation ->
     coursePreview  timezone time courses tsType files assignment tccreation

  PD_Group_Preview timezone time groups tsType files assignment tccreation ->
     groupPreview  timezone time groups tsType files assignment tccreation

  PD_Assignment_Preview timezone key asg tsType files testcase tcmod ev ->
     assignmentPreview  timezone key asg tsType files testcase tcmod ev


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
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent tn (PD_Course tz now c tss ufs))

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
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  renderDynamicPagelet . withUserFrame s . newAssignmentContent tn $
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
  mTestScript         <- getOptionalParameter (jsonParameter (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (jsonParameter (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  return (mTestScript, mZippedTestCaseName, mPlainTestCase)

tcCreation :: Maybe (Maybe TestScriptKey) -> Maybe UsersFile -> Maybe String -> Either String TCCreation
tcCreation Nothing        _ _ = Right NoCreation
tcCreation (Just Nothing) _ _ = Right NoCreation
tcCreation (Just (Just tsk)) (Just uf) _ = Right $ FileCreation tsk uf
tcCreation (Just (Just tsk)) _ (Just t)  = Right $ TextCreation tsk t
tcCreation (Just (Just _tsk)) Nothing Nothing = Left "#1"

readTCModificationParameters :: HandlerError App b TCModificationParameters
readTCModificationParameters = do
  mTestScript         <- getOptionalParameter (jsonParameter (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalParameter (jsonParameter (fieldName assignmentUsersFileField) "Test Script File")
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
tcModification (Just (Just _tsk)) (Just (Left ())) _  = Just NoModification
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
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent tn (PD_Group tz now g tss ufs))

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
  tz <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  tn <- userTimeZone
  renderDynamicPagelet $ withUserFrame s . newAssignmentContent tn $
    PD_Group_Preview tz now g tss ufs assignment tc

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,ufs,tc,ev) <- userStory $ do
    S.isAdministratedAssignment ak
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    ev   <- not <$> S.isThereASubmission ak
    return (as, nonEmptyList tss', ufs, tc, ev)
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  renderDynamicPagelet $ withUserFrame s (newAssignmentContent tn (PD_Assignment tz ak as tss ufs tc ev))

postModifyAssignment :: POSTContentHandler
postModifyAssignment = do
  ModifyAssignment <$> getValue <*> getValue <*> readTCModification

modifyAssignmentPreviewPage :: ViewPOSTContentHandler
modifyAssignmentPreviewPage = withUserState $ \s -> do
  ak <- getValue
  as <- getValue
  tm <- readTCModificationParameters
  (tss,ufs,tc,ev) <- userStory $ do
    S.isAdministratedAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    ev   <- not <$> S.isThereASubmission ak
    return (nonEmptyList tss', ufs, tc, ev)
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  renderDynamicPagelet . withUserFrame s . newAssignmentContent tn $
    PD_Assignment_Preview tz ak as tss ufs tc tm ev

viewAssignmentPage :: GETContentHandler
viewAssignmentPage = withUserState $ \s -> do
  ak <- getValue
  (as,tss,tc) <- userStory $ do
    S.isAdministratedAssignment ak
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ts   <- S.testCaseOfAssignment ak
    return (as, tss', ts)
  tz <- userTimeZoneToLocalTimeConverter
  tn <- userTimeZone
  let ti = do (_tck, _tc, tsk) <- tc
              Map.lookup tsk $ Map.fromList tss
  renderPagelet $ withUserFrame s (newAssignmentContent tn (PD_ViewAssignment tz ak as ti tc))

-- * Helpers

-- | Returns Nothing if the given list was empty, otherwise Just list
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

-- * Page rendering

newAssignmentContent :: TimeZoneName -> PageData -> IHtml
newAssignmentContent tz pd = do
  let hook = assignmentEvTypeHook
  msg <- getI18N

  -- Renders a evaluation selection or hides it if there is a submission already for the assignment,
  -- and renders an explanation.
  evalConfig <- do
    let cfg = evaluationConfig (evSelectionId hook)
    let hiddencfg asg = return $ do
          let e = Assignment.evType asg
          showEvaluationType msg e
          fromString . msg $ Msg_NewAssignment_EvalTypeWarn "The evaluation type can not be modified, there is a submission for the assignment."
          hiddenInput (evSelectionId hook) (toFayJSON e)
    pageDataCata
      (const5 cfg)
      (const5 cfg)
      (\_tz _key asg _ts _fs _tc ev -> if ev then cfg else hiddencfg asg)
      (const5 cfg)
      (const7 cfg)
      (const7 cfg)
      (\_tz _key asg _ts _fs _tc _tm ev -> if ev then cfg else hiddencfg asg)
      pd

  return $ H.div ! formDiv $ do
    H.div ! rightCell $ do
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_Title "Title"
      editOrReadonly pd $ textInput (fieldName assignmentNameField) 10 (amap Assignment.name
        (Just . fromString . msg $ Msg_NewAssignment_Title_Default "Unnamed Assignment") pd)
        ! fillLine ! A.required "" ! asgField
      H.br
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_Description "Description"
      editOrReadonly pd $ textAreaInput (fieldName assignmentDescField) (amap Assignment.desc (Just . fromString . msg $
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
        fromString . msg $ Msg_NewAssignment_Info_Normal $ concat
          [ "Solutions may be submitted from the time of opening until the time of closing. "
          , "The assignment will not be visible until it is opened. "
          , "The assignments open and close automatically."
          ]
      H.p $ do
        H.i (fromString . msg $ Msg_NewAssignment_Title_BallotBox "Ballot Box")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_BallotBox $ concat
          [ "(Recommended for tests.) Students will not be able to access submissions and "
          , "their evaluations until the assignment is closed."
          ]
      H.p $ do
        H.i (fromString . msg $ Msg_NewAssignment_Title_Password "Password-protected")
        ": "
        fromString . msg $ Msg_NewAssignment_Info_Password $ concat
          [ "(Recommended for tests.) Submissions may be only submitted by providing the password. "
          , "The teacher shall use the password during the test in order to authenticate the "
          , "submission for the student."
          ]
      testCaseArea msg pd
    H.div ! leftCell $ do
      H.span ! boldText $ fromString . msg $ Msg_NewAssignment_SubmissionDeadline "Visibility"
      H.div ! A.id (fieldName startDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_StartDate "Opens")
         H.br
         startTimePart pd
      H.div ! A.id (fieldName endDateDivId) $ do
         (fromString . msg $ Msg_NewAssignment_EndDate "Closes")
         H.br
         endTimePart pd
      typeSelection msg pd
      H.br
      H.p $ do
        H.span ! boldText $
              pageDataCata
                (const5 . fromString . msg $ Msg_NewAssignment_Course "Course")
                (const5 . fromString . msg $ Msg_NewAssignment_Group "Group")
                (const7 $ "")
                (const5 $ "")
                (const7 . fromString . msg $ Msg_NewAssignment_Course "Course")
                (const7 . fromString . msg $ Msg_NewAssignment_Group "Group")
                (const8 $ "")
                pd
        H.br
        courseOrGroupName pd
      H.p $ do
        submissionType msg
        selectionWithDefault (fieldName assignmentSubmissionTypeField) currentSubmissionType
          [ (txtSubmission, fromString . msg $ Msg_NewAssignment_TextSubmission "Text")
          , (zipSubmission, fromString . msg $ Msg_NewAssignment_ZipSubmission "Zip file")
          ] ! asgField
      H.p $ do
        hiddenKeyField pd
        testScriptSelection msg pd

        let previewAndCommitForm cfg =
              multiActionPostForm (hookId assignmentForm)
                [ ((routeOf . pagePreview $ pd), (fromString . msg $ Msg_NewAssignment_PreviewButton "Preview"))
                , ((routeOf . page $ pd),        (fromString . msg $ Msg_NewAssignment_SaveButton "Commit"))
                ] $ do evaluationType msg
                       evalSelectionDiv hook
                       -- TODO: Only allow modificiation of evaluation type until the first submission is submitted.
                       hiddenInputWithId (evHiddenValueId hook) (toFayJSON cfg)
                       evalConfig

        pageDataCata
          (const5 (previewAndCommitForm binaryConfig))
          (const5 (previewAndCommitForm binaryConfig))
          (\_timezone _key asg _tsType _files _testcase _ev -> previewAndCommitForm (Assignment.evType asg))
          (\_timezone _key asg _tsInfo _testcase -> showEvaluationType msg $ Assignment.evType asg)
          (\_timezone _time _courses _tsType _files assignment _tccreatio -> previewAndCommitForm (Assignment.evType assignment))
          (\_timezone _time _groups _tsType _files assignment _tccreation -> previewAndCommitForm (Assignment.evType assignment))
          (\_timezone _key asg _tsType _files _testcase _tcmod _ev -> previewAndCommitForm (Assignment.evType asg))
          pd
    where
      aas = fromMaybe Assignment.emptyAspects . amap Assignment.aspects Nothing $ pd

      -- Converts a given value to a string that represents a JSON acceptable string
      -- for the Fay client side
      toFayJSON = BsLazy.unpack . Aeson.encode . showToFay

      timeZoneString = timeZoneName id tz

      asgField = A.form (fromString $ hookId assignmentForm)


      typeSelection msg pd = do
        H.b (fromString . msg $ Msg_NewAssignment_Properties "Properties")
        H.br
        pageDataCata
          (const5 $ ts editable)
          (const5 $ ts editable)
          (const7 $ ts editable)
          (const5 $ ts readOnly)
          (const7 $ ts editable)
          (const7 $ ts editable)
          (const8 $ ts editable)
          pd
        where
          editable = True
          readOnly = False
          ts ed = H.div $ do
                 let pwd = if Assignment.isPasswordProtected aas
                             then Just (Assignment.getPassword aas)
                             else Nothing
                     editable x = if ed then x else (x ! A.readonly "")
                 editable $ checkBox' (fieldName assignmentAspectField)
                           (Assignment.isBallotBox aas)
                           Assignment.BallotBox (msg $ Msg_NewAssignment_BallotBox "Ballot Box") ! asgField
                 H.br
                 editable $ checkBox' (fieldName assignmentAspectField)
                           (Assignment.isPasswordProtected aas)
                           (Assignment.Password "")
                           (msg $ Msg_NewAssignment_PasswordProtected "Password-protected") ! asgField
                 H.br
                 fromString . msg $ Msg_NewAssignment_Password "Password:"
                 editable $ textInput (fieldName assignmentPwdField) 20 pwd ! asgField

      evaluationType msg = do
        H.b (fromString . msg $ Msg_NewAssignment_EvaluationType "Evaluation Type")

      showEvaluationType msg = H.div . evConfigCata
        (fromString . msg $ Msg_NewAssignment_BinaryEvaluation "Binary Evaluation")
        (const . fromString . msg $ Msg_NewAssignment_PercentageEvaluation "Percentage Evaluation")

      [txtSubmission, zipSubmission] = [Assignment.TextSubmission, Assignment.ZipSubmission]

      currentSubmissionType =
        if Assignment.isZippedSubmissions aas
          then zipSubmission
          else txtSubmission

      submissionType msg = do
        H.b (fromString . msg $ Msg_NewAssignment_SubmissionType "Submission Type")

      editOrReadonly = pageDataCata
        (const5 id)
        (const5 id)
        (const7 id)
        (const5 (! A.readonly ""))
        (const7 id)
        (const7 id)
        (const8 id)

      linkToPandocMarkdown = "http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html"

      pagePreview :: PageData -> PageDesc
      pagePreview = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase _ev -> modifyAssignmentPreview key)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase _tc _ev -> modifyAssignmentPreview key)

      page :: PageData -> PageDesc
      page = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignment key)
        (\_tz ak _asg _tsType _files _testcase _ev -> modifyAssignment ak)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignment key)
        (\_tz k _a _fs _ts _tc _tm _ev -> modifyAssignment k)

      newCourseAssignment k = Pages.newCourseAssignment k ()
      newGroupAssignment k  = Pages.newGroupAssignment k ()
      modifyAssignment k    = Pages.modifyAssignment k ()
      newCourseAssignmentPreview k = Pages.newCourseAssignmentPreview k ()
      newGroupAssignmentPreview k  = Pages.newGroupAssignmentPreview k ()
      modifyAssignmentPreview k    = Pages.modifyAssignmentPreview k ()
      viewAssignment k = Pages.viewAssignment k ()
      uploadFile = Pages.uploadFile ()

      amap :: (Assignment -> a) -> Maybe a -> PageData -> Maybe a
      amap f _ (PD_Assignment _ _ a _ _ _ _) = Just . f $ a
      amap f _ (PD_Assignment_Preview _ _ a _ _ __ _ _) = Just . f $ a
      amap f _ (PD_ViewAssignment _ _ a _ _)     = Just . f $ a
      amap f _ (PD_Course_Preview _ _ _ _ _ a _) = Just . f $ a
      amap f _ (PD_Group_Preview  _ _ _ _ _ a _) = Just . f $ a
      amap _ def _ = def

      previewDiv msg = pageDataCata
        (const5 empty)
        (const5 empty)
        (const7 empty)
        (const5 empty)
        (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
        (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
        (\_timezone _key asg _tst _files _testcase _tm _ev -> assignmentPreview asg)
        where
         assignmentPreview a = H.div $ do
           H.h3 $ fromString . msg $ Msg_NewAssignment_AssignmentPreview "Assignment Preview"
           H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc a

      startTimePart = pageDataCata
        (const5 hiddenStartTime)
        (const5 hiddenStartTime)
        (const7 hiddenStartTime)
        (const5 (fromString $ concat [startDefDate, " ", startDefHour, ":", startDefMin, ":00"]))
        (const7 hiddenStartTime)
        (const7 hiddenStartTime)
        (const8 hiddenStartTime)

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
        (const7 hiddenEndTime)
        (const5 (fromString $ concat [endDefDate, " ", endDefHour, ":", endDefMin, ":00"]))
        (const7 hiddenEndTime)
        (const7 hiddenEndTime)
        (const8 hiddenEndTime)

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
        (const7 (return ()))
        (const5 (return ()))
        (\_tz _t (_key,course) _tsType _fs _a _tc -> fromString $ courseName course)
        (\_tz _t (_key,group)  _tsType _fs _a _tc -> fromString $ groupName group)
        (const8 (return ()))

      hiddenKeyField = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key) ! asgField)
        (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key) ! asgField)
        (const7 (return ()))
        (const5 (return ()))
        (\_tz _t (key,_course) _tsType _fs _a _tc -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key) ! asgField)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key) ! asgField)
        (const8 (return ()))

      testCaseArea msg = pageDataCata
        (\_tz _t _c tsType fs -> createTestCaseArea fs tsType)
        (\_tz _t _g tsType fs -> createTestCaseArea fs tsType)
        (\_tz _k _a tsType fs tc _ev -> overwriteTestCaseArea fs tsType tc)
        (\_tz _k _a ts tc -> viewTestCaseArea ts tc)
        (\_tz _t _c tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _t _g tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _k _a tsType fs tc tm _ev -> overwriteTestCaseAreaPreview fs tsType tc tm)
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
                selectionWithDefault (fieldName assignmentUsersFileField) uf (map keyValue fs) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFileCata id))

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
                selection (fieldName assignmentUsersFileField) (map keyValue fs) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFileCata id))

          testCaseText Nothing = Nothing
          testCaseText (Just (_,tc,_)) = withTestCaseValue (tcValue tc) Just (const Nothing)

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
                selectionWithDefault (fieldName assignmentUsersFileField) uf (map keyValue ((Left ()):map Right fs)) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFileCata id uf)


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
                selection (fieldName assignmentUsersFileField) (map keyValue ((Left ()):map Right fs)) ! asgField
                H.p $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFileCata id uf)

      -- TODO
      testScriptSelection :: (Translation String -> String) -> PageData -> H.Html
      testScriptSelection msg = pageDataCata
        (\_tz _t _c tsType _fs -> scriptSelection tsType)
        (\_tz _t _g tsType _fs -> scriptSelection tsType)
        (\_tz _k _a tsType _fs mts _ev -> modificationScriptSelection tsType mts)
        (const5 (return ()))
        (\_tz _t _c tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _t _g tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _k _a tsType _fs mts tm _ev -> modificationScriptSelectionPreview tsType mts tm)
        where
          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            H.br
            selection (fieldName assignmentTestScriptField) (map keyValue (Nothing:map Just ts)) ! asgField

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
            selectionWithDefault'
              (fieldName assignmentTestScriptField)
              ((Just tsk)==)
              (map keyValue (Nothing:map Just ts))
              ! asgField

          modificationScriptSelection ts mts = maybe
            (return ())
            (mtsSelection mts)
            ts

          mtsSelection mts ts = do
            H.br
            H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
            H.br
            selectionWithDefault'
              (fieldName assignmentTestScriptField)
              (def mts)
              (map keyValue (Nothing:map Just ts))
              ! asgField
            where
              def Nothing Nothing = True
              def Nothing _       = False
              def (Just (_,_,tsk)) (Just tsk') = tsk == tsk'
              def _                _           = False

          modificationScriptSelectionPreview ts _mts tm =
            case (tcmpTestScriptKey tm, ts) of
              (Just Nothing   , Just ts') -> mtsSelection' Nothing ts'
              (Just (Just tsk), Just ts') -> mtsSelection' (Just tsk) ts'
              _                           -> return ()
            where
              mtsSelection' tsk ts = do
                H.br
                H.b . fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
                H.br
                selectionWithDefault'
                  (fieldName assignmentTestScriptField)
                  (def tsk)
                  (map keyValue (Nothing:map Just ts))
                  ! asgField
                where
                  def Nothing Nothing = True
                  def Nothing _       = False
                  def (Just tsk) (Just tsk') = tsk == tsk'
                  def _                _     = False


          keyValue :: Maybe (TestScriptKey, TestScriptInfo) -> (Maybe TestScriptKey, String)
          keyValue Nothing = (Nothing, msg $ Msg_NewAssignment_NoTesting "Assignment without testing")
          keyValue (Just (testScriptKey, tsInfo)) = ((Just testScriptKey), tsiName tsInfo)

          nothing = Nothing :: Maybe TestScriptKey


      timeZoneConverter = pageDataCata
        (\tz _t _c _ts _fs -> tz)
        (\tz _t _g _ts _fs -> tz)
        (\tz _k _a _ts _fs _tc _ev -> tz)
        (\tz _k _a _ts _tc -> tz)
        (\tz _t _c _ts _fs _a _tc  -> tz)
        (\tz _t _g _ts _fs _a _tc  -> tz)
        (\tz _k _a _ts _fs _tc _tm _ev -> tz)
        pd

      date t =
        let localTime = timeZoneConverter t
            timeOfDay = Time.localTimeOfDay localTime
        in ( show $ Time.localDay         localTime
           , printf "%02d" $ Time.todHour timeOfDay
           , printf "%02d" $ Time.todMin  timeOfDay
           )

      (startDefDate, startDefHour, startDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc _ev -> Assignment.start a)
        (\_tz _k a _ts _tc -> Assignment.start a)
        (\_tz _t _c _ts _fs a _tc  -> Assignment.start a)
        (\_tz _t _g _ts _fs a _tc  -> Assignment.start a)
        (\_tz _k a _ts _fs _tc _tm _ev -> Assignment.start a)
        pd

      (endDefDate, endDefHour, endDefMin) = date $ pageDataCata
        (\_tz t _c _ts _fs -> t)
        (\_tz t _g _ts _fs -> t)
        (\_tz _k a _ts _fs _tc _ev -> Assignment.end a)
        (\_tz _k a _ts _tc -> Assignment.end a)
        (\_tz _t _c _ts _fs a _tc  -> Assignment.end a)
        (\_tz _t _g _ts _fs a _tc  -> Assignment.end a)
        (\_tz _k a _ts _fs _tc _tm _ev -> Assignment.end a)
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

