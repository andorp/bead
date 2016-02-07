{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assignment.View where

import           Prelude hiding (min)

import           Control.Arrow ((&&&))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BsLazy
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString(..), fromString)
import qualified Data.Time as Time

import qualified Text.Blaze.Html5.Attributes as A (id)
import           Text.Blaze.Html5.Attributes as A hiding (id)
import           Text.Blaze.Html5 as H hiding (map)
import           Text.Printf (printf)

import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Shared.Evaluation
import           Bead.View.Fay.HookIds
import           Bead.View.Fay.Hooks
import           Bead.View.Content hiding (name, option, required)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Markdown

import           Bead.View.Content.Assignment.Data

newAssignmentContent :: PageData -> IHtml
newAssignmentContent pd = do
  msg <- getI18N
  let hook = assignmentEvTypeHook

  -- Renders a evaluation selection or hides it if there is a submission already for the assignment,
  -- and renders an explanation.
  evalConfig <- do
    let evaluationTypeSelection = return $ do
          Bootstrap.selectionWithLabel
            (evHiddenValueId hook)
            (msg $ msg_NewAssignment_EvaluationType "Evaluation Type")
            (== currentEvaluationType)
            [ (binaryConfig, fromString . msg $ msg_NewAssignment_BinEval "Binary")
            , (percentageConfig 0.0, fromString . msg $ msg_NewAssignment_PctEval "Percentage")
            , (freeFormConfig, fromString . msg $ msg_NewAssignment_FftEval "Free form textual")
            ]
    let hiddencfg asg = return $ do
          let e = Assignment.evType asg
          showEvaluationType msg e
          fromString . msg $ msg_NewAssignment_EvalTypeWarn "The evaluation type can not be modified, there is a submission for the assignment."
          hiddenInput (evHiddenValueId hook) (encodeToFay' "selection" e)
    pageDataCata
      (const5 evaluationTypeSelection)
      (const5 evaluationTypeSelection)
      (\_tz _key asg _ts _fs _tc ev -> if ev then evaluationTypeSelection else hiddencfg asg)
      (const5 evaluationTypeSelection)
      (const7 evaluationTypeSelection)
      (const7 evaluationTypeSelection)
      (\_tz _key asg _ts _fs _tc _tm ev -> if ev then evaluationTypeSelection else hiddencfg asg)
      pd

  return $ do
            Bootstrap.row $ Bootstrap.colMd12
              $ H.form ! A.method "post"
              $ H.div ! A.id (fromString $ hookId assignmentForm) $ do

                Bootstrap.formGroup $ do
                    let assignmentTitleField = fromString $ fieldName assignmentNameField

                        assignmentTitlePlaceholder = fromString $
                           fromAssignment
                                (const "")
                                (fromString . msg $ msg_NewAssignment_Title_Default "Unnamed Assignment")
                                pd

                        assignmentTitle = fromAssignment (fromString . Assignment.name) mempty pd

                    Bootstrap.labelFor assignmentTitleField (fromString $ msg $ msg_NewAssignment_Title "Title")
                    editOrReadOnly pd $ Bootstrap.textInputFieldWithDefault assignmentTitleField assignmentTitle

                    H.p ! class_ "help-block"$ fromString . msg $ msg_NewAssignment_Info_Normal $ concat
                      [ "Solutions may be submitted from the time of opening until the time of closing. "
                      , "The assignment will not be visible until it is opened. "
                      , "The assignments open and close automatically."
                      ]

                -- Visibility information of the assignment
                H.h4 $ fromString $ msg $ msg_NewAssignment_SubmissionDeadline "Visibility"

                let date t =
                      let localTime = timeZoneConverter t
                          timeOfDay = Time.localTimeOfDay localTime
                      in ( show $ Time.localDay         localTime
                         , printf "%02d" $ Time.todHour timeOfDay
                         , printf "%02d" $ Time.todMin  timeOfDay
                         )

                    showDate (dt, hour, min) = concat [dt, " ", hour, ":", min, ":00"]

                    startDateStringValue = showDate $ date $ pageDataCata
                      (\_tz t _c _ts _fs -> t)
                      (\_tz t _g _ts _fs -> t)
                      (\_tz _k a _ts _fs _tc _ev -> Assignment.start a)
                      (\_tz _k a _ts _tc -> Assignment.start a)
                      (\_tz _t _c _ts _fs a _tc  -> Assignment.start a)
                      (\_tz _t _g _ts _fs a _tc  -> Assignment.start a)
                      (\_tz _k a _ts _fs _tc _tm _ev -> Assignment.start a)
                      pd

                    endDateStringValue = showDate $ date $ pageDataCata
                      (\_tz t _c _ts _fs -> t)
                      (\_tz t _g _ts _fs -> t)
                      (\_tz _k a _ts _fs _tc _ev -> Assignment.end a)
                      (\_tz _k a _ts _tc -> Assignment.end a)
                      (\_tz _t _c _ts _fs a _tc  -> Assignment.end a)
                      (\_tz _t _g _ts _fs a _tc  -> Assignment.end a)
                      (\_tz _k a _ts _fs _tc _tm _ev -> Assignment.end a)
                      pd

                -- Opening and closing dates of the assignment
                Bootstrap.formGroup $ do
                  Bootstrap.row $ do

                    -- Opening date of the assignment
                    Bootstrap.colMd6 $ do
                      let assignmentStart = fieldName assignmentStartField
                      Bootstrap.labelFor assignmentStart $ fromString $ msg $ msg_NewAssignment_StartDate "Opens"
                      Bootstrap.datetimePicker assignmentStart startDateStringValue isEditPage

                    -- Closing date of the assignment
                    Bootstrap.colMd6 $ do
                      let assignmentEnd = fieldName assignmentEndField
                      Bootstrap.labelFor assignmentEnd $ msg $ msg_NewAssignment_EndDate "Closes"
                      Bootstrap.datetimePicker assignmentEnd endDateStringValue isEditPage

                Bootstrap.rowColMd12 $ H.hr

                -- Properties of the assignment
                H.h4 $ fromString $ msg $ msg_NewAssignment_Properties "Properties"

                let editable = True
                    readOnly = False

                    assignmentPropertiesSection ed = do
                      let pwd = if Assignment.isPasswordProtected aas
                                   then Just (Assignment.getPassword aas)
                                   else Nothing
                          noOfTries = if Assignment.isNoOfTries aas
                                        then Just (Assignment.getNoOfTries aas)
                                        else Nothing
                          editable x = if ed then x else (x ! A.readonly "")
                          readOnly = not ed
                          assignmentAspect = fromString $ fieldName assignmentAspectField
                          assignmentPwd    = fromString $ fieldName assignmentPwdField
                          assignmentNoOfTries = fromString $ fieldName assignmentNoOfTriesField


                      bootstrapCheckbox $
                              checkBoxRO (fieldName assignmentAspectField)
                                (Assignment.isBallotBox aas)
                                readOnly
                                Assignment.BallotBox (msg $ msg_NewAssignment_BallotBox "Ballot Box")

                      Bootstrap.helpBlock $ msg $ msg_NewAssignment_Info_BallotBox $ concat
                                [ "(Recommended for tests.) Students will not be able to access submissions and "
                                , "their evaluations until the assignment is closed."
                                ]

                      bootstrapCheckbox $
                            checkBoxRO (fieldName assignmentAspectField)
                              (Assignment.isIsolated aas)
                              readOnly
                              Assignment.Isolated
                              (msg $ msg_NewAssignment_Isolated "Isolated")

                      Bootstrap.helpBlock $ msg $ msg_NewAssignment_Info_Isolated $ concat
                               [ "(Recommended for tests.) Submissions for other assignments of the course are not visible in the "
                               , "precense of an isolated assignments. Note: If there is more than one isolated assignment for the "
                               , "same course, all the isolated assignment and submissions will be visible for the students."
                               ]

                      Bootstrap.row $ Bootstrap.colMd6 $ do
                          bootstrapCheckbox $ do
                                  checkBoxRO (fieldName assignmentAspectField)
                                    (Assignment.isNoOfTries aas)
                                    readOnly
                                    (Assignment.NoOfTries 0)
                                    (msg $ msg_NewAssignment_NoOfTries "No of tries")

                      Bootstrap.row $ Bootstrap.colMd6 $ Bootstrap.formGroup $
                            editable $ numberInput assignmentNoOfTries (Just 1) (Just 1000) noOfTries ! Bootstrap.formControl

                      Bootstrap.helpBlock $ msg $ msg_NewAssignment_Info_NoOfTries $
                               "Limitation the number of the submissions (per student) for the assignment."

                      bootstrapCheckbox $
                            checkBoxRO (fieldName assignmentAspectField)
                              (Assignment.isPasswordProtected aas)
                              readOnly
                              (Assignment.Password "")
                              (msg $ msg_NewAssignment_PasswordProtected "Password-protected")

                      Bootstrap.helpBlock $ msg $ msg_NewAssignment_Info_Password $ concat
                                [ "(Recommended for tests.) Submissions may be only submitted by providing the password. "
                                , "The teacher shall use the password during the test in order to authenticate the "
                                , "submission for the student."
                                ]

                      Bootstrap.row $ Bootstrap.colMd6 $ Bootstrap.formGroup $ do
                          H.label $ fromString $ msg $ msg_NewAssignment_Password "Password"
                          editable $ Bootstrap.inputForFormControl
                                     ! name assignmentPwd ! type_ "text"
                                     ! value (fromString $ fromMaybe "" pwd)

                      Bootstrap.rowColMd12 $ H.hr

                -- Assignment Properties
                pageDataCata
                  (const5 $ assignmentPropertiesSection editable)
                  (const5 $ assignmentPropertiesSection editable)
                  (const7 $ assignmentPropertiesSection editable)
                  (const5 $ assignmentPropertiesSection readOnly)
                  (const7 $ assignmentPropertiesSection editable)
                  (const7 $ assignmentPropertiesSection editable)
                  (const8 $ assignmentPropertiesSection editable)
                  pd

                submissionTypeSelection msg pd

                -- Assignment Description
                Bootstrap.formGroup $ do
                    let assignmentDesc = fromString $ fieldName assignmentDescField
                    Bootstrap.labelFor assignmentDesc $ fromString . msg $ msg_NewAssignment_Description "Description"
                    editOrReadOnly pd $ Bootstrap.textAreaField assignmentDesc $ do
                      fromString $ fromAssignment Assignment.desc (fromString . msg $
                        msg_NewAssignment_Description_Default $ unlines
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
                          ]) pd

                -- Preview of the assignment
                let assignmentPreview a = do
                      Bootstrap.formGroup $ do
                        H.label $ fromString $ msg $ msg_NewAssignment_AssignmentPreview "Assignment Preview"
                        H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc a

                pageDataCata
                  (const5 empty)
                  (const5 empty)
                  (const7 empty)
                  (const5 empty)
                  (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
                  (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
                  (\_tz _k a _t _fs _tst _tm _ev -> assignmentPreview a)
                  pd

                -- Assignment Test Script Selection
                Bootstrap.formGroup $ do
                      testScriptSelection msg pd

                -- Test Case area
                Bootstrap.formGroup $ do
                    testCaseArea msg pd

                -- Evaluation config
                Bootstrap.formGroup $ do
                  let previewAndCommitForm cfg = do
                        evalSelectionDiv hook
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

                -- Hidden course or group keys for the assignment creation
                pageDataCata
                  (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
                  (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
                  (const7 (return ()))
                  (const5 (return ()))
                  (\_tz _t (key,_course) _tsType _fs _a _tc -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
                  (\_tz _t (key,_group)  _tsType _fs _a _tc -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
                  (const8 (return ()))
                  pd

                -- Submit buttons
                Bootstrap.row $ do
                   let formAction page = onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
                   Bootstrap.colMd6 $
                      onlyOnEdit pd $ Bootstrap.submitButtonWithAttr (formAction $ pagePreview pd) (msg $ msg_NewAssignment_PreviewButton "Preview")
                   Bootstrap.colMd6 $
                      onlyOnEdit pd $ Bootstrap.submitButtonWithAttr (formAction $ page pd) (msg $ msg_NewAssignment_SaveButton "Commit")

            Bootstrap.turnSelectionsOn

    where
      aas = fromAssignment Assignment.aspects Assignment.emptyAspects pd
      currentEvaluationType = fromAssignment Assignment.evType binaryConfig pd

      editOrReadOnly = pageDataCata
        (const5 id)
        (const5 id)
        (const7 id)
        (const5 (! A.readonly ""))
        (const7 id)
        (const7 id)
        (const8 id)

      onlyOnEdit pd t = pageDataCata
        (const5 t)
        (const5 t)
        (const7 t)
        (const5 mempty)
        (const7 t)
        (const7 t)
        (const8 t)
        pd

      isEditPage = pageDataCata
        (const5 True)
        (const5 True)
        (const7 True)
        (const5 False)
        (const7 True)
        (const7 True)
        (const8 True)
        pd

      timeZoneConverter = pageDataCata
        (\tz _t _c _ts _fs -> tz)
        (\tz _t _g _ts _fs -> tz)
        (\tz _k _a _ts _fs _tc _ev -> tz)
        (\tz _k _a _ts _tc -> tz)
        (\tz _t _c _ts _fs _a _tc  -> tz)
        (\tz _t _g _ts _fs _a _tc  -> tz)
        (\tz _k _a _ts _fs _tc _tm _ev -> tz)
        pd

      fromAssignment :: (Assignment -> a) -> a -> PageData -> a
      fromAssignment f d pd = maybe d f (get pd) where
        get (PD_Assignment _ _ a _ _ _ _)           = Just a
        get (PD_Assignment_Preview _ _ a _ _ _ _ _) = Just a
        get (PD_ViewAssignment _ _ a _ __   ) = Just a
        get (PD_Course_Preview _ _ _ _ _ a _) = Just a
        get (PD_Group_Preview  _ _ _ _ _ a _) = Just a
        get _ = Nothing

      -- Renders a submission type selection for all page type but the view
      -- which prints only the selected type
      submissionTypeSelection msg pd = do

        let submissionTypeSelection =
              Bootstrap.selectionWithLabel
                (fieldName assignmentSubmissionTypeField)
                (msg $ msg_NewAssignment_SubmissionType "Submission Type")
                (== currentSubmissionType)
                [ (txtSubmission, fromString . msg $ msg_NewAssignment_TextSubmission "Text")
                , (zipSubmission, fromString . msg $ msg_NewAssignment_ZipSubmission "Zip file")
                ]

        let showSubmissionType s =
              Bootstrap.readOnlyTextInputWithDefault ""
                (msg $ msg_NewAssignment_SubmissionType "Submission Type")
                (Assignment.submissionType
                  (fromString . msg $ msg_NewAssignment_TextSubmission "Text")
                  (fromString . msg $ msg_NewAssignment_ZipSubmission "Zip file")
                  (Assignment.aspectsToSubmissionType $ Assignment.aspects s))

        pageDataCata
          (const5 submissionTypeSelection)
          (const5 submissionTypeSelection)
          (const7 submissionTypeSelection)
          (\_timezone _key asg _tsInfo _testcase -> showSubmissionType asg)
          (const7 submissionTypeSelection)
          (const7 submissionTypeSelection)
          (const8 submissionTypeSelection)
          pd

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
          testScriptField :: (IsString s) => s
          testScriptField = fieldName assignmentTestScriptField

          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            Bootstrap.selectionWithLabel
              testScriptField
              (msg $ msg_NewAssignment_TestScripts "Tester")
              (const False)
              (map keyValue (Nothing:map Just ts))

          scriptSelectionPreview ts tcp = case tcp of
            (Just Nothing    , _, _) -> scriptSelection ts
            (Just (Just tsk) , _, _) -> preview ts tsk
            _ -> return ()
            where
              preview ts tsk = maybe (return ()) (tsSelectionPreview tsk) ts

          tsSelectionPreview tsk ts = do
            Bootstrap.selectionWithLabel
              testScriptField
              (msg $ msg_NewAssignment_TestScripts "Tester")
              ((Just tsk)==)
              (map keyValue (Nothing:map Just ts))

          modificationScriptSelection ts mts = maybe
            (return ())
            (mtsSelection mts)
            ts

          mtsSelection mts ts = do
            Bootstrap.selectionWithLabel
              testScriptField
              (msg $ msg_NewAssignment_TestScripts "Tester")
              (def mts)
              (map keyValue (Nothing:map Just ts))
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
                Bootstrap.selectionWithLabel
                  testScriptField
                  (msg $ msg_NewAssignment_TestScripts "Test scripts")
                  (def tsk)
                  (map keyValue (Nothing:map Just ts))
                where
                  def Nothing Nothing = True
                  def Nothing _       = False
                  def (Just tsk) (Just tsk') = tsk == tsk'
                  def _                _     = False


          keyValue :: Maybe (TestScriptKey, TestScriptInfo) -> (Maybe TestScriptKey, String)
          keyValue Nothing = (Nothing, msg $ msg_NewAssignment_NoTesting "Assignment without testing")
          keyValue (Just (testScriptKey, tsInfo)) = ((Just testScriptKey), tsiName tsInfo)

          nothing = Nothing :: Maybe TestScriptKey

      -- Test Case Ares

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
            Bootstrap.labelFor (fieldName assignmentTestCaseField) (msg $ msg_NewAssignment_TestCase "Test cases")
            editOrReadOnly pd $ Bootstrap.textAreaOptionalField (fieldName assignmentTestCaseField) (maybe mempty fromString val)

          createTestCaseAreaPreview fs ts tcp = case tcp of
            (Just Nothing , Nothing, Nothing) -> createTestCaseArea fs ts
            (Just _       , Just uf, Nothing) -> userFileSelection uf
            (Just _       , Nothing,  Just f) -> textAreaPreview f
            _ -> return ()
            where
              userFileSelection uf = do
                Bootstrap.selectionOptionalWithLabel
                  (fieldName assignmentUsersFileField)
                  (msg $ msg_NewAssignment_TestFile "Test File") (uf==) (map keyValue fs)
                Bootstrap.helpBlock $ fromString (printf (msg $ msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ msg_LinkText_UploadFile "Upload File"))
                Bootstrap.buttonGroup $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFile id id))

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
                Bootstrap.selectionOptionalWithLabel
                  (fieldName assignmentUsersFileField)
                  (msg $ msg_NewAssignment_TestFile "Test File")
                  (const False) (map keyValue fs)
                Bootstrap.helpBlock $ printf (msg $ msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ msg_LinkText_UploadFile "Upload File")
                Bootstrap.buttonGroup $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFile id id))

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
                H.h4 $ fromString . msg $ msg_NewAssignment_TestFile "Test File"
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
                Bootstrap.selectionOptionalWithLabel
                  (fieldName assignmentUsersFileField)
                  (msg $ msg_NewAssignment_TestFile "Test File")
                  (==uf) (map keyValue ((Left ()):map Right fs))
                H.pre $ testCaseFileName tc
                Bootstrap.helpBlock $ fromString $ printf (msg $ msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ msg_LinkText_UploadFile "Upload File")
                Bootstrap.buttonGroup $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFile id id uf)


          overwriteTestCaseArea fs ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where

              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc)) -- simple
                usersFileSelection           -- zipped

              usersFileSelection = do
                Bootstrap.selectionOptionalWithLabel
                  (fieldName assignmentUsersFileField)
                  (msg $ msg_NewAssignment_TestFile "Test File")
                  (const False)
                  (map keyValue ((Left ()):map Right fs))
                H.pre $ testCaseFileName tc
                Bootstrap.helpBlock $ printf (msg $ msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ msg_LinkText_UploadFile "Upload File")
                Bootstrap.buttonGroup $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFile id id uf)

      testScriptType' Nothing   = Nothing
      testScriptType' (Just []) = Nothing
      testScriptType' (Just ((_tk,tsi):_)) = Just $ tsiType tsi

      testScriptType'' = fmap tsiType

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

      showEvaluationType msg e =
        Bootstrap.readOnlyTextInputWithDefault ""
          (msg $ msg_NewAssignment_EvaluationType "Evaluation Type")
          (evConfigCata
            (fromString . msg $ msg_NewAssignment_BinaryEvaluation "Binary Evaluation")
            (const . fromString . msg $ msg_NewAssignment_PercentageEvaluation "Percent")
            (fromString . msg $ msg_NewAssignment_FreeFormEvaluation "Free Form Evaluation")
            e)

      [txtSubmission, zipSubmission] = [Assignment.TextSubmission, Assignment.ZipSubmission]

      currentSubmissionType =
        if Assignment.isZippedSubmissions aas
          then zipSubmission
          else txtSubmission

      -- Pages constants

      newCourseAssignment k = Pages.newCourseAssignment k ()
      newGroupAssignment k  = Pages.newGroupAssignment k ()
      modifyAssignment k    = Pages.modifyAssignment k ()
      newCourseAssignmentPreview k = Pages.newCourseAssignmentPreview k ()
      newGroupAssignmentPreview k  = Pages.newGroupAssignmentPreview k ()
      modifyAssignmentPreview k    = Pages.modifyAssignmentPreview k ()
      viewAssignment k = Pages.viewAssignment k ()
      uploadFile = Pages.uploadFile ()

      -- Boostrap
      bootstrapCheckbox tag = H.div ! A.class_ "checkbox" $ H.label $ tag
