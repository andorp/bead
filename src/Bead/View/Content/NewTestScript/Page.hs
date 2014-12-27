{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.NewTestScript.Page (
    newTestScript
  , modifyTestScript
  ) where

import           Control.Arrow ((***))
import           Data.String (fromString)
import           Data.String.Utils (replace)

import           Text.Blaze.Html5 as H hiding (map)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.UserActions as UA

-- * Content Handlers

data PageData
  = Create [(CourseKey, Course)]
  -- ^ Create a new test script
  | Modify String TestScriptKey TestScript
  -- ^ Modify an existing test script with CourseName Key Script

pageDataCata
  create
  modify
  p = case p of
    Create cs    -> create cs
    Modify cn tk ts -> modify cn tk ts

newTestScript :: ViewModifyHandler
newTestScript = ViewModifyHandler newTestScriptPage postNewTestScript

modifyTestScript :: ViewModifyHandler
modifyTestScript = ViewModifyHandler modifyTestScriptPage postModifyTestScript

newTestScriptPage :: GETContentHandler
newTestScriptPage = do
  cs <- userStory Story.administratedCourses
  return $ testScriptContent (Create cs)

postNewTestScript :: POSTContentHandler
postNewTestScript = do
  script' <- TestScript
    <$> (getParameter (stringParameter (fieldName testScriptNameField) "Test Script Name"))
    <*> (getParameter (stringParameter (fieldName testScriptDescField) "Test Script Description"))
    <*> (getParameter (stringParameter (fieldName testScriptNotesField) "Test Script Notes"))
    <*> (replaceCrlf <$> getParameter (stringParameter (fieldName testScriptScriptField) "Test Script"))
  ck <- getParameter (jsonParameter (fieldName testScriptCourseKeyField) "Course Key")
  script <- userStory $ do
    Story.isAdministratedCourse ck
    (course,_groupkeys) <- Story.loadCourse ck
    return (script' $ courseTestScriptType course)
  return $ UA.CreateTestScript ck script

modifyTestScriptPage :: GETContentHandler
modifyTestScriptPage = do
  tsk <- getParameter testScriptKeyPrm
  (course, script) <- userStory $ do
    Story.isAdministratedTestScript tsk
    (script, ck)  <- Story.loadTestScript tsk
    (course, _gk) <- Story.loadCourse ck
    return (course, script)
  return $ testScriptContent (Modify (courseName course) tsk script)

postModifyTestScript :: POSTContentHandler
postModifyTestScript = do
  script' <- TestScript
    <$> (getParameter (stringParameter (fieldName testScriptNameField) "Test Script Name"))
    <*> (getParameter (stringParameter (fieldName testScriptDescField) "Test Script Description"))
    <*> (getParameter (stringParameter (fieldName testScriptNotesField) "Test Script Notes"))
    <*> (replaceCrlf <$> getParameter (stringParameter (fieldName testScriptScriptField) "Test Script"))
  tsk <- getParameter testScriptKeyPrm
  script <- userStory $ do
    (testscript, _coursekey) <- Story.loadTestScript tsk
    return (script' $ tsType testscript)
  return $ UA.ModifyTestScript tsk script

testScriptContent :: PageData -> IHtml
testScriptContent pd = pageDataCata checkIfThereCourses modify pd
  where
    checkIfThereCourses [] = hasNoCourses pd
    checkIfThereCourses _  = pageContent pd

    modify _ _ _ = pageContent pd

hasNoCourses :: PageData -> IHtml
hasNoCourses pd = do
  msg <- getI18N
  return $ do
    let pageTitle = pageDataCata (const $ Msg_LinkText_NewTestScript "New Test") (const3 $ Msg_LinkText_ModifyTestScript "Modify Test Script") pd
    Bootstrap.rowColMd12 $ p $
      fromString . msg $ Msg_NewTestScript_HasNoCourses "This user cannot administer any courses."

pageContent :: PageData -> IHtml
pageContent pd = do
  msg <- getI18N

  let textField param label valueSelector = pageDataCata
        (const $ Bootstrap.textInput (fieldName param) (msg label) "")
        (\_name _key script -> Bootstrap.textInputWithDefault (fieldName param) (msg label) (valueSelector script))
        pd

  let textArea param label valueSelector = pageDataCata
        (const $ Bootstrap.textArea (fieldName param) (msg label) "")
        (\_name _key script -> Bootstrap.textArea (fieldName param) (msg label) (fromString (valueSelector script)))
        pd

  let utf8TextArea param label valueSelector = pageDataCata
        (const $ Bootstrap.utf8TextArea (fieldName param) (msg label) "")
        (\_name _key script -> Bootstrap.utf8TextArea (fieldName param) (msg label) (fromString (valueSelector script)))
        pd

  let name        = textField testScriptNameField (Msg_NewTestScript_Name "Name") tsName
  let description = textField testScriptDescField (Msg_NewTestScript_Description "Description") tsDescription
  let help        = textArea  testScriptNotesField (Msg_NewTestScript_Notes "Help for writing test cases") tsNotes
  let script      = textArea  testScriptScriptField (Msg_NewTestScript_Script "Test script") tsScript

  return $ do
    postForm (routeOf $ testScriptPage pd) $ do
      course msg
      name
      description
      help
      script
      Bootstrap.submitButton (fieldName testScriptSaveButton) (fromString . msg $ Msg_NewTestScript_Save "Commit")
    Bootstrap.turnSelectionsOn
  where
    testScriptPage = pageDataCata (const (Pages.newTestScript ())) (\_name key _script -> Pages.modifyTestScript key ())

    course msg = pageDataCata
      (Bootstrap.selectionWithLabel (fieldName testScriptCourseKeyField) (msg $ Msg_NewTestScript_Course "Course:")
                                    (const False) . map (id *** courseNameAndType))
      (\courseName _key _script -> fromString courseName)
      pd
      where
        courseNameAndType c = concat
          [courseName c, " - ", courseTypeStr $ courseTestScriptType c]
        courseTypeStr = msg . testScriptTypeCata
          (Msg_TestScriptTypeSimple "Textual")
          (Msg_TestScriptTypeZipped "Binary")

-- Helper

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"
