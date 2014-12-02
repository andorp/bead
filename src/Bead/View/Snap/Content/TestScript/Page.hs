{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.TestScript.Page (
    newTestScript
  , modifyTestScript
  ) where

import           Control.Arrow ((***))
import           Data.Monoid
import           Data.String (fromString)
import           Data.String.Utils (replace)

import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 as H

import           Bead.View.Snap.Content hiding (option)
import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
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
newTestScriptPage = withUserState $ \s -> do
  cs <- userStory Story.administratedCourses
  renderBootstrapPage . bootstrapUserFrame s $ testScriptContent (Create cs)

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
modifyTestScriptPage = withUserState $ \s -> do
  tsk <- getParameter testScriptKeyPrm
  (course, script) <- userStory $ do
    Story.isAdministratedTestScript tsk
    (script, ck)  <- Story.loadTestScript tsk
    (course, _gk) <- Story.loadCourse ck
    return (course, script)
  renderBootstrapPage . bootstrapUserFrame s $
    testScriptContent (Modify (courseName course) tsk script)

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
    checkIfThereCourses [] = hasNoCourses
    checkIfThereCourses _  = hasPageContent pd

    modify _ _ _ = hasPageContent pd

hasNoCourses :: IHtml
hasNoCourses = do
  msg <- getI18N
  return $ do
  fromString . msg $ Msg_NewTestScript_HasNoCourses "This user cannot administer any courses."

-- Helper

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"

hasPageContent :: PageData -> IHtml
hasPageContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ Bootstrap.colMd12 $ H.form $ do
      postForm (routeOf $ testScriptPage pd) $ do
        Bootstrap.textInput
          (fieldName testScriptNameField)
          (fromString . msg $ Msg_NewTestScript_Name "Name")
          (maybe mempty fromString $ testScriptName pd)
        Bootstrap.textInput
          (fieldName testScriptDescField)
          (fromString . msg $ Msg_NewTestScript_Description "Description")
          (maybe mempty fromString $ testScriptDesc pd)

        testScriptCourse msg

        Bootstrap.textArea
          (fieldName testScriptScriptField)
          (fromString . msg $ Msg_NewTestScript_Script "Test script")
          (maybe mempty fromString $ testScriptScript pd)

        Bootstrap.textArea
          (fieldName testScriptNotesField)
          (fromString . msg $ Msg_NewTestScript_Notes "Help for writing test cases")
          (maybe mempty fromString $ testScriptNotes pd)

      Bootstrap.row $ Bootstrap.colMd12 $
        submitButton (fieldName testScriptSaveButton)
        (fromString . msg $ Msg_NewTestScript_Save "Commit")

    Bootstrap.turnSelectionsOn

  where
    testScriptPage = pageDataCata (const (Pages.newTestScript ())) (\_name key _script -> Pages.modifyTestScript key ())
    testScriptName = pageDataCata (const Nothing) (const2 (Just . tsName))
    testScriptDesc = pageDataCata (const Nothing) (const2 (Just . tsDescription))
    testScriptNotes = pageDataCata (const Nothing) (const2 (Just . tsNotes))
    testScriptScript = pageDataCata (const Nothing) (const2 (Just . tsScript))
    testScriptCourse msg = pageDataCata
      (Bootstrap.selection (fieldName testScriptCourseKeyField) (const False) . Prelude.map (Prelude.id *** courseNameAndType))
      (\courseName _key _script -> fromString courseName)
      where
        courseNameAndType c = concat
          [courseName c, " - ", courseTypeStr $ courseTestScriptType c]
        courseTypeStr = msg . testScriptTypeCata
          (Msg_TestScriptTypeSimple "Textual")
          (Msg_TestScriptTypeZipped "Binary")
