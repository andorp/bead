{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.NewTestScript (
    newTestScript
  , modifyTestScript
  ) where

import           Control.Arrow ((***))
import           Data.String (fromString)
import           Data.String.Utils (replace)

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

import           Bead.View.Snap.Content
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
  renderDynamicPagelet . withUserFrame s $ testScriptContent (Create cs)

postNewTestScript :: POSTContentHandler
postNewTestScript = do
  script' <- TestScript
    <$> (getParameter (stringParameter (fieldName testScriptNameField) "Test Script Name"))
    <*> (getParameter (stringParameter (fieldName testScriptDescField) "Test Script Description"))
    <*> (getParameter (stringParameter (fieldName testScriptNotesField) "Test Script Notes"))
    <*> (replaceCrlf <$> getParameter (stringParameter (fieldName testScriptScriptField) "Test Script"))
  ck <- CourseKey <$> getParameter (stringParameter (fieldName testScriptCourseKeyField) "Course Key")
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
  renderDynamicPagelet . withUserFrame s $
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

hasPageContent :: PageData -> IHtml
hasPageContent pd = do
  msg <- getI18N
  return $ do
  postForm (routeOf $ testScriptPage pd) $ H.div ! formDiv $ do
    H.div ! rightCell $ do
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Name "Name"
      textInput (fieldName testScriptNameField) 10 (testScriptName pd) ! fillDiv
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Description "Description"
      textInput (fieldName testScriptDescField) 10 (testScriptDesc pd) ! fillDiv
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Notes "Help for writing test cases"
      textAreaInput (fieldName testScriptNotesField) (testScriptNotes pd) ! (textAreaFillDiv 20)
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Script "Test script"
      textAreaInput (fieldName testScriptScriptField) (testScriptScript pd)
        ! (textAreaFillDiv 50)
        ! (A.acceptCharset "utf-8")
    H.div ! leftCell $ do
      H.b $ fromString . msg $ Msg_NewTestScript_Course "Course:"
      H.br
      testScriptCourse msg pd
      H.br
      submitButton
        (fieldName testScriptSaveButton)
        (fromString . msg $ Msg_NewTestScript_Save "Commit")
  where
    const2 = const . const
    const3 = const2 . const
    testScriptPage = pageDataCata (const (Pages.newTestScript ())) (\_name key _script -> Pages.modifyTestScript key ())
    testScriptName = pageDataCata (const Nothing) (const2 (Just . tsName))
    testScriptDesc = pageDataCata (const Nothing) (const2 (Just . tsDescription))
    testScriptNotes = pageDataCata (const Nothing) (const2 (Just . tsNotes))
    testScriptScript = pageDataCata (const Nothing) (const2 (Just . tsScript))
    testScriptCourse msg = pageDataCata
      (valueSelection (courseKeyMap id *** courseNameAndType) (fieldName testScriptCourseKeyField))
      (\courseName _key _script -> fromString courseName)
      where
        courseNameAndType c = concat
          [courseName c, " - ", courseTypeStr $ courseTestScriptType c]
        courseTypeStr = msg . testScriptTypeCata
          (Msg_TestScriptTypeSimple "Textual")
          (Msg_TestScriptTypeZipped "Binary")

-- CSS Section

slimLeftCell  = A.style "float: left;  width: 30%; height: 5%"
slimRightCell h = A.style (fromString $ concat ["float: right; width: 68%; height: ", show h, "%"])
leftCell      = A.style "float: left;  width: 30%; height: 30%"
rightCell     = A.style "float: right; width: 68%; height: 99%"
fillDiv       = A.style "width: 99%"
textAreaFillDiv h = A.style (fromString $ concat ["width: 99%; height: ", show h,"%"])
formDiv       = A.style "width: 100%; height: 600px"
boldText      = A.style "font-weight: bold"

-- Helper

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"
