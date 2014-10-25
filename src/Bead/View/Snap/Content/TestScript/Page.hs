{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.TestScript.Page (
    newTestScript
  , modifyTestScript
  ) where

import           Control.Arrow ((***))
import           Data.Monoid
import           Data.String (fromString)
import           Data.String.Utils (replace)

--import           Text.Blaze.Html5 ((!))
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
  renderBootstrapPage . bootStrapUserFrame s $ testScriptContent (Create cs)

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
  renderBootstrapPage . bootStrapUserFrame s $
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

{-
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
      (selection (fieldName testScriptCourseKeyField) . map (id *** courseNameAndType))
      (\courseName _key _script -> fromString courseName)
      where
        courseNameAndType c = concat
          [courseName c, " - ", courseTypeStr $ courseTestScriptType c]
        courseTypeStr = msg . testScriptTypeCata
          (Msg_TestScriptTypeSimple "Textual")
          (Msg_TestScriptTypeZipped "Binary")
-}

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

hasPageContent :: PageData -> IHtml
hasPageContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ Bootstrap.colMd12 $ hr
    Bootstrap.row $ Bootstrap.colMd12 $ Bootstrap.pageHeader $ h1 "Test Script"
    Bootstrap.row $ Bootstrap.colMd12 $ H.form $ do
      postForm (routeOf $ testScriptPage pd) $ do
--        Bootstrap.textInput 
{-
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Name "Name"
      textInput (fieldName testScriptNameField) 10 (testScriptName pd) ! fillDiv
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Description "Description"
      textInput (fieldName testScriptDescField) 10 (testScriptDesc pd) ! fillDiv
      H.span ! boldText $ fromString . msg $ Msg_NewTestScript_Notes "Help for writing test cases"
      textAreaInput (fieldName testScriptNotesField) (testScriptNotes pd) ! (textAreaFillDiv 20)
-}
--        ! (textAreaFillDiv 50)
--        ! (A.acceptCharset "utf-8")
        Bootstrap.textInput
          (fieldName testScriptNameField)
          (fromString . msg $ Msg_NewTestScript_Name "Name")
          (maybe mempty fromString $ testScriptName pd)
--        H.div ! class_ "form-group" $ do
--          H.label ! for "exampleInputEmail1" $ "Title"
--          input ! class_ "form-control" ! A.id "exampleInputEmail1" ! placeholder "Enter title" ! type_ "text"
--          p ! class_ "help-block" $ "TODO: Test Script explanation"
        Bootstrap.textInput
          (fieldName testScriptDescField)
          (fromString . msg $ Msg_NewTestScript_Description "Description")
          (maybe mempty fromString $ testScriptDesc pd)

--      textInput  10 (testScriptDesc pd) ! fillDiv
--        H.div ! class_ "form-group" $ do
--          H.label ! for "exampleInputEmail1" $ "Description"
--          input ! class_ "form-control" ! A.id "exampleInputEmail1" ! placeholder "Enter description" ! type_ "text"

        H.div ! class_ "form-group" $ do
          H.label ! for "exampleInputEmail1" $ "Test Script"
          select ! class_ "combobox form-control" ! A.style "display:none" ! A.id "CountrySelection" $ do
            option ! value "" ! selected "selected" $ "Select Test Script"
            option ! value "TestScript1" $ "Test Script 1"
            option ! value "TestScript2" $ "Test Script 2"

        Bootstrap.textArea
          (fieldName testScriptScriptField)
          (fromString . msg $ Msg_NewTestScript_Script "Test script")
          (maybe mempty fromString $ testScriptScript pd)

        Bootstrap.textArea
          (fieldName testScriptNotesField)
          (fromString . msg $ Msg_NewTestScript_Notes "Help for writing test cases")
          (maybe mempty fromString $ testScriptNotes pd)

--        H.div ! class_ "form-group" $ do
--          H.label ! for "exampleInputPassword1" $ "Help for writing test cases"
--          textarea ! class_ "form-control" ! A.id "exampleInputPassword1" ! placeholder "" ! rows "20" $ "Help for writing test cases"
--        H.div ! class_ "form-group" $ do
--          H.label ! for "exampleInputPassword1" $ "Test Script"
--          textarea ! class_ "form-control" ! A.id "exampleInputPassword1" ! placeholder "" ! rows "20" $ "Text of the test script"
      Bootstrap.row $ Bootstrap.colMd12 $
        submitButton (fieldName testScriptSaveButton)
        (fromString . msg $ Msg_NewTestScript_Save "Commit")

      Bootstrap.row $ Bootstrap.colMd12 $ hr

    Bootstrap.turnSelectionsOn
--    script ! type_ "text/javascript" $ "//\n$(document).ready(function(){\n$('.combobox').combobox()\n});\n//"
  where
--    const2 = const . const
--    const3 = const2 . const
    testScriptPage = pageDataCata (const (Pages.newTestScript ())) (\_name key _script -> Pages.modifyTestScript key ())
    testScriptName = pageDataCata (const Nothing) (const2 (Just . tsName))
    testScriptDesc = pageDataCata (const Nothing) (const2 (Just . tsDescription))
    testScriptNotes = pageDataCata (const Nothing) (const2 (Just . tsNotes))
    testScriptScript = pageDataCata (const Nothing) (const2 (Just . tsScript))
    testScriptCourse msg = pageDataCata
      (selection (fieldName testScriptCourseKeyField) . Prelude.map (Prelude.id *** courseNameAndType))
      (\courseName _key _script -> fromString courseName)
      where
        courseNameAndType c = concat
          [courseName c, " - ", courseTypeStr $ courseTestScriptType c]
        courseTypeStr = msg . testScriptTypeCata
          (Msg_TestScriptTypeSimple "Textual")
          (Msg_TestScriptTypeZipped "Binary")
