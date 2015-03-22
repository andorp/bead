{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assignment.Page (
    newGroupAssignment
  , newCourseAssignment
  , modifyAssignment
  , viewAssignment
  , newGroupAssignmentPreview
  , newCourseAssignmentPreview
  , modifyAssignmentPreview
  ) where

import           Control.Monad.Error
import qualified Data.Map as Map
import           Data.Time (getCurrentTime)

import qualified Bead.Controller.UserStories as S
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content
import           Bead.View.ContentHandler (getJSONParameters)
import           Bead.View.RequestParams

import           Bead.View.Content.Assignment.Data
import           Bead.View.Content.Assignment.View

import           Bead.View.Fay.Hooks

-- * Content Handlers

newCourseAssignment = ViewModifyHandler newCourseAssignmentPage postCourseAssignment
newGroupAssignment  = ViewModifyHandler newGroupAssignmentPage postGroupAssignment
modifyAssignment    = ViewModifyHandler modifyAssignmentPage postModifyAssignment
viewAssignment      = ViewHandler viewAssignmentPage
newCourseAssignmentPreview = UserViewHandler newCourseAssignmentPreviewPage
newGroupAssignmentPreview  = UserViewHandler newGroupAssignmentPreviewPage
modifyAssignmentPreview    = UserViewHandler modifyAssignmentPreviewPage

-- * Course Assignment

newCourseAssignmentPage :: GETContentHandler
newCourseAssignmentPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  (c, tss, ufs) <- userStory $ do
    S.isAdministratedCourse ck
    (course, _groupKeys) <- S.loadCourse ck
    tss' <- S.testScriptInfosOfCourse ck
    ufs  <- map fst <$> S.listUsersFiles
    return ((ck, course), nonEmptyList tss', ufs)
  now <- liftIO $ getCurrentTime
  tz <- userTimeZoneToLocalTimeConverter
  return $ newAssignmentContent $ PD_Course tz now c tss ufs

postCourseAssignment :: POSTContentHandler
postCourseAssignment = do
  CreateCourseAssignment
    <$> getParameter (customCourseKeyPrm (fieldName selectedCourse))
    <*> getAssignment
    <*> readTCCreation

newCourseAssignmentPreviewPage :: ViewPOSTContentHandler
newCourseAssignmentPreviewPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  assignment <- getAssignment
  tc <- readTCCreationParameters
  (c, tss, ufs) <- userStory $ do
    S.isAdministratedCourse ck
    (course, _groupKeys) <- S.loadCourse ck
    tss' <- S.testScriptInfosOfCourse ck
    ufs  <- map fst <$> S.listUsersFiles
    return ((ck, course), nonEmptyList tss', ufs)
  now <- liftIO $ getCurrentTime
  tz <- userTimeZoneToLocalTimeConverter
  return $ newAssignmentContent $
    PD_Course_Preview tz now c tss ufs assignment tc

-- Tries to create a TCCreation descriptive value. If the test script, usersfile and testcase
-- parameters are included returns Just tccreation otherwise Nothing
readTCCreation :: ContentHandler TCCreation
readTCCreation = do
  (mTestScript, mZippedTestCaseName, mPlainTestCase) <- readTCCreationParameters
  case tcCreation mTestScript mZippedTestCaseName mPlainTestCase of
    Left  e  -> throwError . strMsg $ "Some error in test case parameters " ++ e
    Right tc -> return tc

readTCCreationParameters :: ContentHandler TCCreationParameters
readTCCreationParameters = do
  mTestScript         <- getOptionalParameter (jsonParameter (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalOrNonEmptyParameter (jsonParameter (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  return (mTestScript, mZippedTestCaseName, mPlainTestCase)

tcCreation :: Maybe (Maybe TestScriptKey) -> Maybe UsersFile -> Maybe String -> Either String TCCreation
tcCreation Nothing        _ _ = Right NoCreation
tcCreation (Just Nothing) _ _ = Right NoCreation
tcCreation (Just (Just tsk)) (Just uf) _ = Right $ FileCreation tsk uf
tcCreation (Just (Just tsk)) _ (Just t)  = Right $ TextCreation tsk t
tcCreation (Just (Just _tsk)) Nothing Nothing = Left "#1"

readTCModificationParameters :: ContentHandler TCModificationParameters
readTCModificationParameters = do
  mTestScript         <- getOptionalParameter (jsonParameter (fieldName assignmentTestScriptField) "Test Script")
  mZippedTestCaseName <- getOptionalOrNonEmptyParameter (jsonParameter (fieldName assignmentUsersFileField) "Test Script File")
  mPlainTestCase      <- getOptionalParameter (stringParameter (fieldName assignmentTestCaseField) "Test Script")
  return (mTestScript,mZippedTestCaseName,mPlainTestCase)

readTCModification :: ContentHandler TCModification
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
newGroupAssignmentPage = do
  now <- liftIO $ getCurrentTime
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  (g,tss,ufs) <- userStory $ do
    S.isAdministratedGroup gk
    group <- S.loadGroup gk
    tss' <- S.testScriptInfosOfGroup gk
    ufs  <- map fst <$> S.listUsersFiles
    return ((gk, group), nonEmptyList tss', ufs)
  tz <- userTimeZoneToLocalTimeConverter
  return $ newAssignmentContent $ PD_Group tz now g tss ufs

postGroupAssignment :: POSTContentHandler
postGroupAssignment = do
  CreateGroupAssignment
  <$> getParameter (customGroupKeyPrm (fieldName selectedGroup))
  <*> getAssignment
  <*> readTCCreation

newGroupAssignmentPreviewPage :: ViewPOSTContentHandler
newGroupAssignmentPreviewPage = do
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  assignment <- getAssignment
  tc <- readTCCreationParameters
  (g,tss,ufs) <- userStory $ do
    S.isAdministratedGroup gk
    group <- S.loadGroup gk
    tss' <- S.testScriptInfosOfGroup gk
    ufs  <- map fst <$> S.listUsersFiles
    return ((gk, group), nonEmptyList tss', ufs)
  tz <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  return $ newAssignmentContent $
    PD_Group_Preview tz now g tss ufs assignment tc

-- * Modify Assignment

modifyAssignmentPage :: GETContentHandler
modifyAssignmentPage = do
  ak <- getAssignmentKey
  (as,tss,ufs,tc,ev) <- userStory $ do
    S.isAdministratedAssignment ak
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    ev   <- not <$> S.isThereASubmission ak
    return (as, nonEmptyList tss', ufs, tc, ev)
  tz <- userTimeZoneToLocalTimeConverter
  return $ newAssignmentContent $
    PD_Assignment tz ak as tss ufs tc ev

postModifyAssignment :: POSTContentHandler
postModifyAssignment = do
  ModifyAssignment
  <$> getAssignmentKey
  <*> getAssignment
  <*> readTCModification

modifyAssignmentPreviewPage :: ViewPOSTContentHandler
modifyAssignmentPreviewPage = do
  ak <- getAssignmentKey
  as <- getAssignment
  tm <- readTCModificationParameters
  (tss,ufs,tc,ev) <- userStory $ do
    S.isAdministratedAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ufs  <- map fst <$> S.listUsersFiles
    tc   <- S.testCaseOfAssignment ak
    ev   <- not <$> S.isThereASubmission ak
    return (nonEmptyList tss', ufs, tc, ev)
  tz <- userTimeZoneToLocalTimeConverter
  return $ newAssignmentContent $
    PD_Assignment_Preview tz ak as tss ufs tc tm ev

viewAssignmentPage :: GETContentHandler
viewAssignmentPage = do
  ak <- getAssignmentKey
  (as,tss,tc) <- userStory $ do
    S.isAdministratedAssignment ak
    as <- S.loadAssignment ak
    tss' <- S.testScriptInfosOfAssignment ak
    ts   <- S.testCaseOfAssignment ak
    return (as, tss', ts)
  tz <- userTimeZoneToLocalTimeConverter
  let ti = do (_tck, _tc, tsk) <- tc
              Map.lookup tsk $ Map.fromList tss
  return $ newAssignmentContent $ PD_ViewAssignment tz ak as ti tc

-- * Helpers

-- | Returns Nothing if the given list was empty, otherwise Just list
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

-- Get Assignment Value
getAssignment = do
  converter <- userTimeZoneToUTCTimeConverter
  startDate <- converter <$> getParameter assignmentStartPrm
  endDate   <- converter <$> getParameter assignmentEndPrm
  when (endDate < startDate) . throwError $ strMsg "The assignment starts later than it ends"
  pwd <- getParameter (stringParameter (fieldName assignmentPwdField) "Password")
  noOfTries <- getParameter (stringParameter (fieldName assignmentNoOfTriesField) "Number of tries")
  asp <- Assignment.aspectsFromList <$> getJSONParameters (fieldName assignmentAspectField) "Aspect parameter"
  stype <- getJSONParam (fieldName assignmentSubmissionTypeField) "Submission type"
  let asp1 = if stype == Assignment.TextSubmission
               then Assignment.clearZippedSubmissions asp
               else Assignment.setZippedSubmissions asp
  let asp2 = if Assignment.isPasswordProtected asp1
               then Assignment.setPassword pwd asp1
               else asp1
  let asp3 = if Assignment.isNoOfTries asp2
               then Assignment.setNoOfTries (read noOfTries) asp2
               else asp2
  Assignment.assignmentAna
    (getParameter (stringParameter (fieldName assignmentNameField) "Name"))
    (getParameter (stringParameter (fieldName assignmentDescField) "Description"))
    (return asp3)
    (return startDate)
    (return endDate)
    (getParameter (evalConfigPrm assignmentEvTypeHook))

getAssignmentKey = getParameter assignmentKeyPrm
