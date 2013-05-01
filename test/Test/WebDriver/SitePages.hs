{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.SitePages where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.PageObject
import Test.WebDriver.Tools
import Test.WebDriver.Support.Select
import Test.WebDriver.Support.Table

import Bead.Domain.Entities
import Bead.View.Snap.TemplateAndComponentNames
import Bead.Controller.Pages

import Data.String
import Data.Maybe
import Data.List (find)
import Control.Monad
import Control.Applicative ((<$>))

import Data.Text (unpack)

-- * Login and logout

data LoginData = LoginData {
    lUsername :: String
  , lPassword :: String
  }

instance PageObject LoginData where
  precondition = const $
    doesFieldExist loginUsername <&&>
    doesFieldExist loginPassword <&&>
    doesFieldExist loginSubmitBtn
  failureMsg = const "Login page"

instance PageAction LoginData where
  action l = do
    (sendKeysStr (lUsername l)) <@> loginUsername
    (sendKeysStr (lPassword l)) <@> loginPassword
    click <@> loginSubmitBtn

data LogoutData = LogoutData

logout = LogoutData

instance PageObject LogoutData where
  precondition = const $ doesFieldExist Logout
  failureMsg   = const "Logout page"

instance PageAction LogoutData where
  action = const $ click <@> Logout

-- * Registration

data RegistrationData = RegistrationData {
    rUsername :: String
  , rPassword :: String
  , rEmail    :: String
  , rFullName :: String
  }

loginInfo :: RegistrationData -> LoginData
loginInfo r = LoginData {
    lUsername = rUsername r
  , lPassword = rPassword r
  }

instance PageObject RegistrationData where
  precondition = const $
    doesFieldExist loginUsername <&&>
    doesFieldExist loginPassword <&&>
    doesFieldExist registrationEmailAddress <&&>
    doesFieldExist registrationFamilyName
  failureMsg = const "Registration page"

instance PageAction RegistrationData where
  action d = do
    (sendKeysStr (rUsername d)) <@> loginUsername
    (sendKeysStr (rPassword d)) <@> loginPassword
    (sendKeysStr (rEmail    d)) <@> registrationEmailAddress
    (sendKeysStr (rFullName d)) <@> registrationFamilyName
    click <@> regSubmitBtn

-- * Home page

data HomePage = HomePage {
    hpUsername :: String
  }

class HomePageInfo d where
  homePage :: d -> HomePage

homePageUsr :: String -> HomePage
homePageUsr u = HomePage {
    hpUsername = u
  }

instance HomePageInfo LoginData where
  homePage = homePageUsr . lUsername

testHeader :: String -> TWD ()
testHeader usr = return ()

testMenu :: Page -> TWD ()
testMenu p = return ()

instance PageObject HomePage where
  precondition h = do
    testHeader (hpUsername h)
    testMenu   Home
    return True
  failureMsg = const "Home page"

instance PageAction HomePage where
  action = const $ return ()

-- * Administration

data AdminData = AdminData

instance PageObject AdminData where
  precondition = const $ do
    failsOnFalse "Create Course Button is missing" (doesElementExist createCourseBtn)
    failsOnFalse "Assign Button is missing" (doesElementExist assignBtn)
    failsOnFalse "Select Button is missing" (doesElementExist selectBtn)
    failsOnFalse "Username field is missing" (doesFieldExist usernameField)
    return True
  failureMsg = const "Administration page"

selectUserToModify :: String -> TWD ()
selectUserToModify u = do
  (sendKeysStr u) <@> usernameField
  click <@> selectBtn

data CourseData = CourseData {
    cName :: String
  , cDesc :: String
  }

createCourse :: CourseData -> TWD ()
createCourse c = do
  sendKeysStr (cName c) <@> courseNameField
  sendKeysStr (cDesc c) <@> courseDescField
  click <@> createCourseBtn


setCourseAdmin :: String -> String -> TWD ()
setCourseAdmin courseAdmin course = do
  users   <- createSelect selectedCourseAdmin "No users selection was found"
  courses <- createSelect selectedCourse      "No courses selection was found"
  selectByValue users (fromString courseAdmin)
  selectByVisibleText courses (fromString course)
  click <@> assignBtn

-- * Modify user's details

data UserDetailsData = UserDetailsData {
    udRole :: Role
  }

instance PageObject UserDetailsData where
  precondition = const $ do
    failsOnFalse "Email field is missing" (doesFieldExist userEmailField)
    failsOnFalse "Family name is missing" (doesFieldExist userFamilyNameField)
    failsOnFalse "User role field is missing" (doesFieldExist userRoleField)
    failsOnFalse "Save changes button does not exist" (doesElementExist saveChangesBtn)
    return True
  failureMsg = const "User detaials page"

instance PageAction UserDetailsData where
  action d = do
    s <- createSelect userRoleField "No Rule selection was found"
    selectByValue s (fromString . show . udRole $ d)
    click <@> saveChangesBtn

-- * Course Admin

data CourseAdminPage = CourseAdminPage

instance PageObject CourseAdminPage where
  precondition = const $ do
    failsOnFalse "Course key field is not found" (doesFieldExist courseKeyInfo)
    failsOnFalse "Group name field is not found" (doesFieldExist groupNameField)
    failsOnFalse "Group description field is not found" (doesFieldExist groupDescField)
    failsOnFalse "Create Group Button is not found" (doesElementExist createGroupBtn)
    failsOnFalse "Group selection is not found" (doesFieldExist selectedGroup)
    failsOnFalse "Group admin selection is not found" (doesFieldExist selectedProfessor)
    failsOnFalse "Assign button is not found" (doesElementExist assignGroupAdminBtn)
    return True
  failureMsg = const "Group Admin page"

data GroupData = GroupData {
    gdCourseName :: String
  , gdGroupName :: String
  , gdGroupDesc :: String
  }

createGroup :: GroupData -> TWD ()
createGroup g = do
  s <- createSelect courseKeyInfo "No course selection field is not found"
  selectByVisibleText s (fromString . gdCourseName $ g)
  sendKeysStr (gdGroupName g) <@> groupNameField
  sendKeysStr (gdGroupDesc g) <@> groupDescField
  click <@> createGroupBtn

checkSelectionVisibleText :: (SnapFieldName s) => s -> String -> TWD ()
checkSelectionVisibleText s v = do
  os <- options =<< (createSelect s $ "No selection was found: " ++ fieldName s)
  e <- elem (fromString v) <$> mapM getText os
  unless e $ failed "No text was found in the selection"

checkGroupSelection :: GroupData -> TWD ()
checkGroupSelection g = checkSelectionVisibleText selectedGroup (gdGroupName g)

setGroupAdmin :: String -> String -> TWD ()
setGroupAdmin groupAdmin g = do
  users <- createSelect selectedProfessor "Group admin user field is not found"
  groups <- createSelect selectedGroup "Group field is not found"
  selectByValue users (fromString groupAdmin)
  selectByVisibleText groups (fromString g)
  click <@> assignGroupAdminBtn

tables :: (SnapClassName c) => c -> TWD [Table]
tables c = do
  tableElements <- findElems (ByClass . className $ c)
  tables <- mapM table tableElements
  unless (and . map isJust $ tables) $
    failed $ join ["At least one element with ",className c," class was not a table"]
  return . map fromJust $ tables

tableById :: (SnapFieldName f) => f -> TWD Table
tableById i = do
  tableElements <- findElems (ById . fieldName $ i)
  when (length tableElements /= 1) $ failed "Zero or more than selected table were found"
  t <- table (head tableElements)
  when (isNothing t) $ failed "Element was not a table"
  return . fromJust $ t

findGroupSubmissionTable :: String -> TWD ()
findGroupSubmissionTable g = do
  ts <- tables groupSubmissionTable
  tableHeaders <- concat <$> (mapM headers ts)
  when (null tableHeaders) $ failed "No header was found"
  tableHeaderTexts <- mapM getText tableHeaders
  unless (isJust . find (fromString g ==) $ tableHeaderTexts) $
    failed $ "No submission table was found with header: " ++ g

findGroupAssignmentInTable :: String -> String -> TWD ()
findGroupAssignmentInTable g a = do
  ts <- tables assignmentTable
  when (null ts) $
    failed "No assignment table was found"
  let t = head ts
  rs <- rows t
  when (null rs) $
    failed "No rows was found in the table"
  found <- or <$> mapM foundAssignmentLine rs
  unless found $
    failed "Assignment was not found"
    where
      foundAssignmentLine e = do
        texts <- mapM getText =<< findElemsFrom e (ByTag "td")
        let foundGroup = elem (fromString g) texts
            foundAsg   = elem (fromString a) texts
        return (foundGroup && foundAsg)

-- * Assignment creation

data AsgType = GroupAsg | CourseAsg

data AssignmentData = AssignmentData {
    aType :: AsgType
  , aGroupOrCourse :: String
  , aName :: String
  , aDesc :: String
  , aTCs  :: String
  , aEv     :: EvaulationType
  , asgType :: AssignmentType
  , aStartDate :: String
  , aEndDate   :: String
  }

instance PageObject AssignmentData where
  precondition a = do
    failsOnFalse "There is no save button" (doesFieldExist saveSubmitBtn)
    failsOnFalse "Assignment name is not found" (doesFieldExist assignmentNameField)
    failsOnFalse "Assignment description is not found" (doesFieldExist assignmentDescField)
    failsOnFalse "Assignment test cases are not found" (doesFieldExist assignmentTCsField)
    failsOnFalse "Assignment type is not found" (doesFieldExist assignmentTypeField)
    failsOnFalse "Assignment evaulation is not found" (doesFieldExist assignmentEvField)
    failsOnFalse "Assignment start field is not found" (doesFieldExist assignmentStartField)
    failsOnFalse "Assignment end field is not found" (doesFieldExist assignmentEndField)
    case aType a of
      GroupAsg  -> failsOnFalse "Assignment group selection is not found"  (doesFieldExist selectedGroup)
      CourseAsg -> failsOnFalse "Assignment course selection is not found" (doesFieldExist selectedCourse)
  failureMsg _ = "Create assignment"

instance PageAction AssignmentData where
  action a = do
    sendKeysStr (aName a) <@> assignmentNameField
    sendKeysStr (aDesc a) <@> assignmentDescField
    sendKeysStr (aTCs  a) <@> assignmentTCsField
    types <- createSelect assignmentTypeField "No assignment selection was found"
    selectByValue types (fromString . show . asgType $ a)
    evals <- createSelect assignmentEvField "No assignment evaulation was found"
    selectByValue evals (fromString . show . aEv $ a)
    sendKeysStr (aStartDate a) <@> assignmentStartField
    sendKeysStr (aEndDate a) <@> assignmentEndField
    gc <- case aType a of
      GroupAsg  -> createSelect selectedGroup  "No group selection is found"
      CourseAsg -> createSelect selectedCourse "No course selection is found"
    selectByVisibleText gc (fromString . aGroupOrCourse $ a)
    click <@> saveSubmitBtn

-- * Group Registration

data GroupRegData = GroupRegData {
    grName :: String
  }

instance PageObject GroupRegData where
  precondition = const $ do
    failsOnFalse "Group registration button was not found" (doesElementExist regGroupSubmitBtn)
    failsOnFalse "Group registration selection was not found" (doesFieldExist groupRegistrationField)
    return True
  failureMsg _ = "Group Registraion"

instance PageAction GroupRegData where
  action g = do
    groups <- createSelect groupRegistrationField "No group selection was not found"
    selectByVisibleText groups (fromString . grName $ g)
    click <@> regGroupSubmitBtn

-- * Submit solution

clickNewSolution :: String -> String -> TWD ()
clickNewSolution gName aName = do
  l <- mapM link =<< filterM assignmentLine =<< rows =<< tableById availableAssignmentsTable
  when (null l) . failed . join $ ["No assignment link was found: ", gName, " ", aName]
  when (length l > 1) . failed . join $ ["More than one assignment were found: ", gName, " ", aName]
  click . head $ l
  where
    assignmentLine :: Element -> TWD Bool
    assignmentLine e = do
      cells <- mapM getText =<< findElemsFrom e (ByTag "td")
      return $ ((cells !! 1) == (fromString gName)) && ((cells !! 3) == (fromString aName))

    link e = do
      cells <- findElemsFrom e (ByTag "td")
      findElemFrom (cells !! 0) (ByTag "a")

data SubmissionData = SubmissionData {
    sbm :: String
  }

instance PageObject SubmissionData where
  precondition = const $ do
    failsOnFalse "No submission field was found" (doesFieldExist submissionTextField)
    failsOnFalse "No submission button was found" (doesElementExist submitSolutionBtn)
  failureMsg = const "Submission"

instance PageAction SubmissionData where
  action s = do
    sendKeysStr (sbm s) <@> submissionTextField
    click <@> submitSolutionBtn

-- * Page Components

isPage :: Page -> TWD ()
isPage _ = return ()

-- * Tools

createSelect :: (SnapFieldName f) => f -> String -> TWD Select
createSelect field msg = (failsOnNothing msg . select) =<< (findField field)
