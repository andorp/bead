{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.SitePages where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.PageObject
import Test.WebDriver.Tools
import Test.WebDriver.Support

import Bead.Domain.Entities
import Bead.Domain.Shared hiding (EvaulationData)
import Bead.Domain.Evaulation as E
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

testHeader :: String -> Test ()
testHeader usr = return ()

testMenu :: Page -> Test ()
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

selectUserToModify :: String -> Test ()
selectUserToModify u = do
  (sendKeysStr u) <@> usernameField
  click <@> selectBtn

data CourseData = CourseData {
    cName :: String
  , cDesc :: String
  , cEval :: EvaulationConfig
  }

createCourse :: CourseData -> Test ()
createCourse c = do
  sendKeysStr (cName c) <@> courseNameField
  sendKeysStr (cDesc c) <@> courseDescField
--  sendKeysStr (show . cEval $ c) <@> courseEvalField TODO: This one is broken
  click <@> createCourseBtn


setCourseAdmin :: String -> String -> Test ()
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
  , gdEval :: EvaulationConfig
  }

createGroup :: GroupData -> Test ()
createGroup g = do
  s <- createSelect courseKeyInfo "No course selection field is not found"
  selectByVisibleText s (fromString . gdCourseName $ g)
  sendKeysStr (gdGroupName g) <@> groupNameField
  sendKeysStr (gdGroupDesc g) <@> groupDescField
  sendKeysStr (show . gdEval $ g) <@> groupEvalField
  click <@> createGroupBtn

checkSelectionVisibleText :: (SnapFieldName s) => s -> String -> Test ()
checkSelectionVisibleText s v = do
  os <- options =<< (createSelect s $ "No selection was found: " ++ fieldName s)
  e <- elem (fromString v) <$> mapM getText os
  unless e $ failed "No text was found in the selection"

checkGroupSelection :: GroupData -> Test ()
checkGroupSelection g = checkSelectionVisibleText selectedGroup (gdGroupName g)

setGroupAdmin :: String -> String -> Test ()
setGroupAdmin groupAdmin g = do
  users <- createSelect selectedProfessor "Group admin user field is not found"
  groups <- createSelect selectedGroup "Group field is not found"
  selectByValue users (fromString groupAdmin)
  selectByVisibleText groups (fromString g)
  click <@> assignGroupAdminBtn

tables :: (SnapClassName c) => c -> Test [Table]
tables c = do
  tableElements <- findElems (ByClass . className $ c)
  tables <- mapM table tableElements
  unless (and . map isJust $ tables) $
    failed $ join ["At least one element with ",className c," class was not a table"]
  return . map fromJust $ tables

tableByClass :: (SnapClassName c) => c -> Test (Maybe Table)
tableByClass = fmap listToMaybe . tables

tableById :: (SnapFieldName f) => f -> Test Table
tableById i = do
  tableElements <- findElems (ById . fieldName $ i)
  when (length tableElements /= 1) $ failed "Zero or more than selected table were found"
  t <- table (head tableElements)
  when (isNothing t) $ failed "Element was not a table"
  return . fromJust $ t

findGroupSubmissionTable :: String -> Test ()
findGroupSubmissionTable g = do
  ts <- tables groupSubmissionTable
  tableHeaders <- concat <$> (mapM headerCells ts)
  when (null tableHeaders) $ failed "No header was found"
  tableHeaderTexts <- mapM getText tableHeaders
  unless (isJust . find (fromString g ==) $ tableHeaderTexts) $
    failed $ "No submission table was found with header: " ++ g

findGroupAssignmentInTable :: String -> String -> Test ()
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

findAssignmentLine :: String -> String -> Test Element
findAssignmentLine gName aName = do
  as <- filterM assignmentLine =<< rows =<< tableById availableAssignmentsTable
  when (null as) . failed . join $ ["No assignment link was found: ", gName, " ", aName]
  when (length as > 1) . failed . join $ ["More than one assignment were found: ", gName, " ", aName]
  return . head $ as

  where
    assignmentLine :: Element -> Test Bool
    assignmentLine e = do
      cells <- mapM getText =<< findElemsFrom e (ByTag "td")
      return $ ((cells !! 1) == (fromString gName)) && ((cells !! 3) == (fromString aName))

rowLink :: Element -> Int -> Test Element
rowLink e i = do
  cells <- findElemsFrom e (ByTag "td")
  findElemFrom (cells !! i) (ByTag "a")

clickNewSolution :: String -> String -> Test ()
clickNewSolution gName aName = click =<< link =<< findAssignmentLine gName aName
  where link e = rowLink e 0

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

-- * New comment on submission

clickOnAssignment :: String -> String -> Test ()
clickOnAssignment gName aName = click =<< link =<< findAssignmentLine gName aName
  where link e = rowLink e 3

data SubmissionListData = SubmissionListData {
    slNo :: Int
  }

instance PageObject SubmissionListData where
  precondition = const $ do
    failsOnTrue
      "No submission table was not found"
      (null <$> findElems (ById . fieldName $ submissionTableName))
    return True
  failureMsg = const $ "Submission List"

instance PageAction SubmissionListData where
  action s = do
    rs <- rows =<< tableById submissionTableName
    click =<< link (rs !! (slNo s))
    where
      link e = rowLink e 0

data CommentData = CommentData {
    cmt :: String
  }

instance PageObject CommentData where
  precondition = const $ do
    failsOnFalse "No comment field was found"  (doesFieldExist commentValueField)
    failsOnFalse "No comment button was found" (doesElementExist commentBtn)
    return True
  failureMsg = const "Comment on solution"

instance PageAction CommentData where
  action c = do
    sendKeysStr (cmt c) <@> commentValueField
    click <@> commentBtn

-- * Select Submission

data SelectSubmissionData = SelectSubmissionData {
    sGroup   :: String
  , sStudent :: String
  , sNo      :: Int
  }

instance PageObject SelectSubmissionData where
  precondition = const $ do
    failsOnTrue
      "No evaulation table was found"
      (null <$> findElems (ByClass . className $ groupSubmissionTable))
    return True
  failureMsg = const "Evaulation"

isHeader :: Element -> Test Bool
isHeader e = (not . null) <$> findElemsFrom e (ByTag "th")

instance PageAction SelectSubmissionData where
  action s = do
    ts <- tables groupSubmissionTable
    t <- failsOnNothing "No evaulation table was found for the group" $ findM isGroup ts
    rs <- rows t
    r <- failsOnNothing "Student was not found" $ findM isStudentRow rs
    ls <- findElemsFrom r (ByTag "a")
    click (ls !! (sNo s))
    where
      isGroup t = do
        hs <- headerCells t
        when (null hs) $ failed "No header was found in the table"
        ((fromString . sGroup $ s) ==) <$> getText (head hs)

      isStudentRow e = do
        h <- isHeader e
        case h of
          True  -> return False
          False -> do
            cs <- findElemsFrom e (ByTag "td")
            when (null cs) $ failed "No cell was found in the student's row"
            ((fromString . sStudent $ s) ==) <$> getText (cs !! 0)

-- * User's submission

data UserSubmissionsData = UserSubmissionsData {
    usNo :: Int
  }

instance PageObject UserSubmissionsData where
  precondition = const $ do
    failsOnTrue
      "No submission table was found"
      (null <$> (findElems (ByClass . className $ userSubmissionClassTable)))
    return True
  failureMsg = const "User's submission page"

instance PageAction UserSubmissionsData where
  action u = do
    t <- failsOnNothing
           "No user's submission table"
           (tableByClass userSubmissionClassTable)
    rs <- filterM (fmap not . isHeader) =<< rows t
    click =<< findElemFrom (rs !! (usNo u)) (ByTag "a")

-- * Evaulation

data EvaulationData = EvaulationData {
    evMessage :: String
  , evValue   :: EvaulationResult
  }

instance PageObject EvaulationData where
  precondition = const $ do
    failsOnFalse "Evaulation field was not found" (doesFieldExist evaulationValueField)
    failsOnFalse "Evaulation result field was not found" (doesFieldExist evaulationResultField)
    failsOnFalse "Save button was not found" (doesElementExist saveEvalBtn)
    return True
  failureMsg = const "Evaulation page"

instance PageAction EvaulationData where
  action e = do
    setResult . evValue $ e
    sendKeysStr (evMessage e) <@> evaulationValueField
    click <@> saveEvalBtn
    where
      visible :: Binary -> String
      visible (Binary E.Passed) = "Passed"
      visible (Binary E.Failed) = "Failed"

      setResult :: EvaulationResult -> Test ()
      setResult (BinEval v) = do
        r <- createSelect evaulationResultField "No result selection was found"
        selectByVisibleText r (fromString . visible $ v)

      setResult p@(PctEval cfg) = do
        t <- createTextInput evaulationResultField "No result input was found"
        clearTextInput t
        enterText (fromString . show $ p) t

checkCommentOnPage :: CommentData -> Test ()
checkCommentOnPage _ = do
  return ()

-- * Page Components

isPage :: Page -> Test ()
isPage _ = return ()

-- * Tools

createSelect :: (SnapFieldName f) => f -> String -> Test Select
createSelect field msg = (failsOnNothing msg . select) =<< (findField field)

createTextInput :: (SnapFieldName f) => f -> String -> Test TextInput
createTextInput field msg = (failsOnNothing msg . textInput) =<< (findField field)
