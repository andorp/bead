{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.SitePages where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.PageObject
import Test.WebDriver.Tools
import Test.WebDriver.Support

import Bead.Domain.Entities
import Bead.Domain.Evaluation as E hiding (EvaluationData)
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Fay.Hooks
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
    doesFieldExist regEmailAddress <&&>
    doesFieldExist regFullName
  failureMsg = const "Registration page"

instance PageAction RegistrationData where
  action d = do
    (sendKeysStr (rUsername d)) <@> loginUsername
    (sendKeysStr (rPassword d)) <@> loginPassword
    (sendKeysStr (rEmail    d)) <@> regEmailAddress
    (sendKeysStr (rFullName d)) <@> regFullName
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
    -- failsOnFalse "Assign Button is missing" (doesElementExist assignBtn)
    failsOnFalse "Select Button is missing" (doesElementExist selectBtn)
    failsOnFalse "Username field is missing" (doesFieldExist usernameField)
    return True
  failureMsg = const "Administration page"

selectUserToModify :: String -> Test ()
selectUserToModify u = do
  (sendKeysStr u) <@> usernameField
  click <@> selectBtn

-- Represents an assignment evaluation type, that
-- the user can select on the administration page
data AssignmentEvalType
  = EvalBinary
  | EvalPercent

assignmentEvalTypeFold :: a -> a -> AssignmentEvalType -> a
assignmentEvalTypeFold b _ EvalBinary  = b
assignmentEvalTypeFold _ p EvalPercent = p

data CourseData = CourseData {
    cName :: String
  , cDesc :: String
  , cEval :: AssignmentEvalType
  }

createCourse :: CourseData -> Test ()
createCourse c = do
  sendKeysStr (cName c) <@> courseNameField
  sendKeysStr (cDesc c) <@> courseDescField
  let courseEvalField = FieldName . evSelectionId $ createCourseHook
  courseType <- createSelect courseEvalField "No course type selection was found"
  selectByVisibleText courseType (assignmentEvalTypeFold "Binary" "Percentage" (cEval c))
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
    return True
  failureMsg = const "Group Admin page"

data GroupData = GroupData {
    gdCourseName :: String
  , gdGroupName :: String
  , gdGroupDesc :: String
  , gdEval :: AssignmentEvalType
  }

createGroup :: GroupData -> Test ()
createGroup g = do
  courses <- createSelect courseKeyInfo "No course selection field is not found"
  selectByVisibleText courses (fromString . gdCourseName $ g)
  sendKeysStr (gdGroupName g) <@> groupNameField
  sendKeysStr (gdGroupDesc g) <@> groupDescField
  let groupEvalField = FieldName . evSelectionId $ createGroupHook
  groupType <- createSelect groupEvalField "No group type selection was found"
  selectByVisibleText groupType (assignmentEvalTypeFold "Binary" "Percentage" (gdEval g))
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
  users <- createSelect selectedGroupAdmin "Group admin user field is not found"
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
  -- TODO
  return ()

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
    setStartDate
    setEndDate
    gc <- case aType a of
      GroupAsg  -> createSelect selectedGroup  "No group selection is found"
      CourseAsg -> createSelect selectedCourse "No course selection is found"
    selectByVisibleText gc (fromString . aGroupOrCourse $ a)
    click <@> saveSubmitBtn
    where
      setStartDate :: Test ()
      setStartDate = do
        startDiv <- findElem (ById . fromString $ "start-date-div")
        i  <- findElemFrom startDiv (ByClass . fromString $ "hasDatepicker")
        click i
        e  <- findElem (ById . fromString $ "ui-datepicker-div")
        dp <- failsOnNothing "No datepicker is found" (datepicker e)
        selectDay 1 dp

      setEndDate :: Test ()
      setEndDate = do
        endDiv <- findElem (ById . fromString $ "end-date-div")
        i  <- findElemFrom endDiv (ByClass . fromString $ "hasDatepicker")
        click i
        e  <- findElem (ById . fromString $ "ui-datepicker-div")
        dp <- failsOnNothing "No datepicker is found" (datepicker e)
        nextMonth dp
        selectDay 28 dp


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
  as <- filterM assignmentLine =<< (liftM tail . rows) =<< tableById availableAssignmentsTable
  when (null as) . failed . join $ ["No assignment link was found: ", gName, " ", aName]
  when (length as > 1) . failed . join $ ["More than one assignment were found: ", gName, " ", aName]
  return . head $ as

  where
    assignmentLine :: Element -> Test Bool
    assignmentLine e = do
      cells <- mapM getText =<< findElemsFrom e (ByTag "td")
      return $
        ((at cells 1 ("Group name is not found: " ++ gName)) == (fromString gName)) &&
        ((at cells 3 ("Assignment name is not found: " ++ aName)) == (fromString aName))

rowLink :: Element -> Int -> Test Element
rowLink e i = do
  cells <- findElemsFrom e (ByTag "td")
  findElemFrom (at cells i "Row link, tag a is not found") (ByTag "a")

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
    click =<< link (at rs (slNo s) "Submission list data")
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
      "No evaluation table was found"
      (null <$> findElems (ByClass . className $ groupSubmissionTable))
    return True
  failureMsg = const "Evaluation"

isHeader :: Element -> Test Bool
isHeader e = (not . null) <$> findElemsFrom e (ByTag "th")

instance PageAction SelectSubmissionData where
  action s = do
    ts <- tables groupSubmissionTable
    t <- failsOnNothing "No evaluation table was found for the group" $ findM isGroup ts
    rs <- rows t
    r <- failsOnNothing "Student was not found" $ findM isStudentRow rs
    ls <- findElemsFrom r (ByTag "a")
    click (at ls (sNo s) "Select submission is not found")
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
            ((fromString . sStudent $ s) ==) <$> getText (at cs 0 "Student text is not found")

-- * User's submission

data UserSubmissionsData = UserSubmissionsData {
    usNo :: Int
  }

instance PageObject UserSubmissionsData where
  precondition = const $ do
    failsOnTrue
      "No submission table was found"
      (null <$> (findElems (ByClass . className $ userSubmissionTable)))
    return True
  failureMsg = const "User's submission page"

instance PageAction UserSubmissionsData where
  action u = do
    t <- failsOnNothing
           "No user's submission table"
           (tableByClass userSubmissionTable)
    rs <- filterM (fmap not . isHeader) =<< rows t
    click =<< findElemFrom (at rs (usNo u) "User submissions data") (ByTag "a")

-- * Evaluation

data EvaluationData = EvaluationData {
    evMessage :: String
  , evValue   :: EvaluationResult
  }

instance PageObject EvaluationData where
  precondition = const $ do
    failsOnFalse "Evaluation field was not found" (doesFieldExist evaluationValueField)
    failsOnFalse "Evaluation result field was not found" (doesFieldExist evaluationResultField)
    failsOnFalse "Save button was not found" (doesElementExist saveEvalBtn)
    return True
  failureMsg = const "Evaluation page"

instance PageAction EvaluationData where
  action e = do
    setResult . evValue $ e
    sendKeysStr (evMessage e) <@> evaluationValueField
    click <@> saveEvalBtn
    where
      visible :: Binary -> String
      visible (Binary E.Passed) = "Passed"
      visible (Binary E.Failed) = "Failed"

      setResult :: EvaluationResult -> Test ()
      setResult (BinEval v) = do
        r <- createSelect evaluationResultField "No result selection was found"
        selectByVisibleText r (fromString . visible $ v)

      setResult p@(PctEval cfg) = do
        t <- createTextInput evaluationResultField "No result input was found"
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

at :: [a] -> Int -> String -> a
at xs p msg
  | p < length xs = xs !! p
  | otherwise = error msg
