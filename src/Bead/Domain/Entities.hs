{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entities where

import           Control.Applicative
import           Control.Monad (join)
import           Data.ByteString.Char8 (ByteString)
import           Data.Data
import           Data.List (findIndex)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime(..), LocalTime, timeZoneName)
import           Data.Time.Format (formatTime)
import qualified Data.Time as Time
import           System.Locale (defaultTimeLocale)

import           Bead.Domain.Types
import           Bead.Domain.Evaluation
import           Bead.View.Snap.Translation

#ifdef TEST
import           Test.Themis.Test hiding (testCaseCata)
import           Bead.Invariants (Invariants(..), UnitTests(..))
#endif

-- * Course, exams, exercises, solutions

data AssignmentType
  = Normal
  | Urn
  | NormalPwd String
  | BallotBoxPwd String
  deriving (Data, Eq, Read, Show, Typeable)

assignmentTypeCata
  normal
  urn
  normalpwd
  ballot
  a = case a of
    Normal -> normal
    Urn    -> urn
    NormalPwd pwd -> normalpwd pwd
    BallotBoxPwd pwd -> ballot pwd

withAssignmentType a normal urn normalpwd ballot =
  assignmentTypeCata normal urn normalpwd ballot a

-- Assignment asptect is a property of an assignment which
-- controls its visibility of start and end date, its controlls
-- over submission.
data AssignmentAspect
  = AaBallotBox -- Submission should not shown for the students only after the end of the dead line
  | AaPassword String -- The assignment is password protected
  deriving (Data, Eq, Show, Ord, Typeable)

assignmentAspect
  ballot
  pwd
  a = case a of
    AaBallotBox -> ballot
    AaPassword p -> pwd p

-- An assignment can have several aspects, which is a set
type AssignmentAspects = Set AssignmentAspect

assignmentAspects :: (Set AssignmentAspect -> a) -> AssignmentAspects -> a
assignmentAspects f x = f x

-- Calculates the assignment aspect set from the type of the assignment
assignmentTypeToAspects :: AssignmentType -> AssignmentAspects
assignmentTypeToAspects = assignmentTypeCata
  Set.empty -- Normal
  (Set.singleton AaBallotBox) -- Urn
  (\pwd -> Set.singleton (AaPassword pwd)) -- NormalPwd
  (\pwd -> Set.fromList [AaBallotBox, AaPassword pwd]) -- BallotBoxPwd

#ifdef TEST
assignmentTypeToAspectsTests = group "assignmentTypeToAspects" $ do
  test "Normal" $
    Equals Set.empty (assignmentTypeToAspects Normal) "Normal type was not converted correctly"
  test "Urn" $
    Equals (Set.fromList [AaBallotBox]) (assignmentTypeToAspects Urn) "Urn type was not converted correctly"
  test "NormalPwd" $
    Equals (Set.fromList [AaPassword "pwd"]) (assignmentTypeToAspects (NormalPwd "pwd")) "NormalPwd type was not converted correctly"
  test "BallotBoxPwd" $
    Equals (Set.fromList [AaBallotBox, AaPassword "pwd"])
           (assignmentTypeToAspects (BallotBoxPwd "pwd"))
           "BallowBoxPwd was not converted correctly"
#endif

isPasswordAspect = assignmentAspect False (const True)
isBallotBoxAspect = assignmentAspect True (const False)

#ifdef TEST
assignmentAspectPredTests = group "assignmentAspectPred" $ do
  test "Password aspect predicate" $
    Equals True (isPasswordAspect (AaPassword "pwd")) "Password aspect is not recognized"
  test "Ballow box aspect predicate" $
    Equals True (isBallotBoxAspect AaBallotBox) "Ballot box aspect is not recognized"
#endif

-- Returns True if the aspect set contains a password protected value
isPasswordProtectedAspects :: AssignmentAspects -> Bool
isPasswordProtectedAspects = not . Set.null . Set.filter isPasswordAspect

#ifdef TEST
isPasswordProtectedAspectsTests = group "isPasswordProtectedAspects" $ do
  test "Empty aspect set"
       (Equals False (isPasswordProtectedAspects Set.empty)
               "Empty set should not contain password")
  test "Non password aspects"
       (Equals False (isPasswordProtectedAspects (Set.fromList [AaBallotBox]))
               "Ballot box set should not contain password")
  test "Password aspect"
       (Equals True (isPasswordProtectedAspects (Set.fromList [AaPassword ""]))
               "Password aspect should be found")
  test "Password aspect within more aspects"
       (Equals True (isPasswordProtectedAspects (Set.fromList [AaPassword "", AaBallotBox]))
               "Password aspect should be found")
#endif

-- Convert assignment aspects into assignment type, the aspect set should contain only one password
assignmentAspectsToType :: AssignmentAspects -> AssignmentType
assignmentAspectsToType = assignmentAspects $ \as ->
  let f s | Set.null s = Normal
          | and [contains password s, contains ballotBox s] = BallotBoxPwd (getPassword s)
          | contains ballotBox s = Urn
          | contains password s = NormalPwd (getPassword s)
          | otherwise = error' $ show s
      password  = isPasswordAspect
      ballotBox = isBallotBoxAspect
      contains = aasContains
      getPassword = aasGetPassword
      error' msg = error $ "assignmentAspectToType: invalid input" ++ msg
  in f as

#ifdef TEST
assignmentAspectsToTypeTests = group "assignmentAspectsToType" $ do
  let aa = assignmentAspectsToType . assignmentTypeToAspects
  test "Normal" $ Equals Normal (aa Normal) "Normal was not converted correctly"
  test "Urn" $ Equals Urn (aa Urn) "Urn was not converted correctly"
  test "NormalPwd" $ Equals (NormalPwd "pwd") (aa (NormalPwd "pwd")) "NormalPwd was not converted correctly"
  test "BallotBoxPwd" $ Equals (BallotBoxPwd "pwd") (aa (BallotBoxPwd "pwd"))
                        "BallotBoxPwd was not converted correctly"
#endif

-- Calculates True if the assignment aspects set contains at least one elements
-- That satisfies the preduicate
aasContains :: (AssignmentAspect -> Bool) -> AssignmentAspects -> Bool
aasContains pred = assignmentAspects (not . Set.null . Set.filter pred)

-- Returns the first password found in the assignment aspects set, if there
-- is no such password throws an error
aasGetPassword :: AssignmentAspects -> String
aasGetPassword = assignmentAspects $ \as ->
  case (Set.toList . Set.filter isPasswordAspect $ as) of
    [] -> error $ "aasGetPassword: no password aspects was found"
    (pwd:_) -> assignmentAspect (error "aasGetPassword: no password aspect was filtered in") id pwd

-- | Assignment for the student
data Assignment = Assignment {
    assignmentName :: String
  , assignmentDesc :: String
  , assignmentType :: AssignmentType
  , assignmentStart :: UTCTime
  , assignmentEnd   :: UTCTime
  -- TODO: Number of maximum tries
  } deriving (Eq, Show)

-- | Template function for the assignment
assignmentCata f (Assignment name desc type_ start end) =
  f name desc type_ start end

-- | Template function for the assignment with flipped arguments
withAssignment a f = assignmentCata f a

assignmentAna name desc type_ start end =
  Assignment <$> name <*> desc <*> type_ <*> start <*> end

-- | Produces True if the given time is between the start-end time of the assignment
isActivePeriod :: Assignment -> UTCTime -> Bool
isActivePeriod a t = and [start <= t, t <= end]
  where
    start = assignmentStart a
    end   = assignmentEnd   a

-- | Solution for one exercise
data Submission = Submission {
    solution         :: String
  , solutionPostDate :: UTCTime
  } deriving (Eq, Show)

-- | Template function for submission
submissionCata f (Submission sub subPostDate) = f sub subPostDate

-- | Template function for submission with flipped arguments
withSubmission s f = submissionCata f s

-- Comment type basically indicates that who left the comment,
-- constructors are self explanatories
data CommentType
  = CT_Student
  | CT_GroupAdmin
  | CT_CourseAdmin
  | CT_Admin
  | CT_Evaluation
  | CT_TestAgent
  | CT_Message   -- Highlighted message to the student
  deriving (Data, Eq, Read, Show, Typeable)

commentTypeCata
  student
  groupAdmin
  courseAdmin
  admin
  evaluation
  testAgent
  message
  c = case c of
    CT_Student     -> student
    CT_GroupAdmin  -> groupAdmin
    CT_CourseAdmin -> courseAdmin
    CT_Admin       -> admin
    CT_Evaluation  -> evaluation
    CT_TestAgent   -> testAgent
    CT_Message     -> message

-- | Comment on the text of exercise, on the evaluation
data Comment = Comment {
    comment       :: String
  , commentAuthor :: String
  , commentDate   :: UTCTime
  , commentType   :: CommentType
  } deriving (Eq, Show)

commentCata f (Comment c a d t) = f c a d t

commentAna comment author date type_ =
  Comment <$> comment <*> author <*> date <*> type_

-- Returns True if the comment can be displayed for the student
-- otherwise false
isStudentComment :: Comment -> Bool
isStudentComment = commentCata $ \_comment _owner _date -> student where
  student = commentTypeCata
    True  -- Student
    True  -- Group Admin
    True  -- Course Admin
    False -- Admin
    True  -- Evaluation
    False -- Test Agent
    True  -- Message

-- Returns True if the comment is a Message comment from the test agent
-- otherwise False
isMessageComment :: Comment -> Bool
isMessageComment = commentCata $ \_comment _owner _date -> message where
  message = commentTypeCata
    False -- student
    False -- groupAdmin
    False -- courseAdmin
    False -- admin
    False -- evaluation
    False -- testAgent
    True  -- message

type CourseName = String

type UsersFullname = String

type EvaluationResult = EvaluationData Binary Percentage

evaluationResultCata
  binary
  percentage
  e = case e of
    BinEval b -> binary b
    PctEval p -> percentage p

type EvaluationConfig = EvaluationData () PctConfig

binaryEvalConfig :: EvaluationConfig
binaryEvalConfig = BinEval ()

percentageEvalConfig :: PctConfig -> EvaluationConfig
percentageEvalConfig p = PctEval p

class IsEvaluationResult e where
  mkEvalResult :: e -> EvaluationResult

instance IsEvaluationResult Binary where
  mkEvalResult = BinEval

instance IsEvaluationResult Percentage where
  mkEvalResult = PctEval

allBinaryEval :: [EvaluationData b p] -> Maybe [b]
allBinaryEval = sequence . map binaryEval

allPercentEval :: [EvaluationData b p] -> Maybe [p]
allPercentEval = sequence . map percentEval

evaluateResults :: EvaluationConfig -> [EvaluationResult] -> Maybe Result
evaluateResults (BinEval cfg) = fmap (flip calculateEvaluation cfg) . allBinaryEval
evaluateResults (PctEval cfg) = fmap (flip calculateEvaluation cfg) . allPercentEval

-- | Evaluation of a submission
data Evaluation = Evaluation {
    evaluationResult  :: EvaluationResult
  , writtenEvaluation :: String
  } deriving (Eq, Show)

-- | Template function for the evaluation
evaluationCata f (Evaluation result written) = f result written

-- | Template function with flipped parameter for the evaluation
withEvaluation e f = evaluationCata f e

resultString :: EvaluationResult -> TransMsg
resultString (BinEval (Binary Passed)) = TransMsg $ Msg_Domain_EvalPassed "Passed"
resultString (BinEval (Binary Failed)) = TransMsg $ Msg_Domain_EvalFailed "Failed"
resultString (PctEval p) =
  case point p of
    Nothing -> TransMsg $ Msg_Domain_EvalNoResultError "No evaluation result, some internal error happened!"
    Just q  -> TransPrmMsg (Msg_Domain_EvalPercentage "%s%%") (show . round $ 100.0 * q)

evaluationComment :: I18N -> UTCTime -> User -> Evaluation -> Comment
evaluationComment i t u e = Comment {
    comment = c
  , commentAuthor = u_name u
  , commentDate = t
  , commentType = CT_Evaluation
  } where
     c = join [
           writtenEvaluation e, "\r\n"
         , translateMessage i . resultString . evaluationResult $ e
         ]

newtype CourseCode = CourseCode String
  deriving (Eq, Ord, Show)

instance Str CourseCode where
  str (CourseCode s) = s

-- Course or Group info. Some information is attached to
-- course or group
data CGInfo a
  = CourseInfo a
  | GroupInfo a
  deriving (Show)

-- Template function for the course or group info value
cgInfoCata
  course
  group
  cg = case cg of
    CourseInfo x -> course x
    GroupInfo  x -> group  x

-- | A course represent a university course
data Course = Course {
    courseName :: String
  , courseDesc :: String
  , courseEvalConfig :: EvaluationConfig
  , courseTestScriptType :: TestScriptType
  } deriving (Eq, Show)

courseCata config script course (Course name desc cfg scriptType)
  = course name desc (config cfg) (script scriptType)

courseAppAna name desc eval test =
  Course <$> name <*> desc <*> eval <*> test

-- | Groups are registered under the courses
data Group = Group {
    groupName  :: String
  , groupDesc  :: String
  , groupEvalConfig :: EvaluationConfig
  } deriving (Eq, Show)

groupCata config group (Group name desc cfg)
  = group name desc (config cfg)

-- | Workflows can happen to exams
data Workflow
  = W_Created
  | W_Open
  | W_Closed
  | W_Expired
  deriving (Eq, Show)

-- * Authorization and authentication

-- | Login roles
data Role
  = Student
  | GroupAdmin
  | CourseAdmin
  | Admin
  deriving (Data, Enum, Eq, Ord, Show, Typeable)

roleCata
  student
  groupAdmin
  courseAdmin
  admin
  r = case r of
    Student     -> student
    GroupAdmin  -> groupAdmin
    CourseAdmin -> courseAdmin
    Admin       -> admin

roles = [Student, GroupAdmin, CourseAdmin, Admin]

-- Decides if the given role can admninstrate groups
-- Returns True if yes, otherwise False
groupAdmin = roleCata
  False
  True
  True
  False

data OutsideRole
  = EmptyRole
  | RegRole
  | TestAgentRole
  deriving (Eq, Ord)

parseRole :: String -> Maybe Role
parseRole "Student"      = Just Student
parseRole "Group Admin"  = Just GroupAdmin
parseRole "Course Admin" = Just CourseAdmin
parseRole "Admin"        = Just Admin
parseRole _              = Nothing

printRole = roleCata
  "Student"
  "Group Admin"
  "Course Admin"
  "Admin"

atLeastCourseAdmin Admin       = True
atLeastCourseAdmin CourseAdmin = True
atLeastCourseAdmin _           = False

class InRole r where
  isAdmin       :: r -> Bool
  isCourseAdmin :: r -> Bool
  isGroupAdmin  :: r -> Bool
  isStudent     :: r -> Bool

instance InRole Role where
  isAdmin       = (== Admin)
  isCourseAdmin = (>= CourseAdmin)
  isGroupAdmin  = (>= GroupAdmin)
  isStudent     = (== Student)

-- * Permissions

-- | Granted permission on a given operation
data Permission
  = P_Open
  | P_Create
  | P_Modify
  | P_Delete
  deriving (Show, Eq, Enum)

canOpen, canCreate, canModify, canDelete :: Permission -> Bool

canOpen   = flip elem [P_Open, P_Create, P_Modify, P_Delete]
canCreate = flip elem [P_Create, P_Modify, P_Delete]
canModify = flip elem [P_Modify, P_Delete]
canDelete = flip elem [P_Delete]

-- | Permissions are allowed on the following objects
data PermissionObject
  = P_Assignment
  | P_UserReg
  | P_Submission
  | P_Evaluation
  | P_Comment
  | P_Statistics
  | P_Password
  | P_GroupAdmin
  | P_User
  | P_Course
  | P_Group
  | P_CourseAdmin
  | P_AdminPage
  | P_PlainPage
  | P_TestScript
  | P_File
  | P_TestIncoming
  | P_TestCase
  | P_StudentPassword
  deriving (Eq, Ord, Show, Enum)

-- Permission Objects are dynamically associated with values
class PermissionObj p where
  permissionObject :: p -> PermissionObject

newtype ObjectPermissions = ObjectPermissions { permissions :: [(Permission, PermissionObject)] }

newtype Username = Username String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

usernameCata :: (String -> a) -> Username -> a
usernameCata f (Username u) = f u

withUsername :: Username -> (String -> a) -> a
withUsername (Username u) f = f u

class AsUsername c where
  asUsername :: c -> Username

instance Str Username where
  str (Username u) = u

type Password = String

class AsPassword p where
  asPassword :: p -> Password

passwordCata :: (String -> a) -> Password -> a
passwordCata f p = f p

newtype Email = Email String
  deriving (Eq, Ord, Read)

emailFold :: (String -> a) -> Email -> a
emailFold f (Email e) = f e

parseEmail :: String -> Maybe Email
parseEmail = Just . Email

instance Show Email where
  show (Email e) = e

instance Str Email where
  str (Email e) = e

-- TODO: throw exception if email string is unacceptable
email' :: String -> Email
email' = Email

emailCata :: (String -> a) -> Email -> a
emailCata f (Email e) = f e

-- | Represents a name of a time zone based on the
-- location for the given time zone.
-- E.g: ZoneInfo "Europe/Budapest"
newtype TimeZoneName = TimeZoneName { unTzn :: String }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

timeZoneName f (TimeZoneName z) = f z

showDate :: LocalTime -> String
showDate = formatTime defaultTimeLocale "%F, %T"

-- UserRegInfo is a User Registration Info that consists of
-- a Username, a Password, an Email Address, a Full Name, and a time zone
newtype UserRegInfo = UserRegInfo (String, String, String, String, TimeZoneName)

userRegInfoCata f (UserRegInfo (username, password, email, fullName, timeZoneName))
  = f username password email fullName timeZoneName

-- The language what the dictionary represents.
newtype Language = Language String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

languageCata f (Language l) = f l

-- | Logged in user
data User = User {
    u_role     :: Role
  , u_username :: Username
  , u_email    :: Email
  , u_name     :: String
  , u_timezone :: TimeZoneName
  , u_language :: Language
  } deriving (Eq, Ord, Show)

userCata f (User role username email name timezone language) =
  f role username email name timezone language

withUser = flip userCata

userAna role username email name timezone language = User
  <$> role
  <*> username
  <*> email
  <*> name
  <*> timezone
  <*> language

newtype PersonalInfo = PersonalInfo (Role, String, TimeZoneName)

personalInfoCata f (PersonalInfo (role, name, timezone))
  = f role name timezone

withPersonalInfo p f = personalInfoCata f p

data UserDesc = UserDesc {
    ud_username :: Username
  , ud_fullname :: String
  } deriving (Eq, Ord, Show)

mkUserDescription :: User -> UserDesc
mkUserDescription u = UserDesc {
    ud_username = u_username u
  , ud_fullname = u_name u
  }

-- | User awaiting for registration
data UserRegistration = UserRegistration {
    reg_username :: String
  , reg_email    :: String
  , reg_name     :: String -- User's full name
  , reg_token    :: String -- Token for identification
  , reg_timeout  :: UTCTime
  } deriving (Eq, Show, Read)

-- | Template function for the UserRegistration
userRegistration f (UserRegistration username email name token timeout) =
  f username email name token timeout

-- Test Script Type represents a choice: The test cases for the
-- test script will be uploaded as plain text or a zip file
data TestScriptType
  = TestScriptSimple
  | TestScriptZipped
  deriving (Eq, Ord, Enum, Show, Read, Data, Typeable)

-- Template function for the TestScriptType
testScriptTypeCata
  simple
  zipped
  t = case t of
    TestScriptSimple -> simple
    TestScriptZipped -> zipped

-- Test Script defines a scripts that can be integrated with the
-- testing framework for the given course.
data TestScript = TestScript {
    tsName :: String -- The name of the script
  , tsDescription :: String -- The short description of the script
  , tsNotes :: String -- The notes for the creator of the test cases, which are associated with the script
  , tsScript :: String -- The script itself that will be subsctituated to the test frameworks shell script
  , tsType :: TestScriptType -- The type of the test script
  } deriving (Eq, Show, Read)

-- Template function for the TestScript
testScriptCata
  tc -- Transformation of the test script type
  f
  (TestScript
    name
    description
    notes
    script
    type_)
  = f name description notes script (tc type_)

-- Template function for the TestScript with flipped parameters
withTestScript t tc f = testScriptCata tc f t

-- Applicative functor based TestScript value creation
testScriptAppAna name desc notes script type_
  = TestScript <$> name <*> desc <*> notes <*> script <*> type_

-- Test cases can arrive in plain text or in a zip file
data TestCaseType
  = TestCaseSimple
  | TestCaseZipped
  deriving (Data, Eq, Enum, Ord, Read, Show, Typeable)

-- Template function for the test case type
testCaseTypeCata
  simple
  zipped
  t = case t of
    TestCaseSimple -> simple
    TestCaseZipped -> zipped

-- Test Cases are for assignment and test script pair, test case
-- has name, description and a piece of code that will be subsctituated
-- during the evaluation of a submission
data TestCase = TestCase {
    tcName :: String -- The name of the test case
  , tcDescription :: String -- The short description of the test case
  , tcValue :: ByteString -- The stored value of test cases
  , tcType  :: TestCaseType -- The type of the test case
  , tcInfo  :: String -- Additional information which interpretation could change depending on the
                      -- type of the test case
  } deriving (Eq, Show, Read)

-- Template method for test case
testCaseCata
  tc -- Transformation of the TestCaseType
  f
  (TestCase name
            description
            value
            type_
            info)
  = f name description value (tc type_) info

-- | Template method for the test case with flipped arguments
withTestCase t tc f = testCaseCata tc f t

-- Applicative functor based TestCase value creation
testCaseAppAna name desc value type_ info
  = TestCase <$> name <*> desc <*> value <*> type_ <*> info

-- Name of the file that a user can upload
newtype UsersFile = UsersFile String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Template function for User's file
usersFileCata f (UsersFile x) = f x

-- File information that will be displayed on the UI
data FileInfo = FileInfo {
    fiSize :: Int     -- The size of the file in bytes
  , fiDate :: UTCTime -- The last modifcation date of the file
  }

-- Template function for the FileInfo value
fileInfoCata f (FileInfo size date) = f size date

-- Applicative functor based FileInfo construction
fileInfoAppAna size date = FileInfo <$> size <*> date

-- * PermObjs instance

instance PermissionObj Course where
  permissionObject _ = P_Course

instance PermissionObj Assignment where
  permissionObject _ = P_Assignment

instance PermissionObj UserRegistration where
  permissionObject _ = P_UserReg


-- * Ordering

-- Hungarian related charecter comparing, for special characters
-- uses the given list otherwise the normal comparism is called
-- capitals and non capitals are different characters
class CompareHun c where
  compareHun :: c -> c -> Ordering

instance CompareHun Char where
  compareHun c c' = maybe (compare c c') id
    ((compare <$> idxSmall   c <*> idxSmall   c') <|>
     (compare <$> idxCapital c <*> idxCapital c'))
    where
      idxSmall   x = findIndex (x==) hunSmall
      idxCapital x = findIndex (x==) hunCapital
      hunSmall   = "aábcdeéfghiíjklmnoóöőpqrstuúüűvwxyz"
      hunCapital = "AÁBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ"

instance CompareHun c => CompareHun [c] where
  compareHun [] []    = EQ
  compareHun [] (_:_) = LT
  compareHun (_:_) [] = GT
  compareHun (x:xs) (y:ys)
    = case compareHun x y of
        EQ -> compareHun xs ys
        other -> other

instance CompareHun Username where
  compareHun (Username u) (Username u') = compareHun u u'

instance CompareHun UserDesc where
  compareHun (UserDesc u n) (UserDesc u' n') =
    case compareHun n n' of
      EQ -> compareHun u u'
      other -> other

-- Status message is shown for the user on the UI
data StatusMessage a
  = SmNormal a -- Normal message
  | SmError a  -- Some none several error happened, the user needs to be informed about.
  deriving (Show, Eq)

statusMessage
  normal
  err
  sm
  = case sm of
    SmNormal x -> normal x
    SmError x -> err x

#ifdef TEST

-- * Invariants

compareHunTests = UnitTests [
    ("Small normal letters a-a", EQ == compareHun 'a' 'a')
  , ("Small normal letters d-z", LT == compareHun 'd' 'z')
  , ("Small normal letters z-a", GT == compareHun 'z' 'a')
  , ("Capital normal letters A-A", EQ == compareHun 'A' 'A')
  , ("Capital normal letters D-Z", LT == compareHun 'D' 'Z')
  , ("Capital normal letters Z-A", GT == compareHun 'Z' 'A')
  , ("Small accented letters á-á", EQ == compareHun 'á' 'á')
  , ("Small accented letters é-ú", LT == compareHun 'é' 'ú')
  , ("Small accented letters ű-á", GT == compareHun 'ű' 'á')
  , ("Capital accented letters Á-Á", EQ == compareHun 'á' 'á')
  , ("Capital accented letters É-Ú", LT == compareHun 'É' 'Ú')
  , ("Capital accented letters Ű-Á", GT == compareHun 'Ű' 'Á')
  ]

roleInvariants = Invariants [
    ( "printRole roles must generate string parseable by parseRole",
      \r -> ((Just r) ==) . parseRole . printRole $ r
    )
  ]

assignmentTests =
  let a = Assignment {
          assignmentName = "name"
        , assignmentDesc = "desc"
        , assignmentType = Normal
        , assignmentStart = read "2010-10-10 12:00:00 UTC"
        , assignmentEnd   = read "2010-11-10 12:00:00 UTC"
        }
      before  = read "2010-09-10 12:00:00 UTC"
      between = read "2010-10-20 12:00:00 UTC"
      after   = read "2010-12-10 12:00:00 UTC"
  in UnitTests [
    ("Time before active period", isFalse $ isActivePeriod a before)
  , ("Time in active period"    , isTrue  $ isActivePeriod a between)
  , ("Time after active period" , isFalse $ isActivePeriod a after)
  ]
  where
    isFalse = not
    isTrue  = id

entitiesTests = group "Bead.Domain.Entities" $ do
  assignmentTypeToAspectsTests
  isPasswordProtectedAspectsTests
  assignmentAspectsToTypeTests
  assignmentAspectPredTests
#endif
