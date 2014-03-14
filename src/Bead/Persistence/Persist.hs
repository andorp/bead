module Bead.Persistence.Persist (
    Persist
  , runPersist

  , saveUser
  , personalInfo
  , filterUsers
  , loadUser
  , updateUser
  , doesUserExist
  , userDescription
  , userSubmissions
  , administratedCourses
  , administratedGroups

  -- Users file upload
  , copyFile  -- Copies the given file with the given filename to the users data directory
  , listFiles -- List all the user's files
  , getFile   -- Get the current path for the user's file

  -- Registration
  , saveUserReg
  , loadUserReg

  -- Course Persistence
  , saveCourse
  , courseKeys
  , filterCourses
  , loadCourse
  , groupKeysOfCourse
  , isUserInCourse
  , userCourses
  , createCourseAdmin
  , courseAdmins
  , subscribedToCourse
  , unsubscribedFromCourse
  , testScriptsOfCourse

  -- Group Persistence
  , saveGroup
  , loadGroup
  , courseOfGroup
  , filterGroups
  , isUserInGroup
  , userGroups
  , subscribe
  , unsubscribe
  , groupAdmins
  , createGroupAdmin
  , subscribedToGroup
  , unsubscribedFromGroup

  -- Test Scripts
  , saveTestScript
  , loadTestScript
  , courseOfTestScript
  , modifyTestScript

  -- Test Cases
  , saveTestCase
  , loadTestCase
  , assignmentOfTestCase
  , testScriptOfTestCase
  , modifyTestCase
  , removeTestCaseAssignment
  , copyTestCaseFile
  , modifyTestScriptOfTestCase

  -- Test Jobs
  , saveTestJob -- Saves the test job for the test daemon

  -- Test Comments
  , testComments
  , deleteTestComment -- Deletes the test daemon's comment from the test-incomming

  -- Assignment Persistence
  , filterAssignment
  , assignmentKeys
  , saveAssignment
  , loadAssignment
  , modifyAssignment
  , courseAssignments
  , groupAssignments
  , saveCourseAssignment
  , saveGroupAssignment
  , courseOfAssignment
  , groupOfAssignment
  , submissionsForAssignment
  , assignmentCreatedTime
  , testCaseOfAssignment

  -- Submission
  , saveSubmission
  , loadSubmission
  , assignmentOfSubmission
  , usernameOfSubmission
  , filterSubmissions
  , evaluationOfSubmission
  , commentsOfSubmission
  , lastSubmission

  , removeFromOpened
  , openedSubmissions
  , usersOpenedSubmissions

  -- Evaluation
  , saveEvaluation
  , loadEvaluation
  , modifyEvaluation
  , submissionOfEvaluation

  -- Comment
  , saveComment
  , loadComment
  , submissionOfComment

  -- Persistence initialization
  , isPersistenceSetUp
  , initPersistence
  ) where

import           Data.Time (UTCTime)

import           Bead.Domain.Types (Erroneous)
import           Bead.Domain.Entities
import           Bead.Domain.Relationships

import qualified Bead.Persistence.NoSQLDir as PersistImpl

type Persist a = PersistImpl.Persist a

saveUser :: User -> Persist ()
saveUser = PersistImpl.saveUser

personalInfo :: Username -> Persist PersonalInfo
personalInfo = PersistImpl.personalInfo

filterUsers :: (User -> Bool) -> Persist [User]
filterUsers = PersistImpl.filterUsers

loadUser :: Username -> Persist User
loadUser = PersistImpl.loadUser

updateUser :: User -> Persist ()
updateUser = PersistImpl.updateUser

doesUserExist :: Username -> Persist Bool
doesUserExist = PersistImpl.doesUserExist

userDescription :: Username -> Persist UserDesc
userDescription = PersistImpl.userDescription

userSubmissions :: Username -> AssignmentKey -> Persist [SubmissionKey]
userSubmissions = PersistImpl.userSubmissions

administratedCourses :: Username -> Persist [(CourseKey, Course)]
administratedCourses = PersistImpl.administratedCourses

administratedGroups :: Username -> Persist [(GroupKey, Group)]
administratedGroups = PersistImpl.administratedGroups

-- * Users file upload

copyFile :: Username -> FilePath -> UsersFile -> Persist () -- Copies the given file with the given filename to the users data directory
copyFile = PersistImpl.copyFile

listFiles :: Username -> Persist [(UsersFile, FileInfo)] -- List all the user's files
listFiles = PersistImpl.listFiles

getFile :: Username -> UsersFile -> Persist FilePath -- Get the current path for the user's file
getFile = PersistImpl.getFile

-- * Registration

saveUserReg :: UserRegistration -> Persist UserRegKey
saveUserReg = PersistImpl.saveUserReg

loadUserReg :: UserRegKey -> Persist UserRegistration
loadUserReg = PersistImpl.loadUserReg

-- * Course Persistence

saveCourse :: Course -> Persist CourseKey
saveCourse = PersistImpl.saveCourse

courseKeys :: Persist [CourseKey]
courseKeys = PersistImpl.courseKeys

filterCourses :: (CourseKey -> Course -> Bool) -> Persist [(CourseKey, Course)]
filterCourses = PersistImpl.filterCourses

loadCourse :: CourseKey -> Persist Course
loadCourse = PersistImpl.loadCourse

groupKeysOfCourse :: CourseKey -> Persist [GroupKey]
groupKeysOfCourse = PersistImpl.groupKeysOfCourse

isUserInCourse :: Username -> CourseKey -> Persist Bool
isUserInCourse = PersistImpl.isUserInCourse

userCourses :: Username -> Persist [CourseKey]
userCourses = PersistImpl.userCourses

createCourseAdmin :: Username -> CourseKey -> Persist ()
createCourseAdmin = PersistImpl.createCourseAdmin

courseAdmins :: CourseKey -> Persist [Username]
courseAdmins = PersistImpl.courseAdmins

subscribedToCourse :: CourseKey -> Persist [Username]
subscribedToCourse = PersistImpl.subscribedToCourse

unsubscribedFromCourse :: CourseKey -> Persist [Username]
unsubscribedFromCourse = PersistImpl.unsubscribedFromCourse

testScriptsOfCourse :: CourseKey -> Persist [TestScriptKey]
testScriptsOfCourse = PersistImpl.testScriptsOfCourse

-- * Group Persistence

saveGroup :: CourseKey -> Group -> Persist GroupKey
saveGroup = PersistImpl.saveGroup

loadGroup :: GroupKey -> Persist Group
loadGroup = PersistImpl.loadGroup

courseOfGroup :: GroupKey -> Persist CourseKey
courseOfGroup = PersistImpl.courseOfGroup

filterGroups :: (GroupKey -> Group -> Bool) -> Persist [(GroupKey, Group)]
filterGroups = PersistImpl.filterGroups

isUserInGroup :: Username -> GroupKey -> Persist Bool
isUserInGroup = PersistImpl.isUserInGroup

userGroups :: Username -> Persist [GroupKey]
userGroups = PersistImpl.userGroups

subscribe :: Username -> CourseKey -> GroupKey -> Persist ()
subscribe = PersistImpl.subscribe

unsubscribe :: Username -> CourseKey -> GroupKey -> Persist ()
unsubscribe = PersistImpl.unsubscribe

groupAdmins :: GroupKey -> Persist [Username]
groupAdmins = PersistImpl.groupAdmins

createGroupAdmin :: Username -> GroupKey -> Persist ()
createGroupAdmin = PersistImpl.createGroupAdmin

subscribedToGroup :: GroupKey -> Persist [Username]
subscribedToGroup = PersistImpl.subscribedToGroup

unsubscribedFromGroup :: GroupKey -> Persist [Username]
unsubscribedFromGroup = PersistImpl.unsubscribedFromGroup

-- * Test Scripts

saveTestScript :: CourseKey -> TestScript -> Persist TestScriptKey
saveTestScript = PersistImpl.saveTestScript

loadTestScript :: TestScriptKey -> Persist TestScript
loadTestScript = PersistImpl.loadTestScript

courseOfTestScript :: TestScriptKey -> Persist CourseKey
courseOfTestScript = PersistImpl.courseOfTestScript

modifyTestScript :: TestScriptKey -> TestScript -> Persist ()
modifyTestScript = PersistImpl.modifyTestScript

-- *  Test Cases

saveTestCase :: TestScriptKey -> AssignmentKey -> TestCase -> Persist TestCaseKey
saveTestCase = PersistImpl.saveTestCase

loadTestCase :: TestCaseKey -> Persist TestCase
loadTestCase = PersistImpl.loadTestCase

assignmentOfTestCase :: TestCaseKey -> Persist AssignmentKey
assignmentOfTestCase = PersistImpl.assignmentOfTestCase

testScriptOfTestCase :: TestCaseKey -> Persist TestScriptKey
testScriptOfTestCase = PersistImpl.testScriptOfTestCase

modifyTestCase :: TestCaseKey -> TestCase -> Persist ()
modifyTestCase = PersistImpl.modifyTestCase

removeTestCaseAssignment :: TestCaseKey -> AssignmentKey -> Persist ()
removeTestCaseAssignment = PersistImpl.removeTestCaseAssignment

copyTestCaseFile :: TestCaseKey -> Username -> UsersFile -> Persist ()
copyTestCaseFile = PersistImpl.copyTestCaseFile

modifyTestScriptOfTestCase :: TestCaseKey -> TestScriptKey -> Persist ()
modifyTestScriptOfTestCase = PersistImpl.modifyTestScriptOfTestCase

-- * Test Jobs

saveTestJob :: SubmissionKey -> Persist () -- Saves the test job for the test daemon
saveTestJob = PersistImpl.saveTestJob

-- * Test Comments

-- | List the comments that the test daemon left in the test-incomming, comment for the
-- groups admin, and comments for the student
testComments :: Persist [(SubmissionKey, Comment)]
testComments = PersistImpl.testComments

deleteTestComment :: SubmissionKey -> Persist ()   -- Deletes the test daemon's comment from the test-incomming
deleteTestComment = PersistImpl.deleteTestComment

-- * Assignment Persistence

filterAssignment :: (AssignmentKey -> Assignment -> Bool) -> Persist [(AssignmentKey, Assignment)]
filterAssignment = PersistImpl.filterAssignment

assignmentKeys :: Persist [AssignmentKey]
assignmentKeys = PersistImpl.assignmentKeys

saveAssignment :: Assignment -> Persist AssignmentKey
saveAssignment = PersistImpl.saveAssignment

loadAssignment :: AssignmentKey -> Persist Assignment
loadAssignment = PersistImpl.loadAssignment

modifyAssignment :: AssignmentKey -> Assignment -> Persist ()
modifyAssignment = PersistImpl.modifyAssignment

courseAssignments :: CourseKey -> Persist [AssignmentKey]
courseAssignments = PersistImpl.courseAssignments

groupAssignments :: GroupKey -> Persist [AssignmentKey]
groupAssignments = PersistImpl.groupAssignments

saveCourseAssignment :: CourseKey -> Assignment -> Persist AssignmentKey
saveCourseAssignment = PersistImpl.saveCourseAssignment

saveGroupAssignment :: GroupKey  -> Assignment -> Persist AssignmentKey
saveGroupAssignment = PersistImpl.saveGroupAssignment

courseOfAssignment :: AssignmentKey -> Persist (Maybe CourseKey)
courseOfAssignment = PersistImpl.courseOfAssignment

groupOfAssignment :: AssignmentKey -> Persist (Maybe GroupKey)
groupOfAssignment = PersistImpl.groupOfAssignment

submissionsForAssignment :: AssignmentKey -> Persist [SubmissionKey]
submissionsForAssignment = PersistImpl.submissionsForAssignment

assignmentCreatedTime :: AssignmentKey -> Persist UTCTime
assignmentCreatedTime = PersistImpl.assignmentCreatedTime

testCaseOfAssignment :: AssignmentKey -> Persist (Maybe TestCaseKey)
testCaseOfAssignment = PersistImpl.testCaseOfAssignment

-- * Submission

saveSubmission :: AssignmentKey -> Username -> Submission -> Persist SubmissionKey
saveSubmission = PersistImpl.saveSubmission

loadSubmission :: SubmissionKey -> Persist Submission
loadSubmission = PersistImpl.loadSubmission

assignmentOfSubmission :: SubmissionKey -> Persist AssignmentKey
assignmentOfSubmission = PersistImpl.assignmentOfSubmission

usernameOfSubmission :: SubmissionKey -> Persist Username
usernameOfSubmission = PersistImpl.usernameOfSubmission

filterSubmissions :: (SubmissionKey -> Submission -> Bool) -> Persist [(SubmissionKey, Submission)]
filterSubmissions = PersistImpl.filterSubmissions

evaluationOfSubmission :: SubmissionKey -> Persist (Maybe EvaluationKey)
evaluationOfSubmission = PersistImpl.evaluationOfSubmission

commentsOfSubmission :: SubmissionKey -> Persist [CommentKey]
commentsOfSubmission = PersistImpl.commentsOfSubmission

lastSubmission :: AssignmentKey -> Username -> Persist (Maybe SubmissionKey)
lastSubmission = PersistImpl.lastSubmission

removeFromOpened :: AssignmentKey -> Username -> SubmissionKey -> Persist ()
removeFromOpened = PersistImpl.removeFromOpened

openedSubmissions :: Persist [SubmissionKey]
openedSubmissions = PersistImpl.openedSubmissions

usersOpenedSubmissions :: AssignmentKey -> Username -> Persist [SubmissionKey] -- ^ Calculates all the opened submisison for a given user and a given assignment
usersOpenedSubmissions = PersistImpl.usersOpenedSubmissions

-- * Evaluation

saveEvaluation :: SubmissionKey -> Evaluation -> Persist EvaluationKey
saveEvaluation = PersistImpl.saveEvaluation

loadEvaluation :: EvaluationKey -> Persist Evaluation
loadEvaluation = PersistImpl.loadEvaluation

modifyEvaluation :: EvaluationKey -> Evaluation -> Persist ()
modifyEvaluation = PersistImpl.modifyEvaluation

submissionOfEvaluation :: EvaluationKey -> Persist SubmissionKey
submissionOfEvaluation = PersistImpl.submissionOfEvaluation

-- * Comment

saveComment :: SubmissionKey -> Comment -> Persist CommentKey
saveComment = PersistImpl.saveComment

loadComment :: CommentKey -> Persist Comment
loadComment = PersistImpl.loadComment

submissionOfComment :: CommentKey -> Persist SubmissionKey
submissionOfComment = PersistImpl.submissionOfComment

-- * Persistence initialization

isPersistenceSetUp :: IO Bool
isPersistenceSetUp = PersistImpl.isPersistenceSetUp

initPersistence :: IO ()
initPersistence = PersistImpl.initPersistence

runPersist :: Persist a -> IO (Erroneous a)
runPersist = PersistImpl.runPersist

