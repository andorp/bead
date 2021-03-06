module Test.Property.EntityGen where

import           Bead.Domain.Entities
import qualified Bead.Domain.Entity.Notification as Notification
import           Bead.Domain.TimeZone (utcZoneInfo, cetZoneInfo)
import           Bead.Domain.Shared.Evaluation

import           Test.Tasty.Arbitrary

import           Control.Monad (join, liftM)
import           Control.Applicative ((<$>),(<*>),pure)
import           Data.String (fromString)

import qualified Data.ByteString.Char8 as BS (pack)

word = listOf1 $ elements ['a' .. 'z' ]
numbers = listOf1 $ elements ['0' .. '9']

manyWords = do
  w <- word
  ws <- manyWords'
  return $ w ++ " " ++ ws

  where
    manyWords' = listOf1 $ elements $ ' ':['a' .. 'z']

usernames = liftM Username (vectorOf 6 $ oneof [capital, digits])
  where
    capital = elements ['A' .. 'Z']
    digits  = elements ['0' .. '9']

uids = fmap (usernameCata Uid) usernames

roleGen = elements [Student, GroupAdmin, CourseAdmin, Admin]

emails = do
  user <- word
  domain <- word
  return $ Email $ join [user, "@", domain, ".com"]

familyNames = do
  first <- word
  last <- word
  return $ join [first, " ", last]

languages = Language <$> word

users = User
  <$> roleGen
  <*> usernames
  <*> emails
  <*> familyNames
  <*> (return utcZoneInfo)
  <*> languages
  <*> uids

userAndEPwds = do
  user <- users
  code <- numbers
  return (user, code)

courseCodes = liftM CourseCode word

courseNames = word

courseDescs = manyWords

evalConfigs = oneof [
    return binaryConfig
  , percentageConfig <$> percentage
  ]

percentage = do
  (_,f) <- properFraction <$> arbitrary
  return $ case f < 0 of
             True  -> (-1.0) * f
             False -> f

courses =
  courseAppAna
    courseNames
    courseDescs
    (elements [TestScriptSimple, TestScriptZipped])

groupCodes = word

groupNames = manyWords

groupDescs = manyWords

groupUsers' = liftM (map Username) (listOf1 word)

groups = Group
  <$> groupNames
  <*> groupDescs

timeZones = elements [utcZoneInfo, cetZoneInfo]

assignments start end = assignmentAna
  assignmentNames
  assignmentDescs
  assignmentTypeGen
  (return start)
  (return end)
  evaluationConfigs

assignmentNames = manyWords

assignmentDescs = manyWords

assignmentTCss = manyWords

assignmentTypeGen = oneof [
    (return emptyAspects)
  , (return $ aspectsFromList [BallotBox])
  , (do pwd <- word; return $ aspectsFromList [Password pwd])
  , (do pwd <- word; return $ aspectsFromList [Password pwd, BallotBox])
  ]

evaluationConfigs = oneof [
    (return binaryConfig)
  , percentageConfig <$> percentage
  ]

passwords = word

solutionValues = oneof [
    SimpleSubmission <$> solutionTexts
  , ZippedSubmission . fromString <$> solutionTexts
  ]

submissions date = Submission
  <$> solutionValues
  <*> (return date)

commentTypes = elements [CT_Student, CT_GroupAdmin, CT_CourseAdmin, CT_Admin]

comments date = Comment
  <$> commentTexts
  <*> commentAuthors
  <*> (return date)
  <*> commentTypes

solutionTexts = manyWords

commentTexts = manyWords

commentAuthors = manyWords

evaluations :: EvConfig -> Gen Evaluation
evaluations cfg = Evaluation
  <$> evaluationResults cfg
  <*> writtenEvaluations

writtenEvaluations = manyWords

evaluationResults =
  evConfigCata
    (binaryResult <$> elements [Passed, Failed])
    (const (percentageResult <$> percentage))
    arbitrary

testScripts = testScriptAppAna
  word      -- words
  manyWords -- desc
  manyWords -- notes
  manyWords -- script
  enumGen   -- type

testCases = oneof [
    TestCase <$> word <*> manyWords <*> (SimpleTestCase <$> manyWords) <*> manyWords
  , TestCase <$> word <*> manyWords <*> (ZippedTestCase . BS.pack <$> manyWords) <*> manyWords
  ]

testFeedbackInfo = oneof
  [ TestResult <$> arbitrary
  , MessageForStudent <$> manyWords
  , MessageForAdmin <$> manyWords
  ]

feedbacks date = Feedback <$> testFeedbackInfo <*> (return date)

scores :: Gen Score
scores = arbitrary

date = read "2016-01-22 14:41:26 UTC"

assessments = Assessment <$> manyWords <*> manyWords <*> pure date <*> evalConfigs

notifEvents = oneof
  [ Notification.NE_CourseAdminCreated <$> manyWords
  , Notification.NE_CourseAdminAssigned <$> manyWords <*> manyWords
  , Notification.NE_TestScriptCreated <$> manyWords <*> manyWords
  , Notification.NE_TestScriptUpdated <$> manyWords <*> manyWords <*> manyWords
  , Notification.NE_RemovedFromGroup <$> manyWords <*> manyWords
  , Notification.NE_GroupAdminCreated <$> manyWords <*> manyWords <*> manyWords
  , Notification.NE_GroupAssigned <$> manyWords <*> manyWords <*> manyWords <*> manyWords
  , Notification.NE_GroupCreated <$> manyWords <*> manyWords <*> manyWords
  , Notification.NE_GroupAssignmentCreated <$> manyWords <*> manyWords <*> manyWords <*> manyWords
  , Notification.NE_CourseAssignmentCreated <$> manyWords <*> manyWords <*> manyWords
  , Notification.NE_GroupAssessmentCreated <$> manyWords <*> manyWords <*> manyWords <*> manyWords
  , Notification.NE_CourseAssessmentCreated <$> manyWords <*> manyWords <*> manyWords
  , Notification.NE_AssessmentUpdated <$> manyWords <*> manyWords
  , Notification.NE_AssignmentUpdated <$> manyWords <*> manyWords
  , Notification.NE_EvaluationCreated <$> manyWords <*> manyWords
  , Notification.NE_AssessmentEvaluationUpdated <$> manyWords <*> manyWords
  , Notification.NE_AssignmentEvaluationUpdated <$> manyWords <*> manyWords
  , Notification.NE_CommentCreated <$> manyWords <*> manyWords <*> manyWords
  ]

notifications =
  Notification.Notification <$> notifEvents <*> pure date <*> pure Notification.System

