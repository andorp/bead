module Test.Quick.EntityGen where

import           Bead.Domain.Entities
import qualified Bead.Domain.Entity.Notification as Notification
import           Bead.Domain.TimeZone (utcZoneInfo, cetZoneInfo)
import           Bead.Domain.Shared.Evaluation

import           Test.Tasty.Arbitrary

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary
import           Control.Monad (join, liftM)
import           Control.Applicative ((<$>),(<*>))
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
scores = return Score

assessments = Assessment <$> manyWords <*> evalConfigs

notifTypes = elements [Notification.Comment, Notification.Feedback, Notification.System]

notifications = Notification.Notification . fromString <$> manyWords

