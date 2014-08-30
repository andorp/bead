module Test.Quick.EntityGen where

import Bead.Domain.Entities
import Bead.Domain.TimeZone (utcZoneInfo, cetZoneInfo)
import Bead.Domain.Shared.Evaluation

import Test.Quick.EnumGen

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad (join, liftM)
import Control.Applicative ((<$>),(<*>))

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

users = userAna
  roleGen
  usernames
  emails
  familyNames
  (return utcZoneInfo)
  languages

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

submissions date = Submission
  <$> solutionTexts
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
  enumGenerator -- type

testCases = testCaseAppAna
  word      -- name
  manyWords -- desc
  (BS.pack <$> manyWords) -- value
  enumGenerator -- type
  manyWords -- info

testFeedbackInfo = oneof
  [ TestResult <$> arbitrary
  , MessageForStudent <$> manyWords
  , MessageForAdmin <$> manyWords
  ]

scores :: Gen Score
scores = return Score

assessments = Assessment <$> manyWords <*> evalConfigs

