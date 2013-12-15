module Test.Quick.EntityGen where

import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation
import Bead.View.UserActions

import Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as UserStory

import Bead.Controller.ServiceContext (ServiceContext)
import qualified Bead.Controller.ServiceContext as Context

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad (join, liftM)
import Control.Applicative ((<$>),(<*>))
import Data.List (nub)

import Data.Map (Map)
import qualified Data.Map as Map

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

users = userAna
  roleGen
  usernames
  emails
  familyNames
  (return UTC)

userAndEPwds = do
  user <- users
  code <- numbers
  return (user, code)

courseCodes = liftM CourseCode word

courseNames = word

courseDescs = manyWords

evalConfigs = oneof [
    binConfig
  , pctConfigs
  ]

binConfig = return (BinEval ())

percentage = do
  (_,f) <- properFraction <$> arbitrary
  return $ case f < 0 of
             True  -> (-1.0) * f
             False -> f

pctConfigs = (PctEval . PctConfig) <$> percentage

courses = Course
  <$> courseNames
  <*> courseDescs
  <*> evalConfigs

groupCodes = word

groupNames = manyWords

groupDescs = manyWords

groupUsers' = liftM (map Username) (listOf1 word)

groups = Group
  <$> groupNames
  <*> groupDescs
  <*> evalConfigs

timeZones = elements [UTC, CET, CEST]

assignments start end = assignmentAna
  assignmentNames
  assignmentDescs
  assignmentTypeGen
  (return start)
  timeZones
  (return end)
  timeZones

assignmentNames = manyWords

assignmentDescs = manyWords

assignmentTCss = manyWords

assignmentTypeGen = elements [Normal, Urn]

passwords = word

submissions date = Submission
  <$> solutionTexts
  <*> (return date)

comments date = Comment
  <$> commentTexts
  <*> (return date)

solutionTexts = manyWords

commentTexts = manyWords

evaluations :: EvaluationConfig -> Gen Evaluation
evaluations cfg = Evaluation
  <$> evaluationResults cfg
  <*> writtenEvaluations

writtenEvaluations = manyWords

evaluationResults =
  evaluationDataMap
    (const (BinEval <$> elements [Binary Passed, Binary Failed]))
    (const (PctEval . Percentage . Scores . (:[]) <$> percentage))
