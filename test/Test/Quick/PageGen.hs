module Test.Quick.PageGen where

import Control.Monad (liftM)
import Control.Applicative ((<$>),(<*>))

import Bead.Controller.Pages
import qualified Bead.Domain.Entities as E
import Bead.Domain.Relationships

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.Quick.EnumGen
import Test.Quick.RolePermissionGen

showInt :: Int -> String
showInt = show

submissionKeyGen :: Gen SubmissionKey
submissionKeyGen = SubmissionKey . showInt <$> choose (1,5000)

evaluationKeyGen :: Gen EvaluationKey
evaluationKeyGen = EvaluationKey . showInt <$> choose (1,5000)

instance Arbitrary Page where
  arbitrary = pageGen submissionKeyGen evaluationKeyGen

pageGen :: Gen SubmissionKey -> Gen EvaluationKey -> Gen Page
pageGen submissionKey evaluationKey = oneof [
    nonParametricPages
  , parametricPages
  ] where
      nonParametricPages = elements [
          Login
        , Logout
        , Home
        , Profile
        , Error
        , Administration
        , CourseAdmin
        , EvaluationTable
        , NewGroupAssignment
        , NewCourseAssignment
        , ModifyAssignment
        , Submission
        , SubmissionList
        , SubmissionDetails
        , GroupRegistration
        , UserDetails
        , UserSubmissions
        , CreateCourse
        , CreateGroup
        , AssignCourseAdmin
        , AssignGroupAdmin
        , ChangePassword
        , SetUserPassword
        ]

      parametricPages = oneof [
          CommentFromEvaluation <$> submissionKey
        , CommentFromModifyEvaluation <$> submissionKey <*> evaluationKey
        , Evaluation <$> submissionKey
        , ModifyEvaluation <$> submissionKey <*> evaluationKey
        ]
