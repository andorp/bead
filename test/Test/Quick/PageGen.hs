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



instance Arbitrary Page where
  arbitrary = pageGen

pageGen :: Gen Page
pageGen = oneof [
    nonParametricPages
  , parametricPages
  ] where
      showInt :: Int -> String
      showInt = show

      assignmentKey = AssignmentKey . showInt <$> choose (1,5000)

      submissionKey = SubmissionKey . showInt <$> choose (1,5000)

      evaluationKey = EvaluationKey . showInt <$> choose (1,5000)

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
        , SubmissionDetails <$> assignmentKey <*> submissionKey
        ]
