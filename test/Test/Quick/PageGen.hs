{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Quick.PageGen where

import Control.Applicative ((<$>),(<*>))

import Bead.Controller.Pages
import Bead.Domain.Relationships

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary PageDesc where
  arbitrary = pageGen

pageGen :: Gen PageDesc
pageGen = oneof [
    nonParametricPages
  , parametricPages
  ] where
      showInt :: Int -> String
      showInt = show

      assignmentKey = AssignmentKey . showInt <$> choose (1,5000)
      submissionKey = SubmissionKey . showInt <$> choose (1,5000)
      evaluationKey = EvaluationKey . showInt <$> choose (1,5000)
      courseKey     = CourseKey . showInt     <$> choose (1,5000)
      groupKey      = GroupKey . showInt      <$> choose (1,5000)
      testScriptKey = TestScriptKey . showInt <$> choose (1,5000)

      nonParametricPages = elements [
          login ()
        , logout ()
        , home ()
        , profile ()
        , administration ()
        , courseAdmin ()
        , evaluationTable ()
        , submission ()
        , submissionList ()
        , groupRegistration ()
        , userDetails ()
        , userSubmissions ()
        , uploadFile ()
        , createCourse ()
        , createGroup ()
        , assignCourseAdmin ()
        , assignGroupAdmin ()
        , changePassword ()
#ifndef LDAPEnabled
        , setUserPassword ()
#endif
        , newTestScript ()
        ]

      parametricPages = oneof [
          evaluation <$> submissionKey <*> unit
        , courseOverview <$> courseKey <*> unit
        , modifyEvaluation <$> submissionKey <*> evaluationKey <*> unit
        , submissionDetails <$> assignmentKey <*> submissionKey <*> unit
        , deleteUsersFromCourse <$> courseKey <*> unit
        , deleteUsersFromGroup <$> groupKey <*> unit
        , unsubscribeFromCourse <$> groupKey <*> unit
        , modifyTestScript <$> testScriptKey <*> unit
        , newCourseAssignment <$> courseKey <*> unit
        , newGroupAssignment <$> groupKey <*> unit
        , modifyAssignment <$> assignmentKey <*> unit
        , viewAssignment <$> assignmentKey <*> unit
        , newCourseAssignmentPreview <$> courseKey <*> unit
        , newGroupAssignmentPreview <$> groupKey <*> unit
        , modifyAssignmentPreview <$> assignmentKey <*> unit
        , getSubmission <$> submissionKey <*> unit
        ]

      unit = return ()
