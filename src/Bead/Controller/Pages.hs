module Bead.Controller.Pages (
    Page(..)
  , allPages
  , pageTransition
  , parentPage
  , allowedPages
  , menuPages
  , reachable
  , invariants
  , unitTests
  ) where

import qualified Bead.Domain.Entities      as E
import qualified Bead.Domain.Relationships as R

import Data.List (nub)

import Data.Set (Set(..))
import qualified Data.Set as Set

import Bead.Invariants (Invariants(..), UnitTests(..))

-- * Page types and necessary data

data Page
  = Login
  | Logout
  | Home
  | Profile
  | Course
  | Courses
  | Group
  | Groups
  | Exercise
  | ClosedExam
  | Error
  | SubmitExam
  | Evaulation
  | Training
  | Admin
  | CreateExercise
  | CreateCourse
  | CreateGroup
  -- etc ...
  deriving (Eq, Ord, Enum, Show)

allPages :: [Page]
allPages = [Login .. ]

pageTransition s = nub $ p s ++ [s, Login, Error, Logout]
  where
    p Login      = [Home]
    p Logout     = []
    p Error      = []
    p Home       = [ Profile, Courses, Group, Exercise, ClosedExam, Evaulation
                   , Training, Admin, SubmitExam, Groups ]
    p CreateExercise = [Admin]
    p CreateCourse   = [Admin]
    p CreateGroup    = [Course]
    p Admin          = [Home, CreateExercise, CreateCourse]
    p Courses    = [Home, Course]
    p Course     = [Courses, Group, CreateGroup]
    p Groups     = [Home]
    p Group      = [Courses]
    p _          = [Home]
    p g = error $ "Unknown transition for page: " ++ show g

reachable :: Page -> Page -> Bool
reachable p q = elem q $ pageTransition p

regularPages = [
    Home
  , Profile
  , Course
  , Courses
  , Group
  , Exercise
  , ClosedExam
  , Error
  , SubmitExam
  , Evaulation
  , Training
  ]

adminPages = [
    Admin
  , Groups
  , CreateCourse
  , CreateExercise
  , CreateGroup
  ]

nonMenuPages = [
    Login
  , Logout
  , Error
  , Exercise
  , Course
  , Group
  , CreateGroup
  ]

allowedPages :: E.Role -> [Page]
allowedPages E.Student     =                  regularPages
allowedPages E.Professor   = CreateExercise : regularPages
allowedPages E.CourseAdmin = CreateExercise : regularPages
allowedPages E.Admin       = adminPages    ++ regularPages
allowedPages p = error $ "There is no pages defined for the " ++ show p

menuPages :: E.Role -> Page -> [Page]
menuPages r p = filter allowedPage $ pageTransition p
  where
    allowedPage p' = and [
        elem p' $ allowedPages r
      , not $ elem p' nonMenuPages
      , p' /= p
      ]

parentPage :: Page -> Page
parentPage Login          = Login
parentPage Error          = Error
parentPage Logout         = Logout
parentPage CreateExercise = Admin
parentPage CreateCourse   = Admin
parentPage CreateGroup    = Course
parentPage Courses        = Home
parentPage Course         = Courses
parentPage Groups         = Home
parentPage Group          = Course
parentPage _              = Home

-- * Invariants

invariants = Invariants [
    -- For each page the following property is hold:
    -- Every parent page of a page has a transition to the given page
    ("Parent page relation", \p -> elem p $ pageTransition $ parentPage p)
  ]

unitTests = UnitTests [
    ("Regular, Admin and NonMenu pages should cover all pages",
     Set.fromList (regularPages ++ adminPages ++ nonMenuPages) == Set.fromList allPages)
  ]
