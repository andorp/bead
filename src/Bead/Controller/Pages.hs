module Bead.Controller.Pages (
    Page(..)
  , allPages
  , pageTransition
  , parentPage
  , allowedPages
  , menuPages
  , reachable
  , invariants
  ) where

import qualified Bead.Domain.Entities      as E
import qualified Bead.Domain.Relationships as R

import Data.List (nub)

import Bead.Invariants (Invariants(..))

-- * Page types and necessary data

data Page
  = Login
  | Logout
  | Home
  | Profile
  | Course
  | Courses
  | Group
  | Exercise
  | ClosedExam
  | Error
  | SubmitExam
  | Evaulation
  | Training
  | Admin
  | CreateExercise
  -- etc ...
  deriving (Eq, Enum, Show)

allPages :: [Page]
allPages = [Login .. ]

pageTransition s = nub $ p s ++ [s, Login, Error, Logout]
  where
    p Login      = [Home]
    p Logout     = []
    p Error      = []
    p Home       = [ Profile, Courses, Group, Exercise, ClosedExam, Evaulation
                   , Training, Admin, SubmitExam ]
    p CreateExercise = [Admin]
    p Admin          = [Home, CreateExercise]
    p Courses    = [Home, Course]
    p Course     = [Courses]
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
  ]

nonMenuPages = [
    Login
  , Error
  , Exercise
  , Course
  ]

allowedPages :: E.Role -> [Page]
allowedPages E.Student     =                  regularPages
allowedPages E.Professor   = CreateExercise : regularPages
allowedPages E.CourseAdmin = CreateExercise : regularPages
allowedPages E.Admin       = Admin          : regularPages
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
parentPage Courses        = Home
parentPage Course         = Courses
parentPage _              = Home

-- * Invariants

invariants = Invariants [
    -- For each page the following property is hold:
    -- Every parent page of a page has a transition to the given page
    ("Parent page relation", \p -> elem p $ pageTransition $ parentPage p)
  ]
