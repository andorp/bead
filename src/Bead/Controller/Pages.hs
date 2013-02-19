module Bead.Controller.Pages (
    Page(..)
  , allPages
  , pageTransition
  , allowedPages
  , menuPages
  ) where

import qualified Bead.Domain.Entities      as E
import qualified Bead.Domain.Relationships as R

import Data.List (nub)

-- * Page types and necessary data

data Page
  = Login
  | Logout
  | Home
  | Profile
  | Course
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

pageTransition s = nub $ p s ++ [Login, Error, Logout]
  where
    p Login      = [Login, Error, Home]
    p Home       = [Profile, Course, Group, Exercise, ClosedExam, Evaulation, Training, Admin]
    p Profile    = [Home]
    p Course     = [Group, Exercise, ClosedExam]
    p Group      = [Exercise, ClosedExam]
    p Exercise   = [SubmitExam]
    p SubmitExam = [Home]
    p ClosedExam = [Evaulation]
    p Training   = [Group, Course, Home, Evaulation]
    p Admin      = [Home, CreateExercise]
    p CreateExercise = [Admin, Home]

regularPages = [Home, Profile, Course, Group, Exercise, ClosedExam, Error, SubmitExam, Evaulation]

nonMenuPages = [Login, Error, Exercise]

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
      ]
