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
  | Home
  | Profile
  | Course
  | Group
  | OpenExam
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

pageTransition s = nub $ p s ++ [Login, Error]
  where
    p Login      = [Login, Error, Home]
    p Home       = [Profile, Course, Group, OpenExam, ClosedExam, Evaulation, Training, Admin]
    p Profile    = [Home]
    p Course     = [Group, OpenExam, ClosedExam]
    p Group      = [OpenExam, ClosedExam]
    p OpenExam   = [SubmitExam]
    p SubmitExam = [Home]
    p ClosedExam = [Evaulation]
    p Training   = [Group, Course, Home, Evaulation]
    p Admin      = [Home, CreateExercise]
    p CreateExercise = [Admin, Home]

regularPages = [Home, Profile, Course, Group, OpenExam, ClosedExam, Error, SubmitExam, Evaulation]

allowedPages :: E.Role -> [Page]
allowedPages E.Student     =                  regularPages
allowedPages E.Professor   = CreateExercise : regularPages
allowedPages E.CourseAdmin = CreateExercise : regularPages
allowedPages E.Admin       = Admin          : regularPages
allowedPages p = error $ "There is no pages defined for the " ++ show p

-- TODO: Context sensitive menu
menuPages :: E.Role -> Page -> [Page]
menuPages r p = [Home, Profile]

