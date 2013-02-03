module Bead.Controller.Pages where

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
    p Admin      = [Home]
{-
landingPage :: Page -> Page
landingPage _ = Home
-}    
-- * Page building blocks

data PageBlocks
  = Logout
  | Statictics
  | Comment
  | Exercise
  | Solution
  | ProfileBlock
  | Username
  -- etc ...
  deriving (Eq, Show)


