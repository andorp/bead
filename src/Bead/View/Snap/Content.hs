{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content (
    Content(..)
  , emptyContent
  , mkContent
  , blaze
  , routeOf
  , module Snap
  , module Data.ByteString.Char8

  , module Bead.Domain.Entities
  , module Bead.View.UserActions
  , module Bead.View.Snap.Application
  , module Bead.View.Snap.Pagelets
  , module Bead.View.Snap.TemplateAndComponentNames
  ) where

import Snap hiding (empty, get, route)
import Snap.Blaze (blaze)
import Data.ByteString.Char8 hiding (span, empty)

import Bead.Controller.Pages as P
import Bead.Domain.Entities
import Bead.View.UserActions
import Bead.View.Snap.Application (App)
import Bead.View.Snap.Pagelets
import Bead.View.Snap.TemplateAndComponentNames hiding (Username)


-- Pages have the following structure. A header, a context-sensitive menu,
-- a footer, and the content area. Every content area has its GET and POST handlers, its
-- page type. The common Html templates can be found in the Pagelet module

-- | Content Pages are rendered in content area.
data Content = Content {
    get   :: Maybe (Handler App App ())
  , post  :: Maybe (Handler App App UserAction)
  }

emptyContent :: Content
emptyContent = Content {
    get   = Nothing
  , post  = Nothing
  }

mkContent
  :: Maybe (Handler App App ())
  -> Maybe (Handler App App UserAction)
  -> ByteString
  -> Content
mkContent g p r = Content { get = g, post = p }

routeOf :: P.Page -> ByteString
routeOf = r where
  r P.Login   = "/login"
  r P.Home    = "/home"
  r P.Profile = "/profile"
  r P.Course  = "/course"
  r P.Group   = "/group"
  r P.OpenExam = "/open-exam"
  r P.ClosedExam = "/closed-exam"
  r P.Error      = "/error"
  r P.SubmitExam = "/submit-exam"
  r P.Evaulation = "/evaulation"
  r P.Training   = "/training"
  r P.Admin      = "/admin"
  r P.CreateExercise = "/create-exercise"
  r p = error $ "There is no route defined for the page: " ++ (show p)
