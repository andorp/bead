{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content (
    Content(..)
  , emptyContent
  , mkContent
  , blaze
  , routeOf
  , runStory
  , withUserState
  , withUserStateAndFrame
  , GETContentHandler
  , POSTContentHandler
  , UserState(..)
  , Html
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
import Data.ByteString.Char8 hiding (span, empty, map)

import Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Domain.Entities
import Bead.View.UserActions
import Bead.View.Snap.Application (App)
import Bead.View.Snap.Pagelets hiding (invariants)
import Bead.View.Snap.RouteOf
import Bead.View.Snap.HandlerUtils (withUserState, runStory)
import Bead.View.Snap.TemplateAndComponentNames hiding (Username)

import Text.Blaze.Html5 (Html)

-- Pages have the following structure. A header, a context-sensitive menu,
-- a footer, and the content area. Every content area has its GET and POST handlers, its
-- page type. The common Html templates can be found in the Pagelet module

type GETContentHandler  = Handler App App ()
type POSTContentHandler = Handler App App UserAction

-- | Content Pages are rendered in content area.
data Content = Content {
    get   :: Maybe GETContentHandler
  , post  :: Maybe POSTContentHandler
  }

emptyContent :: Content
emptyContent = Content {
    get   = Nothing
  , post  = Nothing
  }

mkContent
  :: Maybe GETContentHandler
  -> Maybe POSTContentHandler
  -> Content
mkContent g p = Content { get = g, post = p }

withUserStateAndFrame :: (UserState -> Html) -> Handler App App ()
withUserStateAndFrame f = withUserState $ \state -> blaze $ withUserFrame state (f state) Nothing

