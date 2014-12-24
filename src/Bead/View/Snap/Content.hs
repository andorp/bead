{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Content (
    blaze
  , getParameter
  , getParameterValues
  , getOptionalParameter
  , getOptionalOrNonEmptyParameter
  , getJSONParam
  , getDictionaryInfos
  , setInSessionE
  , setReqParamInSession
  , routeOf
  , routeWithParams
  , runStory
  , userStory
  , userState
  , i18nE
  , i18nH
  , renderBootstrapPage
  , userTimeZone
  , userTimeZoneToLocalTimeConverter
  , userTimeZoneToUTCTimeConverter
  , foundTimeZones
  , withUserState
  , GETContentHandler
  , POSTContentHandler
  , ViewPOSTContentHandler
  , ContentHandler
  , UserState(..)
  , userStateCata
  , usernameInState
  , ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , UserTimeConverter

  , ViewHandler(..)
  , UserViewHandler(..)
  , ViewModifyHandler(..)
  , ModifyHandler(..)
  , DataHandler(..)

  , PageHandler
  , viewHandlerCata
  , userViewHandlerCata
  , viewModifyHandlerCata
  , modifyHandlerCata
  , dataHandlerCata

  , module Snap
  , module Control.Applicative
  , module Data.ByteString.Char8
  , module Data.Monoid

  , module Bead.Domain.Entities
  , module Bead.Domain.Entity.Comment
  , module Bead.Domain.Func
  , module Bead.Domain.Relationships
  , module Bead.View.UserActions
  , module Bead.View.Snap.I18N
  , module Bead.View.Snap.Translation
  , module Bead.View.Snap.Application
  , module Bead.View.Snap.Pagelets
  , module Bead.View.Snap.Style
  , module Bead.View.Snap.DataBridge
  , module Bead.View.Snap.TemplateAndComponentNames
  , module Bead.View.Snap.Fay.HookIds
  , module Bead.View.Snap.Fay.JSON.ServerSide
  ) where

import Control.Applicative hiding (empty)
import Data.Monoid

import Data.ByteString.Char8 (ByteString, unpack)
import Snap hiding (Config(..), empty, get, route, (<$>))
import Snap.Blaze (blaze)

import Bead.Controller.Pages
import Bead.Controller.ServiceContext
import Bead.Domain.Entities hiding (name)
import Bead.Domain.Entity.Comment
import Bead.Domain.Func
import Bead.Domain.Relationships
import Bead.View.UserActions
import Bead.View.Snap.Application
import Bead.View.Snap.ContentHandler
import Bead.View.Snap.DataBridge hiding (name)
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Fay.JSON.ServerSide
import Bead.View.Snap.I18N
import Bead.View.Snap.Translation
import Bead.View.Snap.TemplateAndComponentNames hiding (tcName)
import Bead.View.Snap.RouteOf
import Bead.View.Snap.Style
#ifdef TEST
import Bead.View.Snap.Pagelets hiding (invariants)
#else
import Bead.View.Snap.Pagelets
#endif

-- Pages have the following structure. A header, a context-sensitive menu,
-- a footer, and the content area. Every content area has its GET and POST handlers, its
-- page type. The common Html templates can be found in the Pagelet module

type GETContentHandler  = ContentHandler IHtml
type POSTContentHandler = ContentHandler UserAction
type ViewPOSTContentHandler = ContentHandler IHtml

-- View Handler runs on a view page, that is generated by a GET request,
-- only the rendering of the page is the purpose of the handler.
newtype ViewHandler = ViewHandler { unViewHandler :: GETContentHandler }

viewHandlerCata f (ViewHandler x) = f x

-- User View Handler runs on a User View page, that is generated by a POST request,
-- only the rendering of the page is the purpose of the handler, but the,
-- page content depends on the previous user imput.
newtype UserViewHandler = UserViewHandler { unUserViewHandler :: GETContentHandler }

userViewHandlerCata f (UserViewHandler x) = f x

-- View Modify Handler runs on a GET request, which generates a page, and runs on the
-- corresponding POST request, which generates a UserAction, which will be interpreted
-- as an UserAction with a given UserStory.
data ViewModifyHandler = ViewModifyHandler GETContentHandler POSTContentHandler

viewModifyHandlerCata f (ViewModifyHandler x y) = f x y

-- Modify Handler runs only POST request that lously coupled to a GET generated page, which
-- should have multiple UserAction related, data modification pages.
newtype ModifyHandler = ModifyHandler { unModifyHandler :: POSTContentHandler }

modifyHandlerCata f (ModifyHandler x) = f x

-- Data Handler runs on GET requests and renders data from the actual user state and
-- from the persistence layer
newtype DataHandler = DataHandler { unDataHandler :: ContentHandler () }

dataHandlerCata f (DataHandler x) = f x

type PageHandler = Page ViewHandler UserViewHandler ViewModifyHandler ModifyHandler DataHandler
