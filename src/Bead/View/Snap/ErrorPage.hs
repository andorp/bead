{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.ErrorPage (
    errorPage
  ) where

import Data.String

import Snap
import Snap.Blaze (blaze)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Controller.Pages (Page(Login))
import Bead.View.Snap.Application (App)
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.Pagelets (
    linkToPageWithText, withTitleAndHead
  )

-- | Produces an error page showing the reason of an error, and redirects to
--   login page after a while
errorPage :: ContentHandlerError -> Handler App b ()
errorPage = contentHandlerErrorMap (blaze . page)

page :: (Maybe String) -> Html
page e = withTitleAndHead "Error" $ do
  H.div $ do
    H.h2 $ "Some error happened"
    H.br
    maybe (return ()) fromString e
  H.div $
    linkToPageWithText Login "Go back to login page"

