{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.ErrorPage (
    ErrorPage(..)
  , defErrorPage
  , msgErrorPage
  , translationErrorPage
  ) where

import           Data.String

import           Snap

import           Bead.View.BeadContext (BeadHandler')
import qualified Bead.View.Content.Public.ErrorPage as View
import           Bead.View.ContentHandler
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.Pagelets (publicFrame)
import           Bead.View.Translation

class ErrorPage e where
  errorPage :: Translation String -> e -> BeadHandler' b ()

instance ErrorPage String where
  errorPage title msg = renderPublicErrorPage title msg

instance ErrorPage (Translation String) where
  errorPage title msg = render . (pageTranslation title) $ Just msg

instance ErrorPage TransMsg where
  errorPage title msg = do
    i18n <- i18nH
    render . (page title) $ Just (translateMessage i18n msg)

instance ErrorPage ContentError where
  errorPage title msg = contentHandlerErrorMap (render . (page title)) msg

msgErrorPage :: String -> BeadHandler' b ()
msgErrorPage = defErrorPage

defErrorPage :: (ErrorPage e) => e -> BeadHandler' b ()
defErrorPage = errorPage (msg_ErrorPage_Title "Error!")

-- Produces a handler that renders the error page, with the
-- given title and message for the user
translationErrorPage :: Translation String -> Translation String -> BeadHandler' b ()
translationErrorPage = errorPage

page :: Translation String -> (Maybe String) -> IHtml
page = View.template fromString

pageTranslation :: Translation String -> (Maybe (Translation String)) -> IHtml
pageTranslation title err = do
  msg <- getI18N
  View.template (fromString . msg) title err

renderPublicErrorPage title msg = render $ page title (Just msg)

render = renderBootstrapPublicPage . publicFrame