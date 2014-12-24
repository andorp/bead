{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Snap.ErrorPage (
    ErrorPage(..)
  , defErrorPage
  , msgErrorPage
  , translationErrorPage
  ) where

import           Data.String

import           Snap

import           Bead.View.Snap.Application (App)
import qualified Bead.View.Snap.Content.Public.ErrorPage as View
import           Bead.View.Snap.HandlerUtils
import           Bead.View.Snap.I18N (IHtml, getI18N)
import           Bead.View.Snap.Pagelets (publicFrame)
import           Bead.View.Snap.Translation

class ErrorPage e where
  errorPage :: Translation String -> e -> Handler App b ()

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

msgErrorPage :: String -> Handler App b ()
msgErrorPage = defErrorPage

defErrorPage :: (ErrorPage e) => e -> Handler App b ()
defErrorPage = errorPage (Msg_ErrorPage_Title "Error!")

-- Produces a handler that renders the error page, with the
-- given title and message for the user
translationErrorPage :: Translation String -> Translation String -> Handler App b ()
translationErrorPage = errorPage

page :: Translation String -> (Maybe String) -> IHtml
page = View.template fromString

pageTranslation :: Translation String -> (Maybe (Translation String)) -> IHtml
pageTranslation title err = do
  msg <- getI18N
  View.template (fromString . msg) title err

renderPublicErrorPage title msg = render $ page title (Just msg)

render = renderBootstrapPublicPage . publicFrame