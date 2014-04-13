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
import qualified Text.Blaze.Html5 as H

import           Bead.View.Snap.Application (App)
import           Bead.View.Snap.HandlerUtils
import           Bead.View.Snap.I18N (IHtml, getI18N)
import           Bead.View.Snap.Pagelets (link, withTitleAndHead)
import           Bead.View.Snap.Translation

class ErrorPage e where
  errorPage :: Translation String -> e -> Handler App b ()

instance ErrorPage String where
  errorPage title msg = renderPublicErrorPage title msg

instance ErrorPage (Translation String) where
  errorPage title msg = renderPublicPage . (pageTranslation title) $ Just msg

instance ErrorPage TransMsg where
  errorPage title msg = do
    i18n <- i18nH
    renderPublicPage . (page title) $ Just (translateMessage i18n msg)

instance ErrorPage ContentHandlerError where
  errorPage title msg = contentHandlerErrorMap (renderPublicPage . (page title)) msg

msgErrorPage :: String -> Handler App b ()
msgErrorPage = defErrorPage

defErrorPage :: (ErrorPage e) => e -> Handler App b ()
defErrorPage = errorPage (Msg_ErrorPage_Title "Error!")

-- Produces a handler that renders the error page, with the
-- given title and message for the user
translationErrorPage :: Translation String -> Translation String -> Handler App b ()
translationErrorPage = errorPage -- renderPublicPage . (pageTranslation title) . Just

pageTemplate :: (a -> H.Html) -> Translation String -> Maybe a -> IHtml
pageTemplate content t e = withTitleAndHead t $ do
  msg <- getI18N
  return $ do
    H.div $ do
      H.h2 $ (fromString $ msg $ Msg_ErrorPage_Header "Some error happened... :-)")
      H.p $
        maybe (return ()) content e
      H.br
    H.div $
      link "/" (msg $ Msg_ErrorPage_GoBackToLogin "Back to login")

page :: Translation String -> (Maybe String) -> IHtml
page = pageTemplate fromString

pageTranslation :: Translation String -> (Maybe (Translation String)) -> IHtml
pageTranslation t e = do
  msg <- getI18N
  pageTemplate (fromString . msg) t e

renderPublicErrorPage title msg = renderPublicPage . (page title) $ (Just msg)
