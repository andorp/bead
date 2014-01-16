{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.ErrorPage (
    errorPage
  , msgErrorPage
  , errorPageWithTitle
  , errorPageWithTitleTrans
  ) where

import Data.String

import Snap
import Snap.Blaze (blaze)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Controller.Pages (Page(Login))
import Bead.View.Snap.Application (App)
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.Pagelets (
    link, withTitleAndHead
  )
import Bead.View.Snap.I18N (IHtml, noTranslate, getI18N)
import Bead.View.Snap.Translation
import qualified Text.Blaze.Html5 as H

-- | Produces an error page showing the reason of an error, and redirects to
--   login page after a while
errorPage :: ContentHandlerError -> Handler App b ()
errorPage = contentHandlerErrorMap (render . (page $ Msg_ErrorPage_Title "Error"))

msgErrorPage :: String -> Handler App b ()
msgErrorPage = render . (page $ Msg_ErrorPage_Title "Error") . Just

-- Produces a handler that renders the error page, with the
-- given title and message for the user
errorPageWithTitleTrans :: Translation String -> Translation String -> Handler App b ()
errorPageWithTitleTrans title = render . (pageTranslation title) . Just

-- Produces a handler that renders the error page, with the
-- given title and message for the user
errorPageWithTitle :: Translation String -> String -> Handler App b ()
errorPageWithTitle title = render . (page title) . Just

pageTemplate :: (a -> H.Html) -> Translation String -> Maybe a -> IHtml
pageTemplate content t e = withTitleAndHead t $ do
  msg <- getI18N
  return $ do
    H.div $ do
      H.h2 $ (fromString $ msg $ Msg_ErrorPage_Header "Valami hiba történt... :-)")
      H.p $
        maybe (return ()) content e
      H.br
    H.div $
      link "/" (msg $ Msg_ErrorPage_GoBackToLogin "Vissza a bejelentkezésehez")

page :: Translation String -> (Maybe String) -> IHtml
page = pageTemplate fromString

pageTranslation :: Translation String -> (Maybe (Translation String)) -> IHtml
pageTranslation t e = do
  msg <- getI18N
  pageTemplate (fromString . msg) t e

-- Renders the page with no translation
render :: MonadSnap m => IHtml -> m ()
render = blaze . noTranslate
