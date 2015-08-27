{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content.Public.ErrorPage (
    template
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.Monoid (mempty)
import           Data.String (fromString)

import qualified Text.Blaze.Html5 as H

import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.RouteOf (loginPath)
import           Bead.View.Translation

template :: (e -> H.Html) -> Translation String -> Maybe e -> IHtml
template content t e = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ Bootstrap.pageHeader $ H.h2 $
      fromString $ msg t
    Bootstrap.rowCol4Offset4 $ do
      H.h3 $ fromString $ msg $ msg_ErrorPage_Header "Some error happened... :-)"
      H.p $ maybe mempty content e
    Bootstrap.rowCol4Offset4 $
      Bootstrap.buttonLink loginLink (msg $ msg_ErrorPage_GoBackToLogin "Back to login")
  where
#ifdef SSO
    loginLink = unpack loginPath
#else
    loginLink = "/"
#endif
