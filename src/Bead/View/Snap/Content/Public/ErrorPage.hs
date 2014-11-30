{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Snap.Content.Public.ErrorPage (
    template
  ) where

import           Data.Monoid (mempty)
import           Data.String (fromString)

import qualified Text.Blaze.Html5 as H

import qualified Bead.View.Snap.Content.Bootstrap as Bootstrap
import           Bead.View.Snap.I18N (IHtml, getI18N)
import           Bead.View.Snap.Translation

template :: (e -> H.Html) -> Translation String -> Maybe e -> IHtml
template content t e = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ Bootstrap.pageHeader $ H.h2 $
      fromString $ msg t
    Bootstrap.rowCol4Offset4 $ do
      H.h3 $ fromString $ msg $ Msg_ErrorPage_Header "Some error happened... :-)"
      H.p $ maybe mempty content e
    Bootstrap.rowCol4Offset4 $
      Bootstrap.buttonLink "/" (msg $ Msg_ErrorPage_GoBackToLogin "Back to login")

