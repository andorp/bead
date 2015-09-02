{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Public.Index (
    index
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.String (fromString)

import           Bead.View.Common
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (getI18N)
import           Bead.View.Markdown
import           Bead.View.RouteOf
import           Bead.View.Translation

import qualified Text.Blaze.Html5 as H

fromMarkdown = markdownToHtml

index languages = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ Bootstrap.pageHeader $ H.h2 $
      fromString $ msg $ msg_Index_Header "Welcome"
    Bootstrap.rowCol4Offset4 $ do
      fromMarkdown $ fromString $
        msg $ msg_Index_Body $ unlines
          [ "This page can be only used with an Active Dictory account associated with the hosting institute."
          , ""
          , "With such an account available, please proceed with clicking on the **Proceed** button."
          , ""
          , "*Note:* For the more secure login, it is recommended to use [IWA](https://en.wikipedia.org/wiki/Integrated_Windows_Authentication) in the browsers that support that."
          ]

      Bootstrap.blockButtonLink (unpack loginPath) (msg $ msg_Index_Proceed "Proceed")
    languageMenu msg languages
