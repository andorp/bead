{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as P

-- Definitions --

class BlazeTemplate b where
  template :: b -> Html

base :: Html -> Maybe Html -> Html
base content loggedInContent = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "content" $ content
    case loggedInContent of
      Nothing -> return ()
      Just inner  -> H.div $ do
        H.div ! A.id "menu1" $ do
          inner

empty :: Html
empty = return ()

errorPage :: Html
errorPage = base "Error oage is not defined" Nothing

