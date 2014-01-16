{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Bead.View.Snap.I18N where

import Control.Monad (join)

import Text.Blaze.Internal (MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable(..))

import Data.String
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader

import Bead.View.Snap.Dictionary (I18N)
import Bead.View.Snap.Translation


type IHtml = Reader I18N (MarkupM ())

translate :: I18N -> IHtml -> H.Html
translate = flip runReader
{-# INLINE translate #-}

-- Produces a Html snipet combining the given translation with the given template
i18n :: I18N -> IHtml -> H.Html
i18n = flip runReader
{-# INLINE i18n #-}

noTranslate :: IHtml -> H.Html
noTranslate = flip runReader trans
{-# INLINE noTranslate #-}

html :: H.Html -> IHtml
html = return
{-# INLINE html #-}

getI18N :: Reader I18N (Translation String -> String)
getI18N = asks (\f -> fromString . f)

liftH :: H.Html -> IHtml
liftH = return
{-# INLINE liftH #-}

instance IsString IHtml where
  fromString t = return (fromString t)
  {-# INLINE fromString #-}
