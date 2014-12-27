{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Bead.View.I18N where

import           Data.String
import           Control.Monad.Trans.Reader

import           Text.Blaze.Internal (MarkupM)
import qualified Text.Blaze.Html5 as H

import           Bead.View.Translation

type IHtml = Reader I18N (MarkupM ())

translate :: I18N -> IHtml -> H.Html
translate = flip runReader
{-# INLINE translate #-}

-- Produces a Html snipet combining the given translation with the given template
i18n :: I18N -> IHtml -> H.Html
i18n = flip runReader
{-# INLINE i18n #-}

html :: H.Html -> IHtml
html = return
{-# INLINE html #-}

getI18N :: Reader I18N (Translation String -> String)
getI18N = asks (\f -> fromString . f)
{-# INLINE getI18N #-}

liftH :: H.Html -> IHtml
liftH = return
{-# INLINE liftH #-}

instance IsString IHtml where
  fromString t = return (fromString t)
  {-# INLINE fromString #-}
