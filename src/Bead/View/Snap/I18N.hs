{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Bead.View.Snap.I18N where

import Text.Blaze.Internal (MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable(..))

import Data.String
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader

import Bead.View.Snap.Dictionary (I18N)

newtype I18NHtmlM a = I18NHtmlM { un18html :: ReaderT I18N MarkupM a }
  deriving (Monad, MonadReader I18N)

type I18NHtml = I18NHtmlM ()

type IHtml = I18NHtmlM ()

runI18NHtmlM :: I18NHtmlM a -> I18N -> MarkupM a
runI18NHtmlM markup = runReaderT (un18html markup)
{-# INLINE runI18NHtmlM #-}

runI18NHtml :: I18NHtml -> I18N -> H.Html
runI18NHtml = runI18NHtmlM
{-# INLINE runI18NHtml #-}

translate :: I18N -> I18NHtml -> H.Html
translate = flip runI18NHtmlM
{-# INLINE translate #-}

--instance IsString (I18NHtmlM ()) where
--  fromString = I18NHtmlM . lift . fromString

i18n :: String -> I18NHtml
i18n s = I18NHtmlM $ do
  translator <- ask
  lift . fromString . translator $ s
{-# INLINE i18n #-}

html :: H.Html -> I18NHtml
html = liftH
{-# INLINE html #-}

mkI18NHtml :: (I18N -> H.Html) -> I18NHtml
mkI18NHtml = liftH2
{-# INLINE mkI18NHtml #-}

liftH :: H.Html -> I18NHtml
liftH = I18NHtmlM . lift
{-# INLINE liftH #-}

liftH2 :: (I18N -> H.Html) -> I18NHtml
liftH2 f = I18NHtmlM $ ReaderT f
{-# INLINE liftH2 #-}

lh :: H.Html -> I18NHtml
lh = I18NHtmlM . lift
{-# INLINE lh #-}

lh2 :: (H.Html -> H.Html) -> I18NHtml -> I18NHtml
lh2 f h = I18NHtmlM $ ReaderT (f . runReaderT (un18html h))
{-# INLINE lh2 #-}

instance Attributable (I18NHtmlM a) where
  h ! a = I18NHtmlM $ ReaderT $ \i -> (runI18NHtmlM h i) ! a
  {-# INLINE (!) #-}

instance Attributable (I18NHtmlM a -> I18NHtmlM b) where
  h ! a = (! a) . h
  {-# INLINE (!) #-}

instance IsString (I18NHtmlM a) where
  fromString t = I18NHtmlM $ ReaderT $ \i -> fromString (i t)
  {-# INLINE fromString #-}

noTranslate :: String -> I18NHtmlM a
noTranslate = I18NHtmlM . lift . fromString
{-# INLINE noTranslate #-}
