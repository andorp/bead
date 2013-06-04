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

runI18NHtmlM :: I18NHtmlM a -> I18N -> MarkupM a
runI18NHtmlM markup = runReaderT (un18html markup)

runI18NHtml :: I18NHtml -> I18N -> H.Html
runI18NHtml = runI18NHtmlM

translate :: I18N -> I18NHtml -> H.Html
translate = flip runI18NHtmlM

instance IsString (I18NHtmlM ()) where
  fromString = I18NHtmlM . lift . fromString

i18n :: String -> I18NHtml
i18n s = I18NHtmlM $ do
  translator <- ask
  lift . fromString . translator $ s

html :: H.Html -> I18NHtml
html = liftH

mkI18NHtml :: (I18N -> H.Html) -> I18NHtml
mkI18NHtml = liftH2

liftH :: H.Html -> I18NHtml
liftH = I18NHtmlM . lift

liftH2 :: (I18N -> H.Html) -> I18NHtml
liftH2 f = I18NHtmlM $ ReaderT f

{- TODO: HTML type class: performance check
c :: (MarkupM a -> MarkupM b) -> I18NHtmlM a -> I18NHtmlM b
c f m = I18NHtmlM $ ReaderT (f . runReaderT (un18html m))

instance Attributable (I18NHtmlM a) where
  h ! a = I18NHtmlM $ ReaderT $ \i -> (runI18NHtmlM h i) ! a

instance Attributable (I18NHtmlM a -> I18NHtmlM b) where
  h ! a = (! a) . h
-}

