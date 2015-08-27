module Bead.View.Common (
    languageMenu
  , setDefaultLanguage
  ) where

import           Control.Monad
import           Data.Maybe
import           Data.String

import           Bead.View.BeadContext
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Dictionary
import           Bead.View.Headers.AcceptLanguage
import           Bead.View.RouteOf
import           Bead.View.Session
import           Bead.View.Translation

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

-- Set the default language in session if no information is found
setDefaultLanguage = do
  languages <- dcGetDictionaryInfos
  mLangInSession <- languageFromSession
  when (isNothing mLangInSession) $ do
    defaultLang <- configuredDefaultDictionaryLanguage
    setLanguageInSession defaultLang
    setLanguageFromAcceptLanguage
    commitSessionTop
  return languages

languageMenu msg languages = do
  when (length languages > 1) $ do
    Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $
      Bootstrap.dropdown (msg $ msg_Login_SelectLanguage "Select a language") $
      for languages $ \(language,info) -> do
        link (queryString changeLanguagePath [requestParam language])
           (dictionaryInfoCata (\_icon -> fromString) info)
  where
    for = flip Prelude.map
    link ref text = a ! href ref $ text
