{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Headers.AcceptLanguage (
    setLanguageFromAcceptLanguage
#ifdef TEST
  , acceptLanguageTests
#endif
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List (nub, intersect)
import           Data.Maybe
import           Data.String.Utils
import           Prelude hiding (id)

import           Bead.Domain.Entities (Language(..))
import           Bead.Domain.Types (readMaybe)
import           Bead.View.BeadContext
import           Bead.View.Content hiding (BlazeTemplate, template)
import           Bead.View.Session (setLanguageInSession)

#ifdef TEST
import           Test.Tasty.TestSet
#endif

setLanguageFromAcceptLanguage :: BeadHandler' b ()
setLanguageFromAcceptLanguage = do
  acceptLanguages <- getHeaders "Accept-Language" <$> getRequest
  case acceptLanguages of
    Nothing -> return ()
    Just ls -> do
      let languages = nub . map acceptLanguageToLanguage
                          . concat
                          $ map (parseAcceptLanguageLine . BS.unpack) ls
      dictionaryLanguages <- map fst <$> dcGetDictionaryInfos
      let selectedLang = languages `intersect` dictionaryLanguages
      case selectedLang of
        []    -> return ()
        (l:_) -> setLanguageInSession l

-- The possible accept langauge value in the Accept-Language headers
data AcceptLanguage
  = AL_Simple  String
  | AL_Quality String Double
  deriving (Eq, Show)

-- Eg:
-- AL_Simple "en-US" represents "en-US"
-- AL_quality "en" 0.5 represents "en;q=0.5"

acceptLanguage
  simple
  quality
  al = case al of
    AL_Simple  s   -> simple s
    AL_Quality s q -> quality s q

parseAcceptLangValue :: String -> Maybe AcceptLanguage
parseAcceptLangValue s = case split ";" s of
  [lang]            -> Just $ AL_Simple lang
  [lang, 'q':'=':q] -> AL_Quality lang <$> readMaybe q
  _                 -> Nothing

parseAcceptLanguageLine :: String -> [AcceptLanguage]
parseAcceptLanguageLine = catMaybes . map (parseAcceptLangValue . strip) . split ","

-- Convert an accept language to language dropping the localization
-- and the quality informations
acceptLanguageToLanguage = acceptLanguage
  (Language . dropLocalization)
  (\lang _q -> Language $ dropLocalization lang)
  where
    dropLocalization = takeWhile (/='-')


#ifdef TEST
acceptLanguageTests = group "accpetLanguage" $ do
  group "parse" $ do
    eqPartitions parseAcceptLangValue
      [ Partition "accept language parse empty string" "" Nothing "Empty string is parsed"
      , Partition "simple accept language" "en-US" (Just $ AL_Simple "en-US") "Simple language parameter is not parsed correctly"
      , Partition "simple accept language" "en;q=0.5" (Just $ AL_Quality "en" 0.5) "Simple language parameter is not parsed correctly"
      , Partition "noise for accept langauge" "qns;sdjfkj" Nothing "Noise is parsed"
      ]
    assertEquals
      "accept language line"
      [AL_Simple "en-US", AL_Quality "en" 0.5]
      (parseAcceptLanguageLine "en-US , en;q=0.5")
      "Parse line missed some of the arguments"
  group "accept language to language" $ eqPartitions acceptLanguageToLanguage
    [ Partition "Simple with localization" (AL_Simple "en-US") (Language "en") "Drop localization has failed"
    , Partition "Quality with localization" (AL_Quality "en-US" 0.5) (Language "en") "Drop localization has failed"
    ]
#endif
