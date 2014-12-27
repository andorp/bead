module Main where

import Bead.View.Dictionary
import Bead.View.Translation
import Bead.View.TranslationEnum

{-
Creates dictionary file named language.dict
-}

createExampleLanguageFile :: FilePath -> IO ()
createExampleLanguageFile fp = writeFile fp $ show DictionaryFile {
    iconFile = "icon.ico"
  , langCode = "lr"
  , langName = "Language"
  , entries = map (\t -> t { trans = "TODO" }) translations
  } where
      translations :: [Translation ()]
      translations = [minBound .. maxBound]

main = do
  createExampleLanguageFile "language.dict"
