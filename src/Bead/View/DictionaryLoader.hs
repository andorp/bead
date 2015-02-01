module Bead.View.DictionaryLoader where

import           Control.Applicative ((<$>))
import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Exception
import           Control.Monad (filterM)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath (splitExtension, joinPath)
import           Text.Printf
import           Unsafe.Coerce

import           Bead.View.Dictionary
import           Bead.Domain.Types

import           DynFlags
import           GHC
import           GHC.Paths
import           Linker
import           MonadUtils
import           Packages

-- Reads up all the dictionary files from a given directory, and
-- creates the dictionaries map
loadDictionaries :: FilePath -> IO Dictionaries
loadDictionaries d = do
  putStrLn "Searching for dictionaries..."
  fs <- ((map fullPath  . filter dictionaryFile) <$> getDirectoryContents d) >>= (filterM doesFileExist)
  ds <- catMaybes <$> mapM loadDictionary fs
  return . Map.fromList
         . map ((Language . langCode) &&& (dictionaryFromDFile &&& dictionaryFileToInfo))
         $ ds
    where
      fullPath f = joinPath [d,f]
      dictionaryFile =
        splitExtension >>>
        ("Dictionary" `isPrefixOf`) *** (== ".hs") >>> uncurry (&&)

loadDictionary :: FilePath -> IO (Maybe DictionaryFile)
loadDictionary path = do
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 { hscTarget  = HscInterpreted
                            , log_action = defaultLogAction
                            , flushOut   = FlushOut $ return ()
                            , flushErr   = FlushErr $ return ()
                            }
      _ <- setSessionDynFlags dflags1
      (dflags2,_) <- liftIO $ initPackages dflags1
      liftIO $ unload dflags2 []
      t <- guessTarget path Nothing
      setTargets [t]
      ok <- load LoadAllTargets
      case ok of
        Succeeded -> do
          modGraph <- getModuleGraph
          let modNames = map ms_mod_name modGraph
          loadedModNames <- filterM isLoaded modNames
          let imps = map (IIDecl . simpleImportDecl) $
                       mkModuleName "Bead.View.Dictionary" : loadedModNames
          setContext imps
        Failed    ->
          error "Failed to load the requested module (see the error messages)."
      -- Unfortunately, `Data.Dynamic` appears too tricky to use, so instead
      -- we just simply prescribe the expected type for `dict` and blindly
      -- cast the fetched value into a `DictionaryFile`.  Theoretically, the
      -- evaluation will not be able to reach this expression otherwise anyway.
      Just <$> compileExpr "dict :: DictionaryFile" >>= unsafeCoerce
  `catch` (\e -> do
    printf "Could not load dictionary: %s\n" (show (e :: SomeException))
    return Nothing)
