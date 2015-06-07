module Bead.View.DictionaryLoader where

import           Control.Applicative ((<$>))
import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Exception
import           Control.Monad (filterM)
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Unsafe.Coerce

import           Bead.View.Dictionary
import           Bead.View.Translation
import           Bead.Domain.Types

import           DynFlags
import           GHC
import           GHC.Paths
import           Linker
import           MonadUtils
import           Packages

-- Implements merging dictionaries with the (optional) corresponding patches
-- preceded by some sanity checks.
patchDictionaries :: [(FilePath, DictionaryFile)] -> Either String Dictionaries
patchDictionaries dfs =
  let dfiles = [ (path, d) | (path, d@(DictionaryFile {})) <- dfs ] in
  let pfiles = [ (path, p) | (path, p@(DictionaryPatchFile {})) <- dfs ] in
  let repeatedLangs = repeated [ (path, langCode d) | (path, d) <- dfiles ] in
  let allEntries = concat [ map (\e -> (path, lang p, e)) (entries p) | (path, p) <- pfiles ] in
  let overlappingPatches = overlapping allEntries in
  case (repeatedLangs, overlappingPatches) of
    ([],[]) ->
      let dicts  = Map.fromList [ (langCode d, d) | (_, d) <- dfiles ] in
      let dicts' = foldr applyPatches dicts [ p | (_, p) <- pfiles ] in
      Right . Map.fromList $
        map ((Language . langCode) &&& (dictionaryFromDFile &&& dictionaryFileToInfo)) $
          Map.elems dicts'
    (xs,ys) ->
      Left . unlines . concat $
        [ "The following dictionaries implement the same language:"
          : [ tab 2 ++ lang ++ ": " ++ intercalate ", " paths
            | (paths, lang) <- xs ]
          | not (null xs)
        ] ++
        [ [""] | not (null xs) && not (null ys) ] ++
        [ "The following dictionary patches override the same lines:"
          : [ tab 2 ++ lang ++ ": " ++ intercalate ", " paths ++ ": " ++ label
            | (paths, lang, label) <- ys ]
          | not (null ys)
        ]
    where
      tab = (flip replicate) ' '

      lang (DictionaryFile { langCode = lc })     = lc
      lang (DictionaryPatchFile { parent = par }) = lang par

      repeated =
        sortBy (compare `on` lang) >>>
        groupBy ((==) `on` lang)   >>>
        filter ((> 1) . length)    >>>
        map (\xs@(x:_) -> (map path xs, lang x))
        where
          (path, lang) = (fst, snd)

      overlapping =
        sortBy (compare `on` langTid) >>>
        groupBy ((==) `on` langTid)   >>>
        filter ((> 1) . length)       >>>
        map (\xs@((_,lang,entry):_) -> (map path xs, lang, tlabel entry))
        where
          langTid (_,x,y) = (x,tid y)
          path (x,_,_)    = x


      applyPatches p = Map.adjust apply (lang p)
        where
          patches = entries p
          apply d = d { entries = process $ entries d }
            where
              process =
                map unT >>> Map.fromList >>>
                (\d -> foldr patch d patches)  >>>
                Map.toList >>> map T

              patch p = Map.adjust (const $ trans p) (tid p)

-- Reads up all the dictionary files from a given directory and passes them for
-- patching the dictionaries (if needed).
loadDictionaries :: FilePath -> IO (Either String Dictionaries)
loadDictionaries d = do
  files   <- ((map fullPath . filter dictionaryFile) <$> getDirectoryContents d) >>= (filterM doesFileExist)
  modules <- catMaybes <$> mapM loadDictionary files
  return $ patchDictionaries modules
  where
    fullPath f = d </> f

    dictionaryFile =
      splitExtension >>>
      ("Dictionary" `isPrefixOf`) *** (== ".hs") >>> uncurry (&&)

loadDictionary :: FilePath -> IO (Maybe (FilePath, DictionaryFile))
loadDictionary path = do
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 { hscTarget   = HscInterpreted
                            , log_action  = defaultLogAction
                            , flushOut    = FlushOut $ return ()
                            , flushErr    = FlushErr $ return ()
                            , importPaths = importPaths dflags0
                                ++ [takeDirectory path]
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
      let dict = takeBaseName path ++ ".dict"
      (Just . ((,) path)) <$>
        compileExpr (dict ++ " :: DictionaryFile") >>= unsafeCoerce
  `catch` (\e -> do
    printf "Could not load dictionary: %s\n" (show (e :: SomeException))
    return Nothing)
