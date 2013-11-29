module Bead.View.Snap.Logger where

import Data.Time
import Data.ByteString.Char8 (pack)

import System.FilePath (dropFileName)
import System.Directory (createDirectoryIfMissing)
import System.Locale (defaultTimeLocale)

import qualified System.FastLogger       as S
import qualified Bead.Controller.Logging as L

-- Represents a logger and an action to stop them
data SnapLogger = SnapLogger {
    snapLogger :: L.Logger
  , stopLogger :: IO ()
  }

-- | Creates a SnapLogger that logs every entry on every level
-- to the given file, assuming that the file exist and only
-- used by this logger
createSnapLogger :: FilePath -> IO SnapLogger
createSnapLogger logFile = do
  createDirectoryIfMissing True $ dropFileName logFile
  l <- S.newLogger logFile

  let logger lvl msg = (logMessage lvl msg) >>= (S.logMsg l . pack)

  return $ SnapLogger {
      snapLogger = L.Logger logger
    , stopLogger = S.stopLogger l
    }

  where
    logMessage lvl msg = do
      now <- getCurrentTime
      return $ concat ["[", formatTime defaultTimeLocale "%c" now, "] ", show lvl, " - ", msg]
