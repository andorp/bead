module Bead.View.Snap.Logger where

import Data.ByteString.Char8
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
  l <- S.newLogger logFile

  let logger lvl msg = S.logMsg l (pack msg)

  return $ SnapLogger {
      snapLogger = L.Logger logger
    , stopLogger = S.stopLogger l
    }
