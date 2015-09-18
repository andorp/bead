module Bead.Controller.Logging where

import Prelude hiding (log)

data LogLevel
  = DEBUG
  | INFO
  | ERROR
  deriving (Show)

data Logger = Logger {
    log :: LogLevel -> String -> IO ()
  }

mockLogger = Logger {
    log = \_ msg -> putStrLn msg
  }

-- | Logger that does not log anything
nullLogger = Logger {
    log = \_ _ -> return ()
  }
