module Bead.Controller.Logging where

import Prelude hiding (log)

data LogLevel
  = DEBUG
  | INFO
  | ERROR

data Logger = Logger {
    log :: LogLevel -> String -> IO ()
  }

mockLogger = Logger {
    log = \_ msg -> putStrLn msg
  }

