{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.MySQLTestRunner where

import Bead.Persistence.Initialization
import Bead.Persistence.SQL.Entities (Persist)
import Bead.Persistence.SQL.MySQL

#ifdef TEST
import Data.IORef
import System.IO.Unsafe
#endif

#ifdef TEST

runPersist = runInterpreter

-- TODO: FIX this dirty hack to instatiate only once the persistent layer
persistRef :: IORef Interpreter
persistRef = unsafePerformIO $ newIORef undefined

createInterpreter :: IO ()
createInterpreter = do
  interp <- createPersistInterpreter defaultConfig
  writeIORef persistRef interp

getPersistInterpreter :: IO Interpreter
getPersistInterpreter = readIORef persistRef

runPersistIOCmd :: Persist a -> IO a
runPersistIOCmd m = do
  interp <- getPersistInterpreter
  x <- runPersist interp m
  case x of
    Left msg -> fail msg -- >> return undefined
    Right x  -> return x

runSql :: Persist a -> IO a
runSql p = do
  createInterpreter
  reinitPersistence
  runPersistIOCmd $ do
    p

reinitPersistence = do
  init <- createPersistInit defaultConfig
  tearDown init
  initPersist init
#endif
