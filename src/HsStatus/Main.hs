{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Main
  ( hRunHsStatus
  , hMonitorVars
  , makeVarAndFork
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Zip
import System.Exit

import HsStatus.IO
import HsStatus.Types
import HsStatus.Utils

-- | Given a field, start it on a new thread and return the variable it will
-- update and thread's ID.
--
-- TODO: better exception handling.
makeVarAndFork :: Sem -> Field -> IO (TVar IOString, ThreadId)
makeVarAndFork doneSignal (Field f) = do
  var <- newTVarIO "Updating..."
  thread <- f var `forkFinally` (const $ stopWaitingFor doneSignal)
  return (var, thread)

-- | Monitors a traversable structure of variable, formats them with a given
-- function, and print to the given handle on change.
--
-- TODO: exception handling!
hMonitorVars :: Traversable t => Handle -> IOString -> FormatterFor (t IOString) -> t (TVar IOString) -> IO ()
hMonitorVars handle last format vars = do
  str <- atomically $ do
    string <- format <$> mapM readTVar vars
    check (string /= last)
    return string
  hPutStrLn handle str
  hFlush handle
  hMonitorVars handle str format vars

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: (Traversable t, MonadZip t) => Handle -> FormatterFor (t IOString) -> t Field -> IO ()
hRunHsStatus handle format fields = do
  doneSignal <- newSem
  (vars, threads) <- unzipWithM (makeVarAndFork doneSignal) fields
  monitorThread <- forkIO $ hMonitorVars handle "" format vars
  waitFor doneSignal
  killThread monitorThread
  mapM_ killThread threads
  exitSuccess
