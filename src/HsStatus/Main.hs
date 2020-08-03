{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Main
  ( hRunHsStatus
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

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: (Traversable t, MonadZip t) => Handle -> FormatterFor (t IOString) -> t Field -> IO ()
hRunHsStatus handle format fields = do
  doneSignal <- newSem
  (vars, threads) <- unzipWithM (makeVarAndFork doneSignal) fields
  let monitor last = do
        status <- atomically $ do
          current <- format <$> mapM readTVar vars
          check (current /= last)
          return current
        hPutStrLn handle status
        hFlush handle
        monitor status
  monitorThread <- forkIO (monitor "")
  waitFor doneSignal
  killThread monitorThread
  mapM_ killThread threads
  exitSuccess
