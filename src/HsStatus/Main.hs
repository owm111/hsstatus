{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Main
  ( hRunHsStatus
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit

import HsStatus.IO
import HsStatus.Types
import HsStatus.Utils

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: FieldTuple t => Handle -> (StatesOf t -> IO IOString) -> t -> IO ()
hRunHsStatus handle format fields = do
  doneSignal <- newSem
  queue <- newTQueueIO
  fieldStates <- newTVarIO $ initializeStatesOf fields
  let updateStatus = do
        change <- readTQueue queue
        stateTVar fieldStates $ \x -> let y = change x in (y,y)
      monitor = forever $ atomically updateStatus >>= format >>= putAndFlush
      forkField = flip forkFinally $ const $ stopWaitingFor doneSignal
  fieldThreads <- mapM forkField $ startFields queue fields
  monitorThread <- forkIO monitor
  waitFor doneSignal
  mapM_ killThread (monitorThread:fieldThreads)
  exitSuccess
  where putAndFlush str = hPutStrLn handle str >> hFlush handle
