{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Main
  ( hRunHsStatus
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import System.Exit
import System.INotify (withINotify)

import HsStatus.IO
import HsStatus.Types.FieldTuple (FieldTuple (..))
import HsStatus.Types.Sem (newSem, stopWaitingFor, waitFor)
import HsStatus.Types.Starter (Starter (..))
import HsStatus.Utils

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: FieldTuple t => Handle -> (StateTuple t -> IO ByteString) -> t -> IO ()
hRunHsStatus handle format fields = do
  doneSignal <- newSem
  queue <- newTQueueIO
  fieldStates <- newTVarIO $ initialStateFor fields
  let updateStatus = do
        change <- readTQueue queue
        stateTVar fieldStates $ \x -> let y = change x in (y,y)
      monitor = forever $ atomically updateStatus >>= format >>= putAndFlush
      forkField = flip forkFinally $ const $ stopWaitingFor doneSignal
      Starter startup fieldThreads maybeWatcher = fieldsStarter ((.) (atomically . writeTQueue queue)) fields
      allThreads =
        case maybeWatcher of
          Just go -> withINotify (\i -> go i >> waitFor doneSignal) : fieldThreads
          Nothing -> fieldThreads
  startup
  fieldThreads <- mapM forkField allThreads
  monitorThread <- forkIO monitor
  waitFor doneSignal
  mapM_ killThread (monitorThread:fieldThreads)
  exitSuccess
  where putAndFlush str = hPutStrLn handle str >> hFlush handle
