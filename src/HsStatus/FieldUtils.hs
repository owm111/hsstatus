{-# LANGUAGE OverloadedStrings #-}

module HsStatus.FieldUtils
  ( runEvery
  , readHandle
  , iNotifyWatcher
  , watchProcess
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Concurrent.STM.TSem (signalTSem)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, hGetLine)
import System.Exit (exitSuccess)
import System.INotify (Event, EventVariety, initINotify, addWatch)
import System.Posix.Signals (raiseSignal, softwareTermination)
import System.IO (Handle, hIsEOF)
import System.Process.Typed (Process, ProcessConfig, withProcessWait)

import HsStatus.Types.Field (Field (..))

singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

-- | Creates a field that runs an IO action every @t@ microseconds.
runEvery :: Int -> a -> IO a -> Field a
runEvery delay starting f =
  let f' printSem var = singleton . forkIO . forever $ do
        x <- f
        atomically (writeTVar var x >> signalTSem printSem)
        threadDelay delay
   in Field (starting, f')


-- | Creates a field that displays output from the given handle line-by-line.
-- The thread is killed when an EOF is reached.
--
-- TODO: versions that run a function on input, that don't exit on EOF, and
-- for only stdin.
readHandle :: Handle -> Field ByteString
readHandle handle =
  let f printSem var = singleton . forkIO . forever $ do
        finished <- hIsEOF handle
        when finished (raiseSignal softwareTermination)
        line <- hGetLine handle
        atomically (writeTVar var line >> signalTSem printSem)
   in Field ("", f)

-- | Creates a field that runs a function on an INotify event for a given path.
--
-- TODO: handle exceptions.
iNotifyWatcher :: [([EventVariety], ByteString)] -> a -> (Maybe Event -> IO a) -> Field a
iNotifyWatcher pairs starting f =
  let f' printSem var = do
        inot <- initINotify
        let go ev = do
              x <- f (Just ev)
              atomically (writeTVar var x >> signalTSem printSem)
        traverse (\(evs, path) -> addWatch inot evs path go) pairs
        [] <$ f Nothing
   in Field (starting, f')

-- | Creates an action that communicates with an external process.
--
-- TODO: exception handling.
-- TODO: first run? how?
-- TODO: would it be better to have func return the value instead of passing it
-- a "callback"? forever $ func handles >>= sendVal
-- TODO: better name
watchProcess :: ProcessConfig stdin stdout stderr -> a -> ((a -> IO ()) -> Process stdin stdout stderr -> IO ()) -> Field a
watchProcess conf starting f =
  let f' printSem var = singleton . forkIO . withProcessWait conf $
        f (\x -> atomically (writeTVar var x >> signalTSem printSem))
   in Field (starting, f')
