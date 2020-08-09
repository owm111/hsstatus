module HsStatus.FieldUtils
  ( runEvery
  , readHandle
  , iNotifyWatcher
  , watchProcess
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified System.INotify as IN
import System.Process.Typed

import HsStatus.IO
import HsStatus.Types

-- | Creates a field that runs an IO action every @t@ microseconds.
runEvery :: Int -> IO (Either ByteString a) -> Field a
runEvery = PeriodicField


-- | Creates a field that displays output from the given handle line-by-line.
-- The thread is killed when an EOF is reached.
--
-- TODO: versions that run a function on input, that don't exit on EOF, and
-- for only stdin.
readHandle :: Handle -> Field ByteString
readHandle handle = SimpleField $ \sendChange -> forever $
  exitIfEOF >> hGetLine handle >>= sendChange . Right
  where exitIfEOF = hIsEOF handle >>= flip when (myThreadId >>= killThread)

-- | Creates a field that runs a function on an INotify event for a given path.
--
-- TODO: handle exceptions.
iNotifyWatcher :: [([IN.EventVariety], ByteString)] -> (Maybe IN.Event -> IO (Either ByteString a)) -> Field a
iNotifyWatcher pairs = WatcherField (map (uncurry Watcher) pairs)

-- | Creates an action that communicates with an external process.
--
-- TODO: exception handling.
-- TODO: first run? how?
-- TODO: would it be better to have func return the value instead of passing it
-- a "callback"? forever $ func handles >>= sendVal
-- TODO: better name
watchProcess :: ProcessConfig stdin stdout stderr -> ((Either ByteString a -> IO ()) -> Process stdin stdout stderr -> IO ()) -> Field a
watchProcess = ProcessField
