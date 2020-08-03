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
runEvery :: Int -> IO IOString -> Field
runEvery t f = Field $ \var -> let go = f >>= atomically . writeTVar var >> threadDelay t >> go in go


-- | Creates a field that displays output from the given handle line-by-line.
-- The thread is killed when an EOF is reached.
--
-- TODO: versions that run a function on input, that don't exit on EOF, and
-- for only stdin.
readHandle :: Handle -> Field
readHandle handle = Field $ \var -> forever $
  exitIfEOF >> hGetLine handle >>= atomically . writeTVar var
  where exitIfEOF = hIsEOF handle >>= flip when (myThreadId >>= killThread)

-- | Creates a field that runs a function on an INotify event for a given path.
--
-- TODO: try and consolidate all watchers on all fields to a single inotify
-- instance.
-- TODO: handle exceptions.
-- TODO: check if it is actually necessary to remove watchers when killing the
-- inot.
-- TODO: find a better solution for running immediately.
iNotifyWatcher :: [IN.EventVariety] -> ByteString -> (IN.Event -> IO IOString) -> Field
iNotifyWatcher events path action = Field $ \var ->
  let go = action >=> atomically . writeTVar var
  in do go initialEvent
        signal <- newSem
        bracket IN.initINotify
          (\inot -> stopWaitingFor signal >> IN.killINotify inot)
          (\inot ->
            bracket (IN.addWatch inot events path go)
              IN.removeWatch
              (const $ waitFor signal))
  where initialEvent = IN.Modified False (Just path)

-- | Creates an action that communicates with an external process.
--
-- TODO: exception handling.
-- TODO: first run? how?
-- TODO: would it be better to have func return the value instead of passing it
-- a "callback"? forever $ func handles >>= sendVal
-- TODO: better name
watchProcess :: ProcessConfig stdin stdout stderr -> ((IOString -> IO ()) -> Process stdin stdout stderr -> IO ()) -> Field
watchProcess proc func = Field $ \var ->
  let sendVal = atomically . writeTVar var
   in withProcessWait proc (func sendVal)
