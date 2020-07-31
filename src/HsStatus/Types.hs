module HsStatus.Types
  ( Field (..)
  , IOString
  , FormatterFor
  -- * 'Sem'
  , Sem
  , newSem
  , waitFor
  , stopWaitingFor
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString) 

-- | A field represents something that needs to be monitored and displayed. For
-- example, time, battery, volume, etc.
newtype Field = Field (TVar IOString -> IO ())

-- | Alias for string type used for input and output.
type IOString = ByteString

-- | Alias for a function that converts @a@ to 'IOString'.
type FormatterFor a = a -> IOString

-- | Abstracted boolean semaphore.
type Sem = TMVar ()

-- | Creates a new 'Sem'.
newSem :: IO Sem
newSem = newEmptyTMVarIO

-- | Blocks until provided 'Sem' is signalled.
waitFor :: Sem -> IO ()
waitFor = atomically . takeTMVar

-- | Stops blocking 'waitFor' threads.
stopWaitingFor :: Sem -> IO ()
stopWaitingFor = atomically . flip putTMVar ()
