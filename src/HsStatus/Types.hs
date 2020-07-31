module HsStatus.Types
  ( Field (..)
  , IOString
  , FormatterFor
  -- * 'Sem'
  , Sem
  , newSem
  , waitFor
  , stopWaitingFor
  -- * 'Unzippable'
  , Unzippable (..)
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


-- | TODO: is this needed?
-- Some types might have more efficient unzip functions.
class Functor f => Unzippable f where
  unzipF :: f (a, b) -> (f a, f b)
  unzipF xs = (fst <$> xs, snd <$> xs)
instance Unzippable [] where
  unzipF = unzip
instance Unzippable Maybe where
  unzipF Nothing = (Nothing, Nothing)
  unzipF (Just (a, b)) = (Just a, Just b)
