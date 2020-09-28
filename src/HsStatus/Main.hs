module HsStatus.Main
  ( hRunHsStatus
  ) where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Vector (Vector)
import System.IO
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal, softwareTermination)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import HsStatus.Types.FieldTuple (FieldTuple (..))

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: FieldTuple t => t -> IO ()
hRunHsStatus fields = do
  chan <- newChan :: IO (Chan (Int, ByteString))
  doneVar <- newEmptyMVar
  (initialVect, cleanup) <- startFields chan doneVar fields
  hSetBuffering stdout LineBuffering
  printThread <- forkIO $ do
    updates <- getChanContents chan
    mapM_ (BS.hPutStrLn stdout) $ map (V.foldl1' (<>)) $ scanl' setValue initialVect updates
  let setDone = putMVar doneVar ()
  installHandler keyboardSignal (Catch setDone) Nothing
  installHandler softwareTermination (Catch setDone) Nothing
  takeMVar doneVar
  cleanup
  killThread printThread

-- Ensure this gets inlined.
{-# INLINE hRunHsStatus #-}

setValue :: Vector ByteString -> (Int, ByteString) -> Vector ByteString
setValue vect (i,v) = V.modify (\vs -> MV.write vs i v) vect
