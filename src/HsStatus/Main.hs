module HsStatus.Main
  ( hRunHsStatus
  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem, waitTSem)
import Control.Monad (forever)
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal, softwareTermination)

import HsStatus.Types.FieldTuple (FieldTuple (..))

-- | Initalizes the given fields and prints any changes to the given handle
-- according to the given formatter.
--
-- TODO: exception handling?
-- TODO: exit better.
hRunHsStatus :: FieldTuple t => (StateTuple t -> IO ()) -> t -> IO ()
hRunHsStatus format fields = do
  printSem <- atomically (newTSem 0)
  doneVar <- newEmptyMVar
  (vars, threads) <- startFields printSem doneVar fields
  printThread <- forkIO (forever (atomically (waitTSem printSem >> readVars vars) >>= format))
  let cleanup = mapM_ killThread (printThread:threads) >> putMVar doneVar ()
  installHandler keyboardSignal (Catch cleanup) Nothing
  installHandler softwareTermination (Catch cleanup) Nothing
  takeMVar doneVar
