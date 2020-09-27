module HsStatus.Main
  ( hRunHsStatus
  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
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
  printSem <- newEmptyMVar
  doneVar <- newEmptyMVar
  (vars, threads) <- startFields printSem doneVar fields
  printThread <- forkIO (forever (takeMVar printSem >> readVars vars >>= format))
  let cleanup = mapM_ killThread (printThread:threads)
      setDone = putMVar doneVar ()
  installHandler keyboardSignal (Catch setDone) Nothing
  installHandler softwareTermination (Catch setDone) Nothing
  takeMVar doneVar
  cleanup
