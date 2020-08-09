module HsStatus.Types.Sem
  ( Sem
  , newSem
  , waitFor
  , stopWaitingFor
  ) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)

type Sem = TMVar ()

newSem :: IO Sem
newSem = newEmptyTMVarIO

waitFor :: Sem -> IO ()
waitFor = atomically . takeTMVar

stopWaitingFor :: Sem -> IO ()
stopWaitingFor = atomically . flip putTMVar ()
