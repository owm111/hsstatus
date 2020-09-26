module HsStatus.Types.Field
  ( Field (..)
  , startField
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad

import HsStatus.Types.FieldValue

data Field acq raw out = Field
  { acquire :: IO acq
  , release :: acq -> IO ()
  , collect :: MVar () -> acq -> IO raw
  , process :: raw -> out
  }

startField :: FieldValue out => TSem -> MVar () -> Field acq raw out -> IO (TVar out, ThreadId)
startField psem qsem f = do
  let cleanup (Right _) = putMVar qsem ()
      cleanup (Left  e) = throw e >> putMVar qsem ()
  var <- newTVarIO initialValue
  let go = bracket (acquire f) (release f) loop
      loop x = forever $ do
        out <- process f <$> collect f qsem x
        atomically (writeTVar var out >> signalTSem psem)
  tid <- forkFinally go cleanup
  pure (var, tid)
