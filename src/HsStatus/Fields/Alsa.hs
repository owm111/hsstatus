module HsStatus.Fields.Alsa (alsaMonitor) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad

import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field

alsaMonitor :: String -> String -> Field (Bool, Int)
alsaMonitor mixer element = Field $ \printSem var -> do
  let tell x = atomically (writeTVar var x >> signalTSem printSem)
  tid <- forkIO $ bracket (openMixerElement mixer element)
                          (closeMixerElement)
                          (\mixelm -> forever (awaitNewStatus mixelm >>= tell))
  return [tid]
