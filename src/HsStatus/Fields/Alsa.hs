module HsStatus.Fields.Alsa (alsaMonitor) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad

import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field

alsaMonitor :: String -> String -> Field (Bool, Int)
alsaMonitor mixer element =
  let rethrowAndClose me (Left e)  = closeMixerElement me >> throwIO e
      rethrowAndClose me (Right _) = closeMixerElement me

      go printSem var = do
        mixerelem <- openMixerElement mixer element
        tid <- (`forkFinally` rethrowAndClose mixerelem) . forever $ do
          stat <- awaitNewStatus mixerelem
          atomically (writeTVar var stat >> signalTSem printSem)
        return [tid]
   in Field ((False, 0), go)
