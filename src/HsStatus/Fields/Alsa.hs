module HsStatus.Fields.Alsa (alsaMonitor) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field

alsaMonitor :: String -> String -> Field (Bool, Int)
alsaMonitor mixer element = Field $ \printSem _ var -> do
  let tell x = writeIORef var x >> void (tryPutMVar printSem ())
  bracket (openMixerElement mixer element)
          (closeMixerElement)
          (\mixelm -> forever (awaitNewStatus mixelm >>= tell))
