module HsStatus.Fields.Alsa (alsaMonitor) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)

import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field

alsaMonitor :: String -> String -> (Bool -> Int -> ByteString) -> Field
alsaMonitor mixer element format = Field $ \idx chan -> do
  writeChan chan (idx, format False 0)
  bracket (openMixerElement mixer element)
          (closeMixerElement)
          (\mixelm -> forever (awaitNewStatus mixelm >>= writeChan chan . (,) idx . uncurry format))

{-# INLINE alsaMonitor #-}
