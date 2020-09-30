module HsStatus.Fields.Alsa (alsaMonitor) where

import Data.Functor
import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field
import Streamly
import Streamly.Memory.Array (Array)

import qualified Streamly.Prelude as S

alsaMonitor :: String -> String -> (Bool -> Int -> Array Char) -> Field
alsaMonitor mixer element format =
  S.bracket (openMixerElement mixer element) closeMixerElement $ \mixelm ->
    S.repeatM (awaitNewStatus mixelm) <&> uncurry format

{-# INLINE alsaMonitor #-}
