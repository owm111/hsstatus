module HsStatus.Fields.Alsa (alsaMonitor) where

import HsStatus.Fields.AlsaInternal
import HsStatus.Types.Field

alsaMonitor :: String -> String -> Field MixerElement (Bool, Int) (Bool, Int)
alsaMonitor mixer element = Field
  { acquire = openMixerElement mixer element
  , release = closeMixerElement
  , collect = const awaitNewStatus
  , process = id
  }
