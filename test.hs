{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import HsStatus

import Control.Monad
import System.IO (stdin)

import qualified Streamly.Memory.Array as A

main = hRunHsStatus
  [ readHandle stdin
  , brightnessMonitor "intel_backlight" (A.fromList . show)
  , alsaMonitor "default" "Master" (\sw pc -> A.fromList . show $ (sw,pc))
  , batteryMonitor "BAT0" 5000000 (\st pc -> A.fromList. show $ (st,pc))
  , dateField 1000000 "%T"
  ]
