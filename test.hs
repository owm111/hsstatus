{-# LANGUAGE OverloadedStrings #-}

import HsStatus

import Control.Monad
import System.IO (stdin)

main = hRunHsStatus print
  ( readHandle stdin
  , brightnessMonitor "intel_backlight"
  , alsaMonitor "default" "Master"
  , batteryMonitor "BAT0" 5000000
  , dateField 1000000 "%T"
  )
