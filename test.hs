{-# LANGUAGE OverloadedStrings #-}

import HsStatus

import Control.Monad
import Data.ByteString.Char8 (pack)
import System.IO (stdin)

main = hRunHsStatus
  ( readHandle stdin
  , brightnessMonitor "intel_backlight" (pack . show)
  , alsaMonitor "default" "Master" (\sw pc -> pack . show $ (sw,pc))
  , batteryMonitor "BAT0" 5000000 (\st pc -> pack. show $ (st,pc))
  , dateField 1000000 "%T"
  )
