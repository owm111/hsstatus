module HsStatus.Fields.Brightness
  ( BrightState (..)
  , brightnessMonitor
  ) where

import Data.ByteString.Char8 (pack, readInt)
import Data.Maybe (fromJust)
import System.INotify (EventVariety (..))

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Utils

newtype BrightState a = BrightState a deriving Show

-- | Field that monitors the brightness of a display.
-- 
-- TODO: pass paths as a parameters.
-- TOOD: handle exceptions.
brightnessMonitor :: FormatterFor (BrightState Int) -> IO Field
brightnessMonitor format = do
  max <- fst <$> fromJust <$> readInt <$> withFile maxbright ReadMode hGetLine
  brightH <- openFile bright ReadMode
  let getPerc x = ((fst $ fromJust $ readInt x) * 100) `div` max
      go _ = do hSeek brightH AbsoluteSeek 0
                format <$> BrightState <$> getPerc <$> hGetLine brightH
  return $ iNotifyWatcher [Modify] brightPath go
  where dir = "/sys/class/backlight/intel_backlight/"
        bright = dir ++ "brightness"
        maxbright = dir ++ "max_brightness"
        brightPath = pack bright

brightnessMonitorFloating :: Int -> FormatterFor (BrightState Double) -> IO Field
brightnessMonitorFloating digits format = do
  max <- withFile maxbright ReadMode hGetLine
  brightH <- openFile bright ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go _ = do hSeek brightH AbsoluteSeek 0
                format <$> BrightState <$> getPerc <$> hGetLine brightH
  return $ iNotifyWatcher [Modify] brightPath go
  where dir = "/sys/class/backlight/intel_backlight/"
        bright = dir ++ "brightness"
        maxbright = dir ++ "max_brightness"
        brightPath = pack bright
