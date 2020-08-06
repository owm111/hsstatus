module HsStatus.Fields.Brightness
  ( BrightState (..)
  , brightnessMonitor
  , BrightPaths (..)
  , sysBacklight
  ) where

import Data.ByteString.Char8 (pack, readInt)
import Data.Maybe (fromJust)
import System.INotify (EventVariety (..))

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Utils

newtype BrightPaths = BrightPaths (String, String)

sysBacklight :: String -> String -> String -> BrightPaths
sysBacklight name bright max =
  let dir = "/sys/class/backlight/" ++ name ++ "/"
  in BrightPaths (dir ++ bright, dir ++ max)

newtype BrightState a = BrightState a

-- | Field that monitors the brightness of a display.
-- 
-- TOOD: handle exceptions.
brightnessMonitor :: BrightPaths -> IO (Field (BrightState Int))
brightnessMonitor (BrightPaths (bright, maxbright)) = do
  max <- fst <$> fromJust <$> readInt <$> withFile maxbright ReadMode hGetLine
  brightH <- openFile bright ReadMode
  let getPerc x = ((fst $ fromJust $ readInt x) * 100) `div` max
      go _ = do hSeek brightH AbsoluteSeek 0
                Right <$> BrightState <$> getPerc <$> hGetLine brightH
  return $ iNotifyWatcher [Modify] brightPath go
  where brightPath = pack bright

brightnessMonitorFloating :: BrightPaths -> Int -> IO (Field (BrightState Double))
brightnessMonitorFloating (BrightPaths (bright, maxbright)) digits = do
  max <- withFile maxbright ReadMode hGetLine
  brightH <- openFile bright ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go _ = do hSeek brightH AbsoluteSeek 0
                Right <$> BrightState <$> getPerc <$> hGetLine brightH
  return $ iNotifyWatcher [Modify] brightPath go
  where brightPath = pack bright
