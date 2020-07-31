module HsStatus.Fields.Brightness
  ( BrightState (..)
  , brightnessMonitor
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import System.INotify (EventVariety (..))
import System.IO

import HsStatus.Types
import HsStatus.FieldUtils
import HsStatus.Utils

newtype BrightState = BrightState Double deriving Show

-- | Field that monitors the brightness of a display.
-- 
-- TODO: pass paths as a parameters.
-- TODO: simple integer percent variant.
-- TOOD: handle exceptions.
brightnessMonitor :: Int -> FormatterFor BrightState -> IO Field
brightnessMonitor digits format = do
  max <- withFile maxbright ReadMode BS.hGetLine
  brightH <- openFile bright ReadMode
  let getPerc x = readPercentTruncatedTo digits x max
      go _ = do hSeek brightH AbsoluteSeek 0
                format <$> BrightState <$> getPerc <$> BS.hGetLine brightH
  return $ iNotifyWatcher [Modify] brightPath go
  where dir = "/sys/class/backlight/intel_backlight/"
        bright = dir ++ "brightness"
        maxbright = dir ++ "max_brightness"
        brightPath = pack bright
