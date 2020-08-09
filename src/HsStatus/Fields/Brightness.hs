module HsStatus.Fields.Brightness
  ( BrightState (..)
  , brightnessMonitor
  , BrightPaths (..)
  , sysBacklight
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, readInt)
import Data.Maybe (fromJust)
import Data.Functor
import System.INotify (EventVariety (..))
import System.IO.Error

import HsStatus.FieldUtils
import HsStatus.IO
import HsStatus.Types.Field (Field (..))
import HsStatus.Utils

newtype BrightPaths = BrightPaths (String, String)

sysBacklight :: String -> String -> String -> BrightPaths
sysBacklight name bright max =
  let dir = "/sys/class/backlight/" ++ name ++ "/"
  in BrightPaths (dir ++ bright, dir ++ max)

newtype BrightState a = BrightState a

-- | Field that monitors the brightness of a display.
brightnessMonitor :: BrightPaths -> IO (Field (BrightState Int))
brightnessMonitor (BrightPaths (bright, maxbright)) = do
  maxN <- tryIOError (withFile maxbright ReadMode hGetLine) <&> readIntEither
  nowH <- tryIOError (openFile bright ReadMode)

  let makePercent :: Either IOError Int -> Either IOError Int
      makePercent = liftM2 (flip div) maxN . liftM (*100)

      go :: a -> IO (Either ByteString (BrightState Int))
      go _ = hGetFirstLine nowH <&> readIntEither <&> makePercent <&> liftM BrightState <&> packExceptions

  return (iNotifyWatcher [([Modify], pack bright)] go)

brightnessMonitorFloating :: BrightPaths -> Int -> IO (Field (BrightState Double))
brightnessMonitorFloating (BrightPaths (bright, maxbright)) digits = do
  maxN <- tryIOError (withFile maxbright ReadMode hGetLine) <&> readIntEither
  nowH <- tryIOError (openFile bright ReadMode)

  let makePercent :: Either IOError Int -> Either IOError Double
      makePercent = liftM (/ (10^digits)) . liftM fromIntegral . liftM2 (flip div) maxN . liftM (* (10^(digits + 2)))

      go :: a -> IO (Either ByteString (BrightState Double))
      go _ = hGetFirstLine nowH <&> readIntEither <&> makePercent <&> liftM BrightState <&> packExceptions

  return (iNotifyWatcher [([Modify], pack bright)] go)
