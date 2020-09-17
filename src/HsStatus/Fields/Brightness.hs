module HsStatus.Fields.Brightness
  ( BrightState (..)
  , brightnessMonitor
  --, brightnessMonitorFloating
  , BrightPaths (..)
  , sysBacklight
  ) where

import Control.Monad (liftM, liftM2)
import Data.ByteString (ByteString, hGetLine)
import Data.ByteString.Char8 (pack)
import Data.Functor ((<&>))
import System.INotify (EventVariety (Modify))
import System.IO (IOMode (ReadMode), openFile, withFile)
import System.IO.Error (IOError, tryIOError)

import HsStatus.FieldUtils (iNotifyWatcher)
import HsStatus.Types.Field (Field (..))
import HsStatus.Utils (hGetFirstLine, packExceptions, readIntEither)

newtype BrightPaths = BrightPaths (String, String)

sysBacklight :: String -> String -> String -> BrightPaths
sysBacklight name bright max =
  let dir = "/sys/class/backlight/" ++ name ++ "/"
  in BrightPaths (dir ++ bright, dir ++ max)

newtype BrightState a = BrightState a

-- | Field that monitors the brightness of a display.
brightnessMonitor :: BrightPaths -> IO (Field (BrightState Int))
brightnessMonitor (BrightPaths (bright, maxbright)) = do
  maxN <- tryIOError (withFile maxbright ReadMode hGetLine) <&> readIntEither <&> (\(Right x) -> x)
  nowH <- tryIOError (openFile bright ReadMode)

  let makePercent :: Int -> Int
      makePercent = (flip div) maxN . (*100)

      go :: a -> IO (BrightState Int)
      go _ = hGetFirstLine nowH <&> readIntEither <&> (\(Right x) -> x) <&> makePercent <&> BrightState

  return (iNotifyWatcher [([Modify], pack bright)] (BrightState 0) go)

{-
brightnessMonitorFloating :: BrightPaths -> Int -> IO (Field (BrightState Double))
brightnessMonitorFloating (BrightPaths (bright, maxbright)) digits = do
  maxN <- tryIOError (withFile maxbright ReadMode hGetLine) <&> readIntEither
  nowH <- tryIOError (openFile bright ReadMode)

  let makePercent :: Either IOError Int -> Either IOError Double
      makePercent = liftM (/ (10^digits)) . liftM fromIntegral . liftM2 (flip div) maxN . liftM (* (10^(digits + 2)))

      go :: a -> IO (Either ByteString (BrightState Double))
      go _ = hGetFirstLine nowH <&> readIntEither <&> makePercent <&> liftM BrightState <&> packExceptions

  return (iNotifyWatcher [([Modify], pack bright)] go)
  -}
