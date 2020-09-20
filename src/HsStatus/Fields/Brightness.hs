module HsStatus.Fields.Brightness
  ( brightnessMonitor
  ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.ByteString.Char8 (pack)
import System.INotify
import System.IO

import HsStatus.Types.Field

brightnessMonitor :: String -> Field Int
brightnessMonitor name = Field $ \printSem var -> do
  let brightness = "/sys/class/backlight/" ++ name ++ "/brightness"
      max_brightness = "/sys/class/backlight/" ++ name ++ "/max_brightness"
  locked <- newEmptyMVar
  max <- withFile max_brightness ReadMode hGetLine >>= readIO
  briH <- openFile brightness ReadMode
  let toPerc bri = bri * 100 `div` max
      update = do
        didLock <- tryPutMVar locked ()
        when didLock $ do
          bri <- hGetLine briH >>= readIO
          hSeek briH AbsoluteSeek 0
          tryTakeMVar locked
          atomically (writeTVar var (toPerc bri) >> signalTSem printSem)
  update
  inot <- initINotify
  addWatch inot [CloseWrite] (pack brightness) (const update)
  return []
